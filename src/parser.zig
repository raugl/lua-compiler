//  Here is the complete syntax of Lua in extended BNF. As usual in extended
//  BNF, {A} means 0 or more As, and [A] means an optional A. (For operator
//  precedences, see §3.4.8; for a description of the terminals Name, Numeral,
//  and LiteralString, see §3.1.)
//
// chunk ::= block
//
// block ::= {stat} [’return’ [explist] [‘;’]]
//
// stat ::=  ‘;’ |
//         var {‘,’ var} ‘=’ explist |
//         functioncall |
//         ‘::’ Name ‘::’ |
//         ‘break’ |
//         ‘goto’ Name |
//         ‘do’ block ‘end’ |
//         ‘while’ exp ‘do’ block ‘end’ |
//         ‘repeat’ block ‘until’ exp |
//         ‘if’ exp ‘then’ block {’elseif’ exp ‘then’ block} [’else’ block] ‘end’ |
//         ‘for’ Name ‘=’ exp ‘,’ exp [‘,’ exp] ‘do’ block ‘end’ |
//         ‘for’ Name {‘,’ Name} in explist ‘do’ block ‘end’ |
//         ‘function’ Name {‘.’ Name} [‘:’ Name] funcbody |
//         ‘local’ ‘function’ Name funcbody |
//         ‘local’ Name attrib {‘,’ Name attrib} [‘=’ explist]
//
// attrib ::= [‘<’ Name ‘>’]
//
// var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
//
// explist ::= exp {‘,’ exp}
//
// exp ::=  ‘nil’ | ‘false’ | ‘true’ | Numeral | LiteralString | ‘...’ | ‘function’ funcbody |
//         prefixexp | tableconstructor | exp binop exp | unop exp
//
// prefixexp ::= var | functioncall | ‘(’ exp ‘)’
//
// functioncall ::=  prefixexp [‘:’ Name] arglist
//
// arglist ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString
//
// funcbody ::= ‘(’ [parlist] ‘)’ block end
//
// parlist ::= Name {‘,’ Name} [‘,’ ‘...’] | ‘...’
//
// tableconstructor ::= ‘{’ [field {fieldsep field} [fieldsep]] ‘}’
//
// field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp
//
// fieldsep ::= ‘,’ | ‘;’
//
// binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ |
//         ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ |
//         ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ |
//         ‘and’ | ‘or’
//
// unop ::= ‘-’ | ‘not’ | ‘#’ | ‘~’

const std = @import("std");
const ast = @import("ast.zig");
const lx = @import("lexer.zig");

const meta = std.meta;
const mem = std.mem;

pub const ParseResult = struct {
    nodes: []const ast.Node,
    extra: []const u32,
    // messages: []Message, // TODO

    pub fn deinit(self: ParseResult, alloc: mem.Allocator) void {
        alloc.free(self.nodes);
        alloc.free(self.extra);
    }

    pub fn get(self: ParseResult, id: ast.NodeID) ast.Key {
        return ast.decodeKey(self.nodes, self.extra, id);
    }

    // TODO: Fuzz testing: rebuild the source code from an ast tree and compare it to the striped input
    // TODO: Change traversal to iterative implementation
    pub fn print(
        self: ParseResult,
        writer: std.io.AnyWriter,
        node_id: ast.NodeID,
    ) !void {
        switch (self.get(node_id)) {
            .block => |node| {
                for (node.statements) |stat| {
                    try self.print(writer, stat);
                    try writer.writeByte('\n');
                }
                if (node.return_list.len > 0) {
                    try writer.writeAll("return ");
                    for (node.return_list) |val| {
                        try self.print(writer, val);
                        try writer.writeByte(',');
                    }
                }
            },
            .if_block => |node| {
                try writer.writeAll("if ");
                for (node.branches, 0..) |branch, i| {
                    try self.print(writer, branch.cond);
                    try writer.writeAll(" then\n");
                    try self.print(writer, branch.block);
                    if (i < node.branches.len - 1) {
                        try writer.writeAll("\nelseif ");
                    }
                }
                try writer.writeAll("\nend");
            },
            .unary_op => |node| {
                const lexeme = switch (node.op) {
                    .length => "#",
                    .negate => "-",
                    .bit_not => "~",
                    .logic_not => "not ",
                };
                try writer.writeAll(lexeme);
                try self.print(writer, node.exp);
            },
            .binary_op => |node| {
                const lexeme = switch (node.op) {
                    .logic_or => " or ",
                    .logic_and => " and ",
                    .less_than => " < ",
                    .greater_than => " > ",
                    .less_equal => " <= ",
                    .greater_equal => " >= ",
                    .not_equal => " ~= ",
                    .equal => " == ",
                    .bit_or => " | ",
                    .bit_and => " & ",
                    .l_bit_shift => " << ",
                    .r_bit_shift => " >> ",
                    .str_concat => " .. ",
                    .add => " + ",
                    .sub => " - ",
                    .mul => " * ",
                    .div => " /",
                    .int_div => " // ",
                    .modulo => " % ",
                    .exponent => " ^ ",
                    .dot_access => ".",
                    .index_access => "[",
                };
                try self.print(writer, node.lhs);
                try writer.writeAll(lexeme);
                try self.print(writer, node.rhs);
                if (node.op == .index_access) try writer.writeByte(']');
            },
            else => {
                try writer.writeAll("unknown");
            },
        }
    }
};

/// The caller owns the memory, and shoud free it with `res.deinit(alloc)`. To get
/// a node, use `res.get(node_id)`, and to get the root node use `res.get(.root)`.
pub fn parse(
    alloc: mem.Allocator,
    scratch: mem.Allocator,
    lexer: lx.Lexer,
) mem.Allocator.Error!ParseResult {
    var self = Parser.init(alloc, scratch, lexer);
    const head = try self.nodes.addOne(alloc);
    const chunk = try self.parseChunk();
    head.* = self.nodes.items[@intFromEnum(chunk)];

    const nodes = try self.nodes.toOwnedSlice(alloc);
    errdefer alloc.free(nodes);
    const extra = try self.extra.toOwnedSlice(alloc);
    errdefer alloc.free(extra);

    return ParseResult{ .nodes = nodes, .extra = extra };
}

/// The caller owns the memory inside `ParseResult`, and shoud free it with
/// `result.deinit(alloc)`. To get a node, use `result.get(node_id)`, and to get
/// the root node use `result.get(.root)`.
pub fn parseText(
    alloc: mem.Allocator,
    scratch: mem.Allocator,
    text: [:0]const u8,
) mem.Allocator.Error!ParseResult {
    const lexer = lx.Lexer.init(text);
    return parse(alloc, scratch, lexer);
}

const Message = struct {
    loc: lx.Location,
    text: []const u8,
    level: enum { note, warninig, @"error" },
};

const Parser = struct {
    lexer: lx.Lexer,
    curr_token: lx.Token,
    next_token: lx.Token,

    alloc: mem.Allocator,
    scratch: mem.Allocator,

    // PERF: the messages and nodes shouldn't be interleaved by using the same allocator
    extra: std.ArrayListUnmanaged(u32) = .empty,
    nodes: std.MultiArrayList(ast.Node) = .empty,
    messages: std.ArrayListUnmanaged(Message) = .empty,

    pub fn init(alloc: mem.Allocator, scratch: mem.Allocator, lexer: lx.Lexer) Parser {
        var self = Parser{
            .lexer = lexer,
            .alloc = alloc,
            .scratch = scratch,
            .curr_token = undefined,
            .next_token = undefined,
        };
        self.getTokenUnchecked();
        self.getTokenUnchecked();
        return self;
    }

    fn getTokenUnchecked(self: *Parser) void {
        self.curr_token = self.next_token;
        while (true) {
            self.next_token = self.lexer.next();
            if (self.next_token.tag != .comment) break;
        }
    }

    fn getToken(self: *Parser) void {
        self.getTokenUnchecked();
        while (self.curr_token.tag == .invalid) {
            self.err("invalid token", .{});
            self.getTokenUnchecked();
        }
    }

    // FIXME: this way char token won't be wrapped in quotes
    fn getSource(self: Parser, token: lx.Token) []const u8 {
        return self.lexer.buffer[token.loc.start..token.loc.end];
    }

    fn makeNode(
        self: *Parser,
        comptime tag: ast.KeyTag,
        data: meta.TagPayload(ast.Key, tag),
    ) mem.Allocator.Error!ast.NodeID {
        const key = @unionInit(ast.Key, @tagName(tag), data);
        return ast.encodeKey(self.alloc, &self.nodes, &self.extra, key);
    }

    // TODO:
    fn err(self: *Parser, comptime fmt: []const u8, args: anytype) void {
        _ = self;
        std.log.err(fmt, args);
    }

    fn match(self: *Parser, tag: lx.Token.Tag) bool {
        if (self.curr_token.tag == tag) {
            self.getToken();
            return true;
        }
        return false;
    }

    fn expect(self: *Parser, tag: lx.Token.Tag) void {
        if (self.next_token.tag == tag) {
            self.getToken();
        } else {
            // FIXME: this way char token won't be wrapped in quotes
            self.err("expected {s}, found {s}", .{
                tag.lexeme(),
                self.getSource(self.curr_token),
            });
        }
    }

    pub const parseChunk = expectBlock;

    fn expectBlock(self: *Parser) mem.Allocator.Error!ast.NodeID {
        var stats = std.ArrayList(ast.NodeID).init(self.scratch);
        defer stats.deinit();

        while (try self.parseStatement()) |stat| {
            try stats.append(stat);
        }

        var return_list: []const ast.NodeID = &.{};
        if (self.match(.keyword_return)) {
            return_list = try self.parseExpList() orelse &.{};
            _ = self.match(.@";");
        }
        defer self.scratch.free(return_list);

        return self.makeNode(.block, .{
            .statements = stats.items,
            .return_list = return_list,
        });
    }

    fn parseParamList(self: *Parser) mem.Allocator.Error!struct { []const ast.Slice, bool } {
        var params = std.ArrayList(ast.Slice).init(self.scratch);
        errdefer params.deinit();
        var variadic = false;

        self.expect(.@"(");
        while (true) {
            if (self.match(.@"...")) {
                variadic = true;
                break;
            }
            const name = self.expectName();
            try params.append(name);
            if (!self.match(.@",")) break;
        }
        self.expect(.@")");

        return .{ try params.toOwnedSlice(), variadic };
    }

    // TODO: fix token advancing in parsing
    fn parseExp(self: *Parser, prec: lx.Precedence) mem.Allocator.Error!?ast.NodeID {
        if (try self.parseUnaryExp()) |lhs| {
            var node = lhs;
            const next_prec = self.next_token.precedence(false);

            while (@intFromEnum(prec) < @intFromEnum(next_prec)) {
                node = try self.parseBinaryExp(node);
                self.getToken();
            }
            return node;
        }
        return null;
    }

    fn expectExp(self: *Parser, prec: lx.Precedence) mem.Allocator.Error!ast.NodeID {
        return try self.parseExp(prec) orelse {
            const found = self.getSource(self.curr_token);
            self.err("expected expression, found {s}", .{found});
            return .none;
        };
    }

    // TODO: Add table constructor
    fn parseUnaryExp(self: *Parser) mem.Allocator.Error!?ast.NodeID {
        switch (self.curr_token.tag) {
            .identifier => {
                return try self.makeNode(.identifier, self.parseName().?);
            },
            .@"(" => {
                self.getToken();
                const node = try self.expectExp(.any);
                self.expect(.@")");
                return node;
            },
            .keyword_not, .@"~", .@"-", .@"#" => {
                const op: ast.UnaryOp = switch (self.curr_token.tag) {
                    .@"#" => .length,
                    .@"-" => .negate,
                    .@"~" => .bit_not,
                    .keyword_not => .logic_not,
                    else => unreachable,
                };
                const prec = self.curr_token.precedence(true);
                self.getToken();

                return try self.makeNode(.unary_op, .{
                    .op = op,
                    .exp = try self.expectExp(prec),
                });
            },
            .keyword_nil => {
                self.getToken();
                return try self.makeNode(.literal_nil, {});
            },
            .keyword_true => {
                self.getToken();
                return try self.makeNode(.literal_true, {});
            },
            .keyword_false => {
                self.getToken();
                return try self.makeNode(.literal_false, {});
            },
            .@"..." => {
                self.getToken();
                return try self.makeNode(.literal_ellipsis, {});
            },
            .literal_number => {
                const lexeme = self.getSource(self.curr_token);
                // TODO: Distinguish between floats and ints
                const value = std.fmt.parseInt(i64, lexeme, 10) catch blk: {
                    self.err("could not parse {s} as integer", .{lexeme});
                    break :blk 0;
                };
                self.getToken();
                return try self.makeNode(.literal_int, value);
            },
            .literal_string => {
                const loc = self.curr_token.loc;
                const str = ast.Slice{
                    .start = loc.start,
                    .len = loc.end - loc.start,
                };
                self.getToken();
                return try self.makeNode(.literal_string, str);
            },
            .keyword_function => {
                const params, const variadic = try self.parseParamList();
                defer self.scratch.free(params);

                const block = try self.expectBlock();
                self.expect(.keyword_end);

                return try self.makeNode(.func_anonym, .{
                    .params = params,
                    .block = block,
                    .variadic = variadic,
                });
            },
            else => return null,
        }
    }

    fn parseBinaryExp(self: *Parser, lhs: ast.NodeID) mem.Allocator.Error!ast.NodeID {
        const op: ast.BinaryOp = switch (self.curr_token.tag) {
            .keyword_or => .logic_or,
            .keyword_and => .logic_and,
            .@"=" => .equal,
            .@"~=" => .not_equal,
            .@"<" => .less_than,
            .@">" => .greater_than,
            .@"<=" => .less_equal,
            .@">=" => .greater_equal,
            .@"|" => .bit_or,
            .@"&" => .bit_and,
            .@"<<" => .l_bit_shift,
            .@">>" => .r_bit_shift,
            .@".." => .str_concat,
            .@"+" => .add,
            .@"-" => .sub,
            .@"*" => .mul,
            .@"/" => .div,
            .@"//" => .int_div,
            .@"%" => .modulo,
            .@"^" => .exponent,
            .@"." => .dot_access,
            .@"[" => .index_access,
            .@":", .@"(", .@"{", .literal_string => return self.parseFuncCall(lhs),
            else => unreachable,
        };
        const prec = self.curr_token.precedence(true);
        self.getToken();

        const rhs = try self.expectExp(prec);
        if (op == .index_access) {
            self.expect(.@"]");
        }
        return self.makeNode(.binary_op, .{ .op = op, .lhs = lhs, .rhs = rhs });
    }

    fn parseFuncCall(self: *Parser, lhs: ast.NodeID) mem.Allocator.Error!ast.NodeID {
        var member: ?ast.Slice = null;
        if (self.match(.@":")) {
            member = self.expectName();
        }

        if (try self.parseTableConstructor()) |table| {
            return self.makeNode(.func_call, .{
                .object = lhs,
                .member = member,
                .args = &.{table},
            });
        }

        if (self.match(.literal_string)) {
            const loc = self.curr_token.loc;
            const str = ast.Slice{
                .start = loc.start,
                .len = loc.end - loc.start,
            };
            self.getToken();
            const node = try self.makeNode(.literal_string, str);

            return self.makeNode(.func_call, .{
                .object = lhs,
                .member = member,
                .args = &.{node},
            });
        }

        self.expect(.@"(");
        const args = try self.parseExpList() orelse &.{};
        defer self.scratch.free(args);
        self.expect(.@")");

        return self.makeNode(.func_call, .{
            .object = lhs,
            .member = member,
            .args = args,
        });
    }

    fn parseExpList(self: *Parser) mem.Allocator.Error!?[]const ast.NodeID {
        if (try self.parseExp(.any)) |exp_| {
            var args = std.ArrayList(ast.NodeID).init(self.scratch);
            errdefer args.deinit();
            try args.append(exp_);

            while (self.match(.@",")) {
                const exp = try self.expectExp(.any);
                try args.append(exp);
            }
            return try args.toOwnedSlice();
        }
        return null;
    }

    fn expectExpList(self: *Parser) mem.Allocator.Error![]const ast.NodeID {
        return try self.parseExpList() orelse {
            self.err("expected <exp>, found {s}", .{self.getSource(self.curr_token)});
            return &.{};
        };
    }

    fn parseTableConstructor(self: *Parser) mem.Allocator.Error!?ast.NodeID {
        // tableconstructor ::= ‘{’ [field {fieldsep field} [fieldsep]] ‘}’
        // fieldsep ::= ‘,’ | ‘;’

        if (self.match(.@"{")) {
            var fields = std.ArrayList(ast.Field).init(self.scratch);
            defer fields.deinit();

            if (try self.parseField()) |field_| {
                try fields.append(field_);

                while (self.match(.@",") or self.match(.@";")) {
                    const field = try self.parseField() orelse break;
                    try fields.append(field);
                }
            }
            self.expect(.@"}");
            return try self.makeNode(.table_constructor, .{ .fields = fields.items });
        }
        return null;
    }

    fn parseField(self: *Parser) mem.Allocator.Error!?ast.Field {
        // field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp

        if (self.match(.@"[")) {
            const key = try self.expectExp(.any);
            self.expect(.@"]");
            self.expect(.@"=");
            return ast.Field{
                .key = key,
                .value = try self.expectExp(.any),
            };
        }
        if (self.match(.identifier)) {
            const name = self.expectName();
            const key = try self.makeNode(.identifier, name);
            self.expect(.@"=");
            return ast.Field{
                .key = key,
                .value = try self.expectExp(.any),
            };
        }
        if (try self.parseExp(.any)) |exp| {
            return ast.Field{ .value = exp };
        }
        return null;
    }

    // TODO: Verify usage
    fn parseName(self: *Parser) ?ast.Slice {
        if (self.match(.identifier)) {
            const loc = self.curr_token.loc;
            return ast.Slice{
                .start = loc.start,
                .len = loc.end - loc.start,
            };
        }
        return null;
    }

    // TODO: Verify usage
    fn expectName(self: *Parser) ast.Slice {
        return self.parseName() orelse {
            self.err("expected <name>, found {s}", .{self.getSource(self.curr_token)});
            return .empty;
        };
    }

    // FIXME: What happens in an expect fails???
    fn parseStatement(self: *Parser) mem.Allocator.Error!?ast.NodeID {
        switch (self.curr_token.tag) {
            // TODO: var {‘,’ var} ‘=’ explist
            .@";" => {
                self.getToken();
                return self.parseStatement();
            },
            .@"::" => {
                self.getToken();
                const name = self.expectName();
                self.expect(.@"::");
                return try self.makeNode(.label, name);
            },
            .keyword_break => {
                self.getToken();
                return try self.makeNode(.@"break", {});
            },
            .keyword_goto => {
                self.getToken();
                const name = self.expectName();
                return try self.makeNode(.goto, name);
            },
            .keyword_do => {
                self.getToken();
                const block = try self.expectBlock();
                self.expect(.keyword_end);
                return block;
            },
            .keyword_while => {
                self.getToken();
                const cond = try self.expectExp(.any);
                self.expect(.keyword_do);
                const block = try self.expectBlock();
                self.expect(.keyword_end);
                return try self.makeNode(.while_loop, .{
                    .cond = cond,
                    .block = block,
                });
            },
            .keyword_repeat => {
                self.getToken();
                const block = try self.expectBlock();
                self.expect(.keyword_until);
                const cond = try self.expectExp(.any);
                self.expect(.keyword_end);
                return try self.makeNode(.repeat_loop, .{
                    .cond = cond,
                    .block = block,
                });
            },
            .keyword_if => {
                var branches = std.ArrayList(ast.Conditional).init(self.scratch);
                defer branches.deinit();
                self.getToken();

                while (true) {
                    const branch = try branches.addOne();
                    branch.cond = try self.expectExp(.any);
                    self.expect(.keyword_then);
                    branch.block = try self.expectBlock();

                    if (!self.match(.keyword_elseif)) break;
                }
                var else_block: ?ast.NodeID = null;
                if (self.match(.keyword_else)) {
                    else_block = try self.expectBlock();
                }
                self.expect(.keyword_end);
                return try self.makeNode(.if_block, .{
                    .branches = branches.items,
                    .else_block = else_block,
                });
            },
            .keyword_for => {
                self.getToken();
                const name = self.expectName();

                if (self.match(.@"=")) {
                    const start = try self.expectExp(.any);
                    self.expect(.@",");
                    const end = try self.expectExp(.any);

                    var step: ?ast.NodeID = null;
                    if (self.match(.@",")) {
                        step = try self.expectExp(.any);
                    }
                    self.expect(.keyword_do);
                    const block = try self.expectBlock();
                    self.expect(.keyword_end);
                    return try self.makeNode(.for_loop, .{
                        .name = name,
                        .start = start,
                        .step = step,
                        .end = end,
                        .block = block,
                    });
                } else {
                    var names = std.ArrayList(ast.Slice).init(self.scratch);
                    defer names.deinit();

                    try names.append(name);
                    while (self.match(.@",")) {
                        const other_name = self.expectName();
                        try names.append(other_name);
                    }
                    self.expect(.keyword_in);
                    const values = try self.expectExpList();
                    defer self.scratch.free(values);

                    self.expect(.keyword_do);
                    const block = try self.expectBlock();
                    self.expect(.keyword_end);

                    return try self.makeNode(.for_each, .{
                        .names = names.items,
                        .values = values,
                        .block = block,
                    });
                }
            },
            .keyword_function => {
                var names = std.ArrayList(ast.Slice).init(self.scratch);
                defer names.deinit();

                self.getToken();
                while (true) {
                    const name = self.expectName();
                    try names.append(name);
                    if (!self.match(.@".")) break;
                }
                var member: ?ast.Slice = null;
                if (self.match(.@":")) {
                    member = self.expectName();
                }
                const params, const variadic = try self.parseParamList();
                defer self.scratch.free(params);
                const block = try self.expectBlock();
                self.expect(.keyword_end);

                return try self.makeNode(.func_decl, .{
                    .names = names.items,
                    .params = params,
                    .block = block,
                    .member = member,
                    .variadic = variadic,
                });
            },
            .keyword_local => {
                self.getToken();
                if (self.match(.keyword_function)) {
                    const name = self.expectName();

                    const params, const variadic = try self.parseParamList();
                    defer self.scratch.free(params);
                    const block = try self.expectBlock();
                    self.expect(.keyword_end);

                    return try self.makeNode(.func_local, .{
                        .name = name,
                        .params = params,
                        .block = block,
                        .variadic = variadic,
                    });
                } else {
                    var names = std.ArrayList(ast.Slice).init(self.scratch);
                    defer names.deinit();

                    while (true) {
                        try names.append(self.expectName());
                        _ = self.parseAttrib() orelse {}; // NOTE: Discarding attributes
                        if (!self.match(.@",")) break;
                    }
                    if (self.match(.@"=")) {
                        const values = try self.expectExpList();
                        defer self.scratch.free(values);
                        return try self.makeNode(.local_decl, .{
                            .names = names.items,
                            .values = values,
                        });
                    }
                    return try self.makeNode(.local_decl, .{
                        .names = names.items,
                        .values = &.{},
                    });
                }
            },
            else => return null,
        }
    }

    fn parseAttrib(self: *Parser) ?ast.Slice {
        if (self.match(.@"<")) {
            const attrib = self.expectName();
            self.expect(.@">");
            return attrib;
        }
        return null;
    }

    // FIXME: What happens in an expect fails???
    // fn matchStatement(self: *Parser) bool {
    //     if (self.match(.@";") or self.matchFuncCall() or self.match(.keyword_break)) {
    //         return true;
    //     }
    //     if (self.matchVar()) {
    //         while (self.match(.@",")) {
    //             self.expect(self.matchVar(), "<name>");
    //         }
    //         self.expect(.@"=");
    //         self.expect(self.matchExpList(), "expression");
    //         return true;
    //     }
    //     if (self.match(.@"::")) {
    //         self.expect(.identifier);
    //         self.expect(.@"::");
    //         return true;
    //     }
    //     if (self.match(.keyword_goto)) {
    //         self.expect(.identifier);
    //         return true;
    //     }
    //     if (self.match(.keyword_do)) {
    //         self.expect(self.matchBlock(), "block");
    //         self.expect(.keyword_end);
    //         return true;
    //     }
    //     if (self.match(.keyword_while)) {
    //         self.expect(self.matchExpression(), "expression");
    //         self.expect(.keyword_do);
    //         self.expect(self.matchBlock(), "block");
    //         self.expect(.keyword_end);
    //         return true;
    //     }
    //     if (self.match(.keyword_repeat)) {
    //         self.expect(self.matchBlock(), "block");
    //         self.expect(.keyword_until);
    //         self.expect(self.matchExpression(), "expression");
    //         return true;
    //     }
    //
    //     if (self.match(.keyword_for)) {
    //         self.expect(.identifier);
    //         if (self.match(.@"=")) {
    //             self.expect(self.matchExpression(), "expression");
    //             self.expect(.@",");
    //             self.expect(self.matchExpression(), "expression");
    //
    //             while (self.match(.@",")) {
    //                 self.expect(self.matchExpression(), "expression");
    //             }
    //             self.expect(.keyword_do);
    //             self.expect(self.matchBlock(), "block");
    //             self.expect(.keyword_end);
    //         } else {
    //             while (self.match(.@",")) {
    //                 self.expect(.identifier);
    //             }
    //             self.expect(.keyword_in);
    //             self.expect(self.matchExpList(), "expression");
    //             self.expect(.keyword_do);
    //             self.expect(self.matchBlock(), "block");
    //             self.expect(.keyword_end);
    //         }
    //         return true;
    //     }
    //     if (self.match(.keyword_function)) {
    //         self.expect(.identifier);
    //         while (self.match(.@".")) {
    //             self.expect(.identifier);
    //         }
    //         if (self.match(.@":")) {
    //             self.expect(.identifier);
    //         }
    //         self.expect(self.matchFuncBody(), "function body");
    //         return true;
    //     }
    //     if (self.match(.keyword_local)) {
    //         if (self.expect(.keyword_function)) {
    //             self.expect(.identifier);
    //             self.expect(self.matchFuncBody(), "function body");
    //         } else {
    //             self.expect(.identifier);
    //             _ = self.matchAttribute();
    //             while (self.match(.@",")) {
    //                 self.expect(.identifier);
    //                 _ = self.matchAttribute();
    //             }
    //             if (self.match(.@"=")) {
    //                 self.expect(self.matchExpList(), "expression");
    //             }
    //         }
    //         return true;
    //     }
    //     return false;
    // }
    //
    // fn matchVar(self: *Parser) bool {
    //     if (self.match(.identifier)) {
    //         return true;
    //     }
    //     if (self.matchPrefixExp()) {
    //         self.expect(.@"[");
    //         self.expect(self.matchExpression(), "expression");
    //         self.expect(.@"]");
    //         return true;
    //     }
    //     if (self.matchPrefixExp()) {
    //         self.expect(.@".");
    //         self.expect(.identifier);
    //     }
    //     return true;
    // }
};

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
