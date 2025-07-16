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
    nodes: []const ast.NodeID,
    extra: []const u32,
    // messages: []Message, // TODO

    pub fn deinit(self: ParseResult, alloc: mem.Allocator) void {
        alloc.free(self.nodes);
        alloc.free(self.extra);
    }

    pub fn get(self: ParseResult, id: ast.NodeID) ast.Key {
        return ast.decodeKey(id, self.nodes, self.extra);
    }
};

/// The caller owns the memory, and shoud free it with `res.deinit(alloc)`. To get
/// a node, use `res.get(node_id)`, and to get the root node use `res.get(.root)`.
pub fn parse(alloc: mem.Allocator, scratch: mem.Allocator, lexer: lx.Lexer) !ParseResult {
    var self = Parser.init(alloc, scratch, lexer);
    const head = try self.nodes.addOne(alloc);
    const chunk = try self.parseChunk();
    head.* = self.nodes.items[@intFromEnum(chunk)];

    const nodes = try self.nodes.toOwnedSlice();
    errdefer alloc.free(nodes);
    const extra = try self.extra.toOwnedSlice();
    errdefer alloc.free(extra);

    return ParseResult{ .nodes = nodes, .extra = extra };
}

/// The caller owns the memory inside `ParseResult`, and shoud free it with
/// `result.deinit(alloc)`. To get a node, use `result.get(node_id)`, and to get
/// the root node use `result.get(.root)`.
pub fn parseText(alloc: mem.Allocator, scratch: mem.Allocator, text: [:0]const u8) !ParseResult {
    const lexer = lx.Lexer.init(text);
    return parse(alloc, scratch, lexer);
}

const Message = struct {
    loc: lx.Token.Loc,
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
    nodes: std.ArrayListUnmanaged(ast.Node) = .empty,
    messages: std.ArrayListUnmanaged(Message) = .empty,

    pub fn init(alloc: mem.Allocator, scratch: mem.Allocator, lexer: lx.Lexer) Parser {
        var self = Parser{
            .lexer = lexer,
            .alloc = alloc,
            .scratch = scratch,
            .curr_token = undefined,
            .next_token = undefined,
        };
        self.getToken();
        self.getToken();
        return self;
    }

    fn getToken(self: *Parser) void {
        self.curr_token = self.next_token;
        self.next_token = self.lexer.next();
        while (self.next_token.tag == .comment) {
            self.next_token = self.lexer.next();
        }
    }

    // FIXME: this way char token won't be wrapped in quotes
    fn getLexeme(self: Parser, token: lx.Token) []const u8 {
        return self.lexer.buffer[token.loc.start..token.loc.end];
    }

    fn makeNode(
        self: *Parser,
        comptime tag: meta.Tag(ast.Key),
        data: meta.TagPayload(ast.Key, tag),
    ) mem.Allocator.Error!ast.NodeID {
        const key = @unionInit(ast.Key, @tagName(tag), data);
        return ast.encodeKey(self.alloc, key, &self.nodes, &self.extra);
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
                self.getLexeme(self.curr_token),
            });
        }
    }

    pub const parseChunk = parseBlock;

    fn parseBlock(self: *Parser) !ast.NodeID {
        var stats = std.ArrayList(ast.NodeID).init(self.scratch);
        defer stats.deinit();
        var ret_list = std.ArrayList(ast.NodeID).init(self.scratch);
        defer ret_list.deinit();

        while (try self.parseStatement()) |stat| {
            try stats.append(stat);
        }
        if (self.match(.keyword_return)) {
            if (try self.parseExp(0)) |exp| {
                try ret_list.append(exp);

                while (self.match(.@",")) {
                    try ret_list.append(try self.expectExp(0));
                }
            }
            _ = self.match(.@";");
        }

        return self.makeNode(.block, .{
            .statements = stats.items,
            .return_list = ret_list.items,
        });
    }

    fn parseParamList(self: *Parser) !struct { []const ast.Slice, bool } {
        var params = std.ArrayList(ast.Slice).init(self.scratch);
        defer params.definit();
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
    fn parseExp(self: *Parser, precedence: u8) !?ast.NodeID {
        if (try self.parseUnaryExp()) |lhs| {
            var node = lhs;
            while (precedence < self.next_token.tag.precedence(false)) {
                node = try self.parseBinaryExp(node);
                self.getToken();
            }
            return node;
        }
        return null;
    }

    fn expectExp(self: *Parser, precedence: u8) !ast.NodeID {
        return self.parseExp(precedence) orelse {
            const found = self.getLexeme(self.curr_token);
            self.err("expected expression, found {s}", .{found});
            return .none;
        };
    }

    fn parseUnaryExp(self: *Parser) !?ast.NodeID {
        switch (self.curr_token.tag) {
            .identifier => {
                return self.makeNode(.identifier, self.parseName());
            },
            .@"(" => {
                self.getToken();
                const node = try self.expectExp(0);
                self.expect(.@")");
                return node;
            },
            .keyword_not, .@"~", .@"-", .@"#" => {
                const op = switch (self.curr_token.tag) {
                    .@"#" => .length,
                    .@"-" => .negate,
                    .@"~" => .bitwise_not,
                    .keyword_not => .logic_not,
                };
                const prec = self.curr_token.tag.precedence(true);
                self.getToken();

                return self.makeNode(.unary_op, .{
                    .op = op,
                    .exp = try self.expectExp(prec),
                });
            },
            .keyword_nil => {
                self.getToken();
                return self.makeNode(.literal_nil, .{});
            },
            .keyword_true => {
                self.getToken();
                return self.makeNode(.literal_true, .{});
            },
            .keyword_false => {
                self.getToken();
                return self.makeNode(.literal_false, .{});
            },
            .@"..." => {
                self.getToken();
                return self.makeNode(.literal_ellipsis, .{});
            },
            .literal_number => {
                const lexeme = self.getLexeme(self.curr_token);
                // TODO: Distinguish between floats and ints
                const value = std.fmt.parseInt(i64, lexeme, 10) catch blk: {
                    self.err("could not parse {s} as integer", .{lexeme});
                    break :blk 0;
                };
                self.getToken();
                return self.makeNode(.literal_int, value);
            },
            .literal_string => {
                const lexeme = self.getLexeme(self.curr_token);
                // TODO: Encode the string, place it in static storage section of arena allocator
                // NOTE: The string encode should be left for latter
                const value = lexeme;
                self.getToken();
                return self.makeNode(.literal_string, value);
            },
            .keyword_function => {
                const params, const variadic = try self.parseParamList();
                defer self.scratch.free(params);

                const block = try self.parseBlock();
                self.expect(.keyword_end);

                return self.makeNode(.func_anonym, .{
                    .params = params,
                    .block = block,
                    .variadic = variadic,
                });
            },
            else => return null,
        }
    }

    fn parseBinaryExp(self: *Parser, lhs: ast.NodeID) !ast.NodeID {
        const op = switch (self.curr_token.tag) {
            .keyword_or => .logic_or,
            .keyword_and => .logic_and,
            .@"=" => .equal,
            .@"~=" => .not_equal,
            .@"<" => .less,
            .@">" => .greater,
            .@"<=" => .less_equal,
            .@">=" => .greater_equal,
            .@"|" => .bitwise_or,
            .@"&" => .bitwise_and,
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
            .@"." => .member_access,
            .@"[" => .subscript,
            .@":", .@"(", .@"{", .@"'", .@"\"" => .func_call,
        };
        const prec = self.curr_token.tag.precedence(true);

        if (op == .func_call) {
            var member: ?ast.Slice = null;
            if (self.match(.@":")) {
                member = self.expectName();
            }
            if (try self.parseArgList()) |args| {
                defer self.scratch.free(args);
                return self.makeNode(.func_call, .{
                    .object = lhs,
                    .member = member,
                    .args = args,
                });
            }

            const args = try self.parseTableConstructor() orelse blk: {
                self.err("expected string literal, found {s}", .{
                    self.getLexeme(self.curr_token),
                });
                break :blk self.parseString();
            };
            return self.makeNode(.func_call, .{
                .object = lhs,
                .member = member,
                .args = args,
            });
        }

        self.getToken();
        const rhs = try self.expectExp(prec);
        if (op == .subscript) self.expect(.@"]");
        return self.makeNode(.binary_op, .{ .op = op, .lhs = lhs, .rhs = rhs });
    }

    fn parseArgList(self: *Parser) !?[]const ast.NodeID {
        // arglist ::= ‘(’ [exp {, exp}] ‘)’

        if (self.match(.@"(")) {
            var args = std.ArrayList(ast.NodeID).init(self.scratch);
            defer args.deinit();

            if (try self.parseExp(0)) |exp_| {
                try args.append(self.scratch, exp_);

                while (self.match(.@",")) {
                    const exp = try self.expectExp(0);
                    try args.append(self.scratch, exp);
                }
            }
            self.expect(.@")");
            return args.toOwnedSlice();
        }
        return null;
    }

    fn parseTableConstructor(self: *Parser) !?ast.NodeID {
        // tableconstructor ::= ‘{’ [field {fieldsep field} [fieldsep]] ‘}’
        // fieldsep ::= ‘,’ | ‘;’

        if (self.match(.@"{")) {
            var fields = std.ArrayList(Field).init(self.scratch);
            defer fields.deinit();

            if (try self.parseField()) |field_| {
                try fields.append(field_);

                while (self.match(.@",") or self.match(.@";")) {
                    const field = try parseField() orelse break;
                    try fields.append(field);
                }
            }
            self.expect(.@"}");
            return self.makeNode(.table_constructor, .{
                .fields = fields.items,
            });
        }
        return null;
    }

    const Field = struct {
        key: ?ast.NodeID = null,
        value: ast.NodeID,
    };

    fn parseField(self: *Parser) !?Field {
        // field ::= ‘[’ exp ‘]’ ‘=’ exp | Name ‘=’ exp | exp

        if (self.match(.@"[")) {
            const key = try self.expectExp(0);
            self.expect(.@"]");
            self.expect(.@"=");
            return Field{
                .key = key,
                .value = try self.expectExp(0),
            };
        }
        if (self.match(.identifier)) {
            const key = self.expectName();
            self.expect(.@"=");
            return Field{
                .key = key,
                .value = try self.expectExp(0),
            };
        }
        if (try self.parseExp(0)) |exp| {
            return Field{ .value = exp };
        }
        return null;
    }

    // FIXME: What happens in an expect fails???
    fn parseStatement(self: *Parser) !?ast.NodeID {
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
                return self.makeNode(.label, name);
            },
            .keyword_break => {
                self.getToken();
                return self.makeNode(.@"break", {});
            },
            .keyword_goto => {
                self.getToken();
                const name = self.expectName();
                return self.makeNode(.goto, name);
            },
            .keyword_do => {
                self.getToken();
                const block = try self.parseBlock();
                self.expect(.keyword_end);
                return block;
            },
            .keyword_while => {
                self.getToken();
                const cond = try self.expectExp(0);
                self.expect(.keyword_do);
                const block = try self.parseBlock();
                self.expect(.keyword_end);
                return self.makeNode(.@"while", .{
                    .cond = cond,
                    .block = block,
                });
            },
            .keyword_repeat => {
                self.getToken();
                const block = try self.parseBlock();
                self.expect(.keyword_until);
                const cond = try self.expectExp(0);
                self.expect(.keyword_end);
                return self.makeNode(.repeat, .{
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
                    branch.cond = self.expectExpression();
                    self.expect(.keyword_then);
                    branch.block = try self.parseBlock();

                    if (!self.match(.keyword_elseif)) break;
                }
                if (self.match(.keyword_else)) {
                    const else_block = try self.parseBlock();
                    self.expect(.keyword_end);

                    return self.makeNode(.if_else, .{
                        .branches = branches.items,
                        .else_block = else_block,
                    });
                }
                self.expect(.keyword_end);
                return self.makeNode(.@"if", .{
                    .branches = branches.items,
                });
            },
            .keyword_for => {
                self.getToken();
                const name = self.expectName();

                if (self.match(.@"=")) {
                    const start = try self.expectExp(0);
                    self.expect(.@",");
                    const end = try self.expectExp(0);

                    if (self.match(.@",")) {
                        const step = try self.expectExp(0);
                        self.expect(.keyword_do);
                        const block = try self.parseBlock();
                        self.expect(.keyword_end);

                        return self.makeNode(.for_init_step, .{
                            .name = name,
                            .start = start,
                            .step = step,
                            .end = end,
                            .block = block,
                        });
                    }
                    self.expect(.keyword_do);
                    const block = try self.parseBlock();
                    self.expect(.keyword_end);
                    return self.makeNode(.for_init, .{
                        .name = name,
                        .start = start,
                        .end = end,
                        .block = block,
                    });
                } else {
                    var names = std.ArrayList(ast.Slice).init(self.scratch);
                    defer names.deinit();
                    var values = std.ArrayList(ast.NodeID).init(self.scratch);
                    defer values.deinit();

                    try names.append(name);
                    while (self.match(.@",")) {
                        const other_name = self.expectName();
                        try names.append(other_name);
                    }
                    self.expect(.keyword_in);

                    while (true) {
                        try values.append(try self.expectExp(0));
                        if (!self.match(.@",")) break;
                    }
                    self.expect(.keyword_do);
                    const block = try self.parseBlock();
                    self.expect(.keyword_end);

                    return self.makeNode(.for_each, .{
                        .names = names.items,
                        .values = values.items,
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

                var member = false;
                if (self.match(.@":")) {
                    const name = self.expectName();
                    try names.append(name);
                    member = true;
                }

                const params, const variadic = try self.parseParamList();
                defer self.scratch.free(params);

                const block = try self.parseBlock();
                self.expect(.keyword_end);

                return self.makeNode(.func_decl, .{
                    .name = names.items,
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

                    const block = try self.parseBlock();
                    self.expect(.keyword_end);

                    return self.makeNode(.func_local, .{
                        .name = name,
                        .params = params,
                        .block = block,
                        .variadic = variadic,
                    });
                } else {
                    // TODO: go over this
                    var names = std.ArrayList(ast.Slice).init(self.scratch);
                    defer names.deinit();

                    try names.append(self.expectName());
                    _ = self.parseAttrib() orelse {}; // NOTE: Discarding attributes

                    while (self.match(.@",")) {
                        try names.append(self.expectName());
                        _ = self.parseAttrib() orelse {}; // NOTE: Discarding attributes
                    }
                    if (self.match(.@"=")) {
                        const values = self.expectExpList();
                        return self.makeNode(.local_decl_init, .{
                            .names = names.items,
                            .values = values,
                        });
                    }
                    return self.makeNode(.local_decl, .{
                        .names = names.items,
                    });
                }
            },
            else => return null,
        }
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
