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
const lx = @import("lexer.zig");
const ast = @import("ast.zig");

pub const Parser = struct {
    lexer: lx.Lexer,
    curr_token: lx.Token,
    next_token: lx.Token,

    // PERF: the messages and nodes shouldn't be interleaved by using the same allocator
    alloc: std.mem.Allocator,
    scratch: std.mem.Allocator,

    extra: std.ArrayListUnmanaged(u32) = .empty,
    nodes: std.ArrayListUnmanaged(ast.Node) = .empty,
    messages: std.ArrayListUnmanaged(Message) = .empty,

    const Message = struct {
        loc: lx.Token.Loc,
        text: []const u8,
        level: enum { note, warninig, @"error" },
    };

    pub fn init(lexer: lx.Lexer) Parser {
        var self = Parser{
            .lexer = lexer,
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

    fn expect(tag: lx.Token.Tag) void {
        // TODO
    }

    // TODO: use a scratch allocator
    fn parseStatement(self: *Parser) ?ast.NodeID {
        switch (self.curr_token) {
            .@";" => {
                self.getToken();
                return self.parseStatement();
            },
            .@"(", .identifier => {
                // var, funccall
                // table[func()]()[] = 100

                // prefixexp ::= (Name | ‘(’ exp ‘)’) [ ‘[’ exp ‘]’ | ‘.’ Name | [‘:’ Name] arglist]
                // arglist ::= ‘(’ [exp {, exp}] ‘)’ | tableconstructor | LiteralString

                if (self.curr_token.tag == .identifier) {
                    const name = self.parseName();
                    const node = self.makeNode(.identifier, name);
                } else {
                    self.getToken();
                    const exp = self.expectExp();
                    self.expect(.@")");
                }
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
                const block = self.expectBlock();
                self.expect(.keyword_end);
                return block;
            },
            .keyword_while => {
                self.getToken();
                const cond = self.expectExp();
                self.expect(.keyword_do);
                const block = self.expectBlock();
                self.expect(.keyword_end);
                return self.makeNode(.@"while", .{
                    .cond = cond,
                    .block = block,
                });
            },
            .keyword_repeat => {
                self.getToken();
                const block = self.expectBlock();
                self.expect(.keyword_until);
                const cond = self.expectExp();
                self.expect(.keyword_end);
                return self.makeNode(.repeat, .{
                    .cond = cond,
                    .block = block,
                });
            },
            .keyword_if => {
                var branches = std.ArrayList(ast.Conditional).init(self.alloc);
                defer branches.deinit();
                self.getToken();

                while (true) {
                    const branch = branches.addOne() catch @panic("OOM");
                    branch.cond = self.expectExpression();
                    self.expect(.keyword_then);
                    branch.block = self.expectBlock();

                    if (!self.match(.keyword_elseif)) break;
                }
                if (self.match(.keyword_else)) {
                    const else_block = self.expectBlock();
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
                    const start = self.expectExp();
                    self.expect(.@",");
                    const end = self.expectExp();

                    if (self.match(.@",")) {
                        const step = self.expectExp();
                        self.expect(.keyword_do);
                        const block = self.expectBlock();
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
                    const block = self.expectBlock();
                    self.expect(.keyword_end);
                    return self.makeNode(.for_init, .{
                        .name = name,
                        .start = start,
                        .end = end,
                        .block = block,
                    });
                } else {
                    var names = std.ArrayList(ast.Slice).init(self.alloc);
                    defer names.deinit();
                    var values = std.ArrayList(ast.NodeID).init(self.alloc);
                    defer values.deinit();

                    names.append(name) catch @panic("OOM");
                    while (self.match(.@",")) {
                        const other_name = self.expectName();
                        names.append(other_name) catch @panic("OOM");
                    }
                    self.expect(.keyword_in);

                    while (true) {
                        values.append(self.expectExp()) catch @panic("OOM");
                        if (!self.match(.@",")) break;
                    }
                    self.expect(.keyword_do);
                    const block = self.expectBlock();
                    self.expect(.keyword_end);

                    return self.makeNode(.for_each, .{
                        .names = names.items,
                        .values = values.items,
                        .block = block,
                    });
                }
            },
            .keyword_function => {
                var names = std.ArrayList(ast.Slice).init(self.alloc);
                defer names.deinit();
                var params = std.ArrayList(ast.Slice).init(self.alloc);
                defer params.definit();

                var is_member = false;
                var is_variadic = false;

                self.getToken();
                while (true) {
                    const name = self.expectName();
                    names.append(name) catch @panic("OOM");
                    if (!self.match(.@",")) break;
                }
                if (self.match(.@":")) {
                    const name = self.expectName();
                    names.append(name) catch @panic("OOM");
                    is_member = true;
                }

                self.expect(.@"(");
                while (true) {
                    if (self.match(.@"...")) {
                        is_variadic = true;
                        break;
                    }
                    const name = self.expectName();
                    params.append(name) catch @panic("OOM");
                    if (!self.match(.@",")) break;
                }
                self.expect(.@")");

                return self.makeNode(.func_def, .{
                    .name = names.items,
                    .params = params.items,
                    .block = self.expectFuncBody(),
                    .member = is_member,
                    .variadic = is_variadic,
                });
            },
            // TODO: go over this
            .keyword_local => {
                self.getToken();
                if (self.match(.keyword_function)) {
                    const name = self.expectName();
                    const body = self.expectFuncBody();
                    return self.makeNode(.local_func_def, .{
                        .name = name,
                        .body = body,
                    });
                } else {
                    var names = std.ArrayList(ast.Slice).init(self.alloc);
                    defer names.deinit();

                    names.append(self.expectName()) catch @panic("OOM");
                    _ = self.parseAttrib() orelse {}; // NOTE: Discarding attributes

                    while (self.match(.@",")) {
                        names.append(self.expectName()) catch @panic("OOM");
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
            else => if (self.parseExp()) |exp| {
                // var, funccall

                // var {‘,’ var} ‘=’ explist
                // var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name
                // prefixexp ::= exp | exp [‘:’ Name] arglist

            },
        }
        return null;
    }

    // TODO: fix token advancing in parsing
    // FIXME: All 3 functions
    fn parseExp(self: *Parser, precedence: u8) ?ast.NodeID {
        if (self.parsePrefixExp()) |exp| {
            var node = exp;
            while (precedence < self.next_token.tag.precedence(false)) {
                node = self.parseInfixExp(node);
                self.getToken();
            }
            return node;
        }
        return null;
    }

    fn parsePrefixExp(self: *Parser) ?ast.NodeID {
        // prefixexp ::= (Name | ‘(’ exp ‘)’) [ ‘.’ Name | ‘[’ exp ‘]’ | [‘:’ Name] arglist]
        // arglist ::= ‘(’ [exp {, exp}] ‘)’ | tableconstructor | LiteralString

        switch (self.curr_token.tag) {
            .identifier => {
                // parsePrefixExpHelper();
                const lhs = self.parseName();

                if (self.match(.@".")) {
                    const rhs = self.expectName();
                    return self.makeNode(.member_access, .{ .lhs = lhs, .rhs = rhs });
                }
                if (self.match(.@"[")) {
                    const rhs = self.parseExp(0);
                    self.expect(.@"]");
                    return self.makeNode(.subscript, .{ .lhs = lhs, .rhs = rhs });
                }

                var func: ?ast.NodeID = null;
                if (self.match(.@":")) {
                    func = self.expectName();
                }

                if (self.match(.@"(")) {
                    if (self.parseExp()) |exp| {
                        try explist.append(self.alloc, exp);
                        while (self.match(.@",")) {
                            self.expectExp();
                            try explist.append(self.alloc, self.expectExp());
                        }
                    }
                    self.expect(.@")");
                }
                if (self.match(.@"{")) {
                    // TODO:
                    self.expect(.@"}");
                }
            },
            .keyword_nil, .keyword_true, .keyword_false, .@"..." => {
                self.getToken();
                return self.makeNode(self.curr_token);
            },
            .@"(" => {
                self.getToken();
                const node = self.expectExp();
                self.expectTok(.@")");
                return node;
            },
            .keyword_not, .@"~", .@"-", .@"#" => {
                const node = self.makeNode(self.curr_token);
                const precedence = self.curr_token.tag.precedence(true);
                self.getToken();
                const value = self.parseExp(precedence) orelse @panic("expected expression");
                self.getValue(node).prefix_exp = value;
                self.getToken();
                return node;
            },
            .literal_number => {
                const node = self.makeNode(self.curr_token);
                const lexeme = self.getLexeme(self.curr_token);
                const value = std.fmt.parseInt(i64, lexeme, 10) catch blk: {
                    self.err("could not parse {s} as integer", .{lexeme});
                    break :blk 0;
                };
                self.getValue(node).literal_int = value;
                self.getToken();
                return node;
            },
            .literal_string => {
                const node = self.makeNode(self.curr_token);
                const lexeme = self.getLexeme(self.curr_token);
                // TODO: Encode the string, place it in static storage section of arena allocator
                const value = lexeme;
                self.getValue(node).literal_int = value;
                self.getToken();
                return node;
            },
            else => return null,
        }
    }

    fn parseInfixExp(self: *Parser, lhs: ast.NodeID) ast.NodeID {
        const node = self.makeNode(self.curr_token) catch @panic("OOM");
        const precedence = self.curr_token.tag.precedence(false);
        self.getToken();

        const rhs = self.parseExp(precedence) orelse @panic("expected expression");
        // FIXME: change this to the Key api
        self.getValue(node).infix_exp.lhs = lhs;
        self.getValue(node).infix_exp.rhs = rhs;
        self.getToken();
        return node;
    }

    // ==== TODO: ==================================================================================

    pub fn parse(self: *Parser, alloc: std.mem.Allocator) ![]const Key {
        // TODO: parsing

        // TODO: flush messages
        // NOTE: the message queue may need to be encoded as a some sort of
        // tree so that it can be sorted while preserving the order of context
        // messages
        return self.nodes.toOwnedSlice(alloc);
    }

    fn getValue(self: Parser, node: ast.NodeID) *Key.ValueUnion {
        return &self.nodes.items[node].value;
    }

    fn getLexeme(self: Parser, token: lx.Token) []const u8 {
        return self.lexer.buffer[token.loc.start..token.loc.end];
    }

    fn makeNode(self: *Parser, token: lx.Token) std.mem.Allocator.Error!ast.NodeID {
        const node_idx = self.nodes.items.len;
        try self.nodes.append(
            self.alloc,
            Key{ .token = token, .tag = tagCast(token.tag), .value = undefined },
        );
        return node_idx;
    }

    fn match(self: *Parser, tag: lx.Token.Tag) bool {
        if (self.curr_token.tag == tag) {
            self.getToken();
            return true;
        }
        return false;
    }

    fn expectTok(self: *Parser, tag: lx.Token.Tag) void {
        if (self.next_token.tag == tag) {
            self.getToken();
        } else {
            // FIXME: this way char token won't be wrapped in quotes
            self.err("expected {s}, found {s}", .{ tag.lexeme(), self.getLexeme(self.curr_token) });
        }
    }

    fn expect(self: *Parser, ok: bool, comptime lexeme: []const u8) void {
        if (!ok) {
            // FIXME: this way char token won't be wrapped in quotes
            self.err("expected {s}, found {s}", .{ lexeme, self.getLexeme(self.curr_token) });
        }
    }

    fn parseBlock(self: *Parser) bool {
        while (self.matchStatement()) {}
        if (self.match(.keyword_return)) {
            if (self.matchExpList()) {}
            if (self.match(.semicolon)) {}
        }
        return true;
    }

    // FIXME: What happens in an expect fails???
    fn matchStatement(self: *Parser) bool {
        if (self.match(.@";") or self.matchFuncCall() or self.match(.keyword_break)) {
            return true;
        }
        if (self.matchVar()) {
            while (self.match(.@",")) {
                self.expect(self.matchVar(), "<name>");
            }
            self.expectTok(.@"=");
            self.expect(self.matchExpList(), "expression");
            return true;
        }
        if (self.match(.@"::")) {
            self.expectTok(.identifier);
            self.expectTok(.@"::");
            return true;
        }
        if (self.match(.keyword_goto)) {
            self.expectTok(.identifier);
            return true;
        }
        if (self.match(.keyword_do)) {
            self.expect(self.matchBlock(), "block");
            self.expectTok(.keyword_end);
            return true;
        }
        if (self.match(.keyword_while)) {
            self.expect(self.matchExpression(), "expression");
            self.expectTok(.keyword_do);
            self.expect(self.matchBlock(), "block");
            self.expectTok(.keyword_end);
            return true;
        }
        if (self.match(.keyword_repeat)) {
            self.expect(self.matchBlock(), "block");
            self.expectTok(.keyword_until);
            self.expect(self.matchExpression(), "expression");
            return true;
        }

        if (self.match(.keyword_for)) {
            self.expectTok(.identifier);
            if (self.match(.@"=")) {
                self.expect(self.matchExpression(), "expression");
                self.expectTok(.@",");
                self.expect(self.matchExpression(), "expression");

                while (self.match(.@",")) {
                    self.expect(self.matchExpression(), "expression");
                }
                self.expectTok(.keyword_do);
                self.expect(self.matchBlock(), "block");
                self.expectTok(.keyword_end);
            } else {
                while (self.match(.@",")) {
                    self.expectTok(.identifier);
                }
                self.expectTok(.keyword_in);
                self.expect(self.matchExpList(), "expression");
                self.expectTok(.keyword_do);
                self.expect(self.matchBlock(), "block");
                self.expectTok(.keyword_end);
            }
            return true;
        }
        if (self.match(.keyword_function)) {
            self.expectTok(.identifier);
            while (self.match(.@".")) {
                self.expectTok(.identifier);
            }
            if (self.match(.@":")) {
                self.expectTok(.identifier);
            }
            self.expect(self.matchFuncBody(), "function body");
            return true;
        }
        if (self.match(.keyword_local)) {
            if (self.expectTok(.keyword_function)) {
                self.expectTok(.identifier);
                self.expect(self.matchFuncBody(), "function body");
            } else {
                self.expectTok(.identifier);
                _ = self.matchAttribute();
                while (self.match(.@",")) {
                    self.expectTok(.identifier);
                    _ = self.matchAttribute();
                }
                if (self.match(.@"=")) {
                    self.expect(self.matchExpList(), "expression");
                }
            }
            return true;
        }
        return false;
    }

    fn matchAttribute(self: *Parser) bool {
        if (self.expectTok(.@"<")) {
            self.expectTok(.identifier);
            self.expectTok(.@">");
        }
        return true;
    }

    fn matchVar(self: *Parser) bool {
        if (self.match(.identifier)) {
            return true;
        }
        if (self.matchPrefixExp()) {
            self.expectTok(.@"[");
            self.expect(self.matchExpression(), "expression");
            self.expectTok(.@"]");
            return true;
        }
        if (self.matchPrefixExp()) {
            self.expectTok(.@".");
            self.expectTok(.identifier);
        }
        return true;
    }

    fn matchExpList(self: *Parser) bool {
        if (self.matchExpression()) {
            while (self.match(.@",")) {
                self.expect(self.matchExpression(), "expression");
            }
            return true;
        }
        return false;
    }

    fn matchExp(self: *Parser) bool {}

    fn matchFuncCall(self: *Parser) bool {
        if (self.matchPrefixExp()) {
            self.expect(self.matchArgList(), "arguments");
            if (self.matchArgList()) {
                return true;
            } else {
                self.expectTok(.@":");
                self.expectTok(.identifier);
                self.expect(self.matchArgList(), "arguments");
                return true;
            }
        }
        return false;
    }

    fn matchArgList(self: *Parser) bool {
        if (self.match(.@"(")) {
            if (self.matchExpList()) {}
            self.expectTok(.@")");
            return true;
        }
        return self.matchTableConstructor() or self.match(.literal_string);
    }

    fn matchFuncBody(self: *Parser) bool {
        if (self.match(.@"(")) {
            if (self.match(.identifier)) {
                while (self.match(.@",")) {
                    self.expectTok(.identifier);
                }
            }
            self.expectTok(.@")");
            self.expect(self.matchBlock(), "block");
            self.expectTok(.keyword_end);
            return true;
        }
        return false;
    }

    fn matchParamList(self: *Parser) bool {
        if (self.match(.identifier)) {
            while (self.match(.@",")) {
                self.expectTok(.identifier);
            }
            if (self.match(.@",")) {
                self.expectTok(.@"...");
            }
            return true;
        }
        return self.match(.@"...");
    }

    fn matchTableConstructor(self: *Parser) bool {
        if (self.match(.@"{")) {
            if (self.matchField()) {
                while (self.matchFieldSep()) {
                    self.expect(self.matchField(), "field");
                }
                if (self.matchFieldSep()) {}
            }
            self.expectTok(.@"}");
            return true;
        }
        return false;
    }

    fn matchField(self: *Parser) bool {
        if (self.match(.@"[")) {
            self.expect(self.matchExpression(), "expression");
            self.expectTok(.@"]");
            self.expectTok(.@"=");
            self.expect(self.matchExpression(), "expression");
            return true;
        }
        if (self.match(.identifier)) {
            self.expectTok(.@"=");
            self.expect(self.matchExpression(), "expression");
        }
        return self.matchExpression();
    }

    fn matchFieldSep(self: *Parser) bool {
        return self.match(.@",") or self.match(.@";");
    }
};
