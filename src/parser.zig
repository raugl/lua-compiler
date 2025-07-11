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
const ast = @import("ast_node.zig");

const Token = lx.Token;
const Lexer = lx.Lexer;
const Index = u32;

// const tagCast = Key.Tag.tagCast;
// const null_index = std.math.maxInt(Index);
//
// pub const Node = struct {
//     tag: Tag,
//     data: struct {
//         lhs: u32,
//         rhs: u32,
//     },
//
//     pub const Tag = enum {
//         // zig fmt: off
//         assignment,               // lhs: var_list, rhs: exp_list
//         func_call,                // lhs: name, rhs: arg_list
//         label,                    // string
//         @"break",                 // none
//         goto,                     // string
//         while_loop,               // lhs: cond, rhs: body
//         repeat_until,             // lhs: cond, rhs: body
//         if_do,                    // lhs: cond, rhs: body
//         if_else,                  // lhs: cond, rhs: body -> else
//         for_iter,                 // lhs: name, rhs: start -> end, body
//         for_iter_step,            // lhs: name, rhs: start -> end, step, body
//         for_each,                 // lhs: name_list -> exp_list, rhs: body
//         func_def,                 // lhs: name -> param_list, rhs: body
//         local_func_def,           // lhs: name -> param_list, rhs: body
//         local_var_decl,           // lhs: name_list, rhs: exp_list // NOTE: Ignoring attributes
//         // zig fmt: on
//     };
// };

// pub const _Node = struct {
//     tag: Tag,
//     data: Data = undefined,
//     next: Index = null_index,
//     list_len: u16 = 0,
//     token_tag: Token.Tag,
//     // loc_start: Index,
//
//     const Data = struct {
//         lhs: Index,
//         rhs: Index,
//     };
//
//     pub const Tag = enum(u8) {
//         // zig fmt: off
//         assignment,               // lhs: var_list, rhs: exp_list
//         func_call,                // lhs: name, rhs: arg_list
//         label,                    // string
//         @"break",                 // none
//         goto,                     // string
//         while_loop,               // lhs: cond, rhs: body
//         repeat_until,             // lhs: cond, rhs: body
//         if_do,                    // lhs: cond, rhs: body
//         if_else,                  // lhs: cond, rhs: body -> else
//         for_iter,                 // lhs: name, rhs: start -> end, body
//         for_iter_step,            // lhs: name, rhs: start -> end, step, body
//         for_each,                 // lhs: name_list -> exp_list, rhs: body
//         func_def,                 // lhs: name -> param_list, rhs: body
//         local_func_def,           // lhs: name -> param_list, rhs: body
//         local_var_decl,           // lhs: name_list, rhs: exp_list // NOTE: Ignoring attributes
//         // zig fmt: on
//
//         logic_or,
//         logic_not,
//         logic_and,
//         less_than,
//         greater_than,
//         less_or_equal,
//         greater_or_equal,
//         not_equal,
//         equal,
//         bit_or,
//         bit_not,
//         bit_and,
//         bit_shift_left,
//         bit_shift_right,
//         str_concat,
//         add,
//         sub,
//         mul,
//         div,
//         int_div,
//         modulo,
//         length,
//         negate,
//         exponent,
//         identifier,
//         literal_nil,
//         literal_true,
//         literal_false,
//         literal_int,
//         literal_float,
//         literal_string,
//
//         var_list = 256,
//         exp_list = 320,
//         arg_list = 384,
//         name_list = 448,
//         param_list = 512,
//         statement_list = 576,
//         _,
//
//         pub fn listLen(tag: Tag) u32 {
//             switch (tag) {
//                 0...256 => unreachable,
//                 else => @intFromEnum(tag) - 256,
//             }
//         }
//
//         pub fn makeLen(tag: Tag, len: u32) Tag {
//             switch (tag) {
//                 .var_list,
//                 .exp_list,
//                 .arg_list,
//                 .param_list,
//                 .statement_list,
//                 => return @intFromEnum(tag) + len,
//                 else => unreachable,
//             }
//         }
//
//         pub fn isUnaryOp(tag: Tag) bool {
//             return switch (tag) {
//                 .logic_not, .bit_not, .length, .negate => true,
//                 else => false,
//             };
//         }
//
//         pub fn isBinaryOp(tag: Tag) bool {
//             return switch (tag) {
//                 .logic_or,
//                 .logic_and,
//                 .less_than,
//                 .greater_than,
//                 .less_or_equal,
//                 .greater_or_equal,
//                 .not_equal,
//                 .equal,
//                 .bit_or,
//                 .bit_and,
//                 .bit_shift_left,
//                 .bit_shift_right,
//                 .str_concat,
//                 .add,
//                 .sub,
//                 .mul,
//                 .div,
//                 .int_div,
//                 .modulo,
//                 .exponent,
//                 => true,
//                 else => false,
//             };
//         }
//
//         pub fn precedence(tag: Tag) u8 {
//             return switch (tag) {
//                 .logic_or => 1,
//                 .logic_and => 2,
//                 .equal,
//                 .not_equal,
//                 .less_than,
//                 .greater_than,
//                 .less_or_equal,
//                 .greater_or_equal,
//                 => 3,
//                 .bit_or => 4,
//                 .bit_and => 5,
//                 .bit_shift_left, .bit_shift_right => 6,
//                 .str_concat => 7,
//                 .add, .sub => 8,
//                 .mul, .div, .int_div, .modulo => 9,
//                 .length, .negate, .bit_not, .logic_not => 10,
//                 .exponent => 11,
//                 .identifier,
//                 .literal_int,
//                 .literal_float,
//                 .literal_string,
//                 .literal_nil,
//                 .literal_true,
//                 .literal_false,
//                 => 0,
//             };
//         }
//
//         fn tagCast(tag: Token.Tag) Node.Tag {
//             return switch (tag) {
//                 .identifier => .identifier,
//                 .literal_number => .literal_int, // FIXME: floats
//                 .literal_string => .literal_string,
//                 .@"+" => .add,
//                 .@"-" => .sub, // FIXME: what about .nagate
//                 .@"*" => .mul,
//                 .@"/" => .div,
//                 .@"%" => .modulo,
//                 .@"^" => .exponent,
//                 .@"#" => .length,
//                 .@"&" => .bit_and,
//                 .@"~" => .bit_not,
//                 .@"|" => .bit_or,
//                 .@"<<" => .bit_shift_left,
//                 .@">>" => .bit_shift_right,
//                 .@"//" => .int_div,
//                 .@"==" => .equal,
//                 .@"!=" => .not_equal,
//                 .@"<=" => .less_or_equal,
//                 .@">=" => .greater_or_equal,
//                 .@"<" => .less_than,
//                 .@">" => .greater_than,
//                 .@".." => .str_concat,
//                 .@"(",
//                 .@")",
//                 .@",",
//                 .@".",
//                 .@"...",
//                 .@":",
//                 .@"::",
//                 .@";",
//                 .@"=",
//                 .@"[",
//                 .@"]",
//                 .@"{",
//                 .@"}",
//                 .comment,
//                 .eof,
//                 .invalid,
//                 .keyword_break,
//                 .keyword_do,
//                 .keyword_else,
//                 .keyword_elseif,
//                 .keyword_end,
//                 .keyword_for,
//                 .keyword_function,
//                 .keyword_goto,
//                 .keyword_if,
//                 .keyword_in,
//                 .keyword_local,
//                 .keyword_repeat,
//                 .keyword_return,
//                 .keyword_then,
//                 .keyword_until,
//                 .keyword_while,
//                 => unreachable, // TODO
//             };
//         }
//     };
// };

pub const Parser = struct {
    lexer: Lexer,
    curr_token: Token,
    next_token: Token,

    // PERF: the messages and nodes shouldn't be interleaved by using the same allocator
    alloc: std.mem.Allocator,
    scratch: std.mem.Allocator,

    extra: std.ArrayListUnmanaged(u32) = .empty,
    nodes: std.ArrayListUnmanaged(ast.Node) = .empty,
    messages: std.ArrayListUnmanaged(Message) = .empty,

    const Message = struct {
        loc: Token.Loc,
        text: []const u8,
        level: enum { note, warninig, @"error" },
    };

    pub fn init(lexer: Lexer) Parser {
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

    // TODO: use a scratch allocator
    fn parseStatement(self: *Parser) ?ast.NodeID {
        switch (self.curr_token) {
            .@";" => {
                self.getToken();
                return self.parseStatement();
            },
            .@"(", .identifier => {
                // var, funccall
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
            else => return null,
        }
    }

    // TODO: fix token advancing in parsing
    fn parseExpression(self: *Parser, precedence: u8) !Index {
        var node = try self.parsePrefixExp();
        while (precedence < tagCast(self.next_token.tag).precedence()) {
            node = try self.parseInfixExp(node);
            self.getToken();
        }
        return node;
    }

    fn parsePrefixExp(self: *Parser) !Index {
        std.debug.assert(tagCast(self.curr_token.tag).isUnaryOp());
        defer self.getToken();

        switch (self.curr_token.tag) {
            .identifier, .keyword_nil, .keyword_true, .keyword_false, .@"..." => {
                return self.makeNode(self.curr_token);
            },
            .@"(" => {
                const node = try self.parseExpression(0);
                self.expectTok(.@")");
                return node;
            },
            .keyword_not, .@"~", .@"-", .@"#" => {
                const node = try self.makeNode(self.curr_token);
                const precedence = Key.Tag.negate.precedence();
                self.getToken();
                const value = try self.parseExpression(precedence);
                self.getValue(node).prefix_exp = value;
                return node;
            },
            .literal_number => {
                const node = try self.makeNode(self.curr_token);
                const lexeme = self.getLexeme(self.curr_token);
                const value = std.fmt.parseInt(i64, lexeme, 10) catch blk: {
                    self.err("could not parse {s} as integer", .{lexeme});
                    break :blk 0;
                };
                self.getValue(node).literal_int = value;
                return node;
            },
            .literal_string => {
                const node = try self.makeNode(self.curr_token);
                const lexeme = self.getLexeme(self.curr_token);
                // TODO: Encode the string, place it in static storage section of arena allocator
                const value = lexeme;
                self.getValue(node).literal_int = value;
                return node;
            },
            else => unreachable,
        }
    }

    fn parseInfixExp(self: *Parser, lhs: Index) !Index {
        std.debug.assert(tagCast(self.curr_token.tag).isBinaryOp());

        const node = try self.makeNode(self.curr_token);
        const precedence = tagCast(self.curr_token.tag).precedence();
        self.getToken();

        const rhs = try self.parseExpression(precedence);
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

    fn getValue(self: Parser, node: Index) *Key.ValueUnion {
        return &self.nodes.items[node].value;
    }

    fn getLexeme(self: Parser, token: Token) []const u8 {
        return self.lexer.buffer[token.loc.start..token.loc.end];
    }

    fn makeNode(self: *Parser, token: Token) std.mem.Allocator.Error!Index {
        const node_idx = self.nodes.items.len;
        try self.nodes.append(
            self.alloc,
            Key{ .token = token, .tag = tagCast(token.tag), .value = undefined },
        );
        return node_idx;
    }

    fn match(self: *Parser, tag: Token.Tag) bool {
        if (self.curr_token.tag == tag) {
            self.getToken();
            return true;
        }
        return false;
    }

    fn expectTok(self: *Parser, tag: Token.Tag) void {
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
