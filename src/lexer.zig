const std = @import("std");

// TODO: Add token lexemes for better error messages
// TODO: Relax the requirements for valid escape sequences
// TODO: Setup fuzz testing

pub const Precedence = enum(u8) {
    any,
    logic_or,
    logic_and,
    comparison,
    bitwise_or,
    bitwise_and,
    bit_shift,
    str_concat,
    add,
    mul,
    negate,
    exponent,
    func_call, // This is for function calls, member access, and subscript access
};

pub const Location = struct {
    start: u32,
    end: u32,
};

pub const Token = struct {
    tag: Tag,
    loc: Location,

    pub const Tag = enum {
        eof,
        invalid,
        comment,
        identifier,
        literal_number,
        literal_string,
        @"+",
        @"-",
        @"*",
        @"/",
        @"%",
        @"^",
        @"#",
        @"&",
        @"~",
        @"|",
        @"<<",
        @">>",
        @"//",
        @"==",
        @"~=",
        @"<=",
        @">=",
        @"<",
        @">",
        @"=",
        @"(",
        @")",
        @"{",
        @"}",
        @"[",
        @"]",
        @"::",
        @";",
        @":",
        @",",
        @".",
        @"..",
        @"...",
        keyword_and,
        keyword_break,
        keyword_do,
        keyword_else,
        keyword_elseif,
        keyword_end,
        keyword_false,
        keyword_for,
        keyword_function,
        keyword_goto,
        keyword_if,
        keyword_in,
        keyword_local,
        keyword_nil,
        keyword_not,
        keyword_or,
        keyword_repeat,
        keyword_return,
        keyword_then,
        keyword_true,
        keyword_until,
        keyword_while,

        pub fn lexeme(tag: Tag) [:0]const u8 {
            return switch (tag) {
                .eof => "<eof>",
                .invalid => "<invalid>",
                .comment => "<comment>",
                .identifier => "<name>",
                .literal_number => "<number>",
                .literal_string => "<string>",
                inline else => |foo| {
                    const name = @tagName(foo);
                    return if (name.len > 8)
                        "`" ++ name[8..] ++ "`"
                    else
                        "symbol `" ++ name ++ "`";
                },
            };
        }
    };

    pub fn precedence(self: Token, is_prefix: bool) Precedence {
        return switch (self.tag) {
            .keyword_or => .logic_or,
            .keyword_and => .logic_and,
            .@"=", .@"~=", .@"<", .@">", .@"<=", .@">=" => .comparison,
            .@"|" => .bitwise_or,
            .@"&" => .bitwise_and,
            .@"<<", .@">>" => .bit_shift,
            .@".." => .str_concat,
            .@"+" => .add,
            .@"*", .@"/", .@"//", .@"%" => .mul,
            .@"#", .@"~", .keyword_not => .negate,
            .@"-" => if (is_prefix) .negate else .add,
            .@"^" => .exponent,
            else => .any,
        };
    }

    pub fn getKeyword(bytes: []const u8) ?Tag {
        return keywords.get(bytes);
    }

    pub const keywords = std.StaticStringMap(Tag).initComptime(.{
        .{ "and", .keyword_and },
        .{ "break", .keyword_break },
        .{ "do", .keyword_do },
        .{ "else", .keyword_else },
        .{ "elseif", .keyword_elseif },
        .{ "end", .keyword_end },
        .{ "false", .keyword_false },
        .{ "for", .keyword_for },
        .{ "function", .keyword_function },
        .{ "goto", .keyword_goto },
        .{ "if", .keyword_if },
        .{ "in", .keyword_in },
        .{ "local", .keyword_local },
        .{ "nil", .keyword_nil },
        .{ "not", .keyword_not },
        .{ "or", .keyword_or },
        .{ "repeat", .keyword_repeat },
        .{ "return", .keyword_return },
        .{ "then", .keyword_then },
        .{ "true", .keyword_true },
        .{ "until", .keyword_until },
        .{ "while", .keyword_while },
    });
};

pub const Lexer = struct {
    buffer: [:0]const u8,
    index: u32,

    const State = enum {
        start,
        invalid,
        @"-",
        @"/",
        @"~",
        @"<",
        @">",
        @"=",
        @"[",
        @":",
        @".",
        @"..",
        identifier,
        literal_string,
        literal_string_backslash,
        literal_string_whitespace,
        comment,
        comment_body,
        long_bracket_body,
        long_bracket_end,
        int,
        int_period,
        int_exponent,
        float,
        float_exponent,
    };

    pub fn init(buffer: [:0]const u8) Lexer {
        return .{
            .buffer = buffer,
            .index = if (std.mem.startsWith(u8, buffer, "\xEF\xBB\xBF")) 3 else 0, // TODO: What is this? Skip the UTF-8 BOM if present.
        };
    }

    pub fn next(self: *Lexer) Token {
        var result = Token{
            .tag = undefined,
            .loc = .{
                .start = self.index,
                .end = undefined,
            },
        };
        var quote_type: enum { single, double } = undefined;
        var long_bracket_level: u32 = 0;

        state: switch (State.start) {
            .start => {
                switch (self.buffer[self.index]) {
                    0 => if (self.index == self.buffer.len) return .{
                        .tag = .eof,
                        .loc = .{
                            .start = self.index,
                            .end = self.index,
                        },
                    } else {
                        continue :state .invalid;
                    },
                    ' ', '\t', '\r', '\n', 0xb, 0xc => {
                        self.index += 1;
                        result.loc.start = self.index;
                        continue :state .start;
                    },
                    '+' => result.tag = .@"+",
                    '*' => result.tag = .@"*",
                    '%' => result.tag = .@"%",
                    '^' => result.tag = .@"^",
                    '#' => result.tag = .@"#",
                    '&' => result.tag = .@"&",
                    '|' => result.tag = .@"|",
                    '(' => result.tag = .@"(",
                    ')' => result.tag = .@")",
                    '{' => result.tag = .@"{",
                    '}' => result.tag = .@"}",
                    ']' => result.tag = .@"]",
                    ';' => result.tag = .@";",
                    ',' => result.tag = .@",",
                    '-' => continue :state .@"-",
                    '/' => continue :state .@"/",
                    '~' => continue :state .@"~",
                    '<' => continue :state .@"<",
                    '>' => continue :state .@">",
                    '=' => continue :state .@"=",
                    ':' => continue :state .@":",
                    '.' => continue :state .@".",
                    '[' => {
                        result.tag = .literal_string;
                        continue :state .@"[";
                    },
                    '"' => {
                        quote_type = .double;
                        continue :state .literal_string;
                    },
                    '\'' => {
                        quote_type = .single;
                        continue :state .literal_string;
                    },
                    'a'...'z', 'A'...'Z', '_' => {
                        continue :state .identifier;
                    },
                    '0'...'9' => {
                        result.tag = .literal_number;
                        self.index += 1;
                        continue :state .int;
                    },
                    else => continue :state .invalid,
                }
                self.index += 1;
            },
            .invalid => {
                switch (self.buffer[self.index]) {
                    '\n' => result.tag = .invalid,
                    0 => if (self.index == self.buffer.len) {
                        result.tag = .invalid;
                    } else {
                        self.index += 1;
                        continue :state .invalid;
                    },
                    else => {
                        self.index += 1;
                        continue :state .invalid;
                    },
                }
            },
            .@"-" => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-' => continue :state .comment,
                    else => result.tag = .@"-",
                }
            },
            .@"/" => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '/' => {
                        self.index += 1;
                        result.tag = .@"//";
                    },
                    else => result.tag = .@"/",
                }
            },
            .@"~" => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        self.index += 1;
                        result.tag = .@"~=";
                    },
                    else => result.tag = .@"~",
                }
            },
            .@"<" => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '<' => {
                        self.index += 1;
                        result.tag = .@"<<";
                    },
                    '=' => {
                        self.index += 1;
                        result.tag = .@"<=";
                    },
                    else => result.tag = .@"<",
                }
            },
            .@">" => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '>' => {
                        self.index += 1;
                        result.tag = .@">>";
                    },
                    '=' => {
                        self.index += 1;
                        result.tag = .@">=";
                    },
                    else => result.tag = .@">",
                }
            },
            .@"=" => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '=' => {
                        self.index += 1;
                        result.tag = .@"==";
                    },
                    else => result.tag = .@"=",
                }
            },
            .@":" => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    ':' => {
                        result.tag = .@"::";
                        self.index += 1;
                    },
                    else => result.tag = .@":",
                }
            },
            .@"." => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '.' => continue :state .@"..",
                    else => result.tag = .@".",
                }
            },
            .@".." => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '.' => {
                        self.index += 1;
                        result.tag = .@"...";
                    },
                    else => result.tag = .@"..",
                }
            },
            .@"[" => {
                self.index += 1;
                while (self.buffer[self.index] == '=') {
                    self.index += 1;
                    long_bracket_level += 1;
                }
                switch (self.buffer[self.index]) {
                    0 => continue :state .invalid,
                    '[' => continue :state .long_bracket_body,
                    else => switch (result.tag) {
                        .comment => {
                            continue :state .comment_body;
                        },
                        else => {
                            result.tag = .@"[";
                            self.index = result.loc.start + 1;
                        },
                    },
                }
            },
            .long_bracket_body => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => continue :state .invalid,
                    ']' => continue :state .long_bracket_end,
                    else => continue :state .long_bracket_body,
                }
            },
            .long_bracket_end => {
                self.index += 1;

                var level: u32 = 0;
                while (self.buffer[self.index] == '=') {
                    self.index += 1;
                    level += 1;
                }
                switch (self.buffer[self.index]) {
                    0 => continue :state .invalid,
                    ']' => if (level == long_bracket_level) {
                        self.index += 1;
                    } else {
                        continue :state .long_bracket_end;
                    },
                    else => continue :state .long_bracket_body,
                }
            },
            .identifier => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a'...'z', 'A'...'Z', '_', '0'...'9' => continue :state .identifier,
                    else => {
                        const ident = self.buffer[result.loc.start..self.index];
                        result.tag = Token.getKeyword(ident) orelse .identifier;
                    },
                }
            },
            .literal_string => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '"' => if (quote_type == .double) {
                        self.index += 1;
                        result.tag = .literal_string;
                    } else {
                        continue :state .literal_string;
                    },
                    '\'' => if (quote_type == .single) {
                        self.index += 1;
                        result.tag = .literal_string;
                    } else {
                        continue :state .literal_string;
                    },
                    0, '\r', '\n' => continue :state .invalid,
                    '\\' => continue :state .literal_string_backslash,
                    else => continue :state .literal_string,
                }
            },
            .literal_string_backslash => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    'a', 'b', 'f', 'n', 'r', 't', 'v', '"', '\'', '\\', '\r', '\n', 'u', 'x', '0'...'9' => {
                        continue :state .literal_string;
                    },
                    'z' => continue :state .literal_string_whitespace,
                    else => continue :state .invalid,
                }
            },
            .literal_string_whitespace => {
                switch (self.buffer[self.index + 1]) {
                    ' ', '\t', '\r', '\n', 0xb, 0xc => {
                        self.index += 1;
                        continue :state .literal_string_whitespace;
                    },
                    else => continue :state .literal_string,
                }
            },
            .comment => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => continue :state .invalid,
                    '[' => {
                        result.tag = .comment;
                        continue :state .@"[";
                    },
                    else => {
                        result.loc.start = self.index;
                        continue :state .comment_body;
                    },
                }
            },
            .comment_body => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    0 => continue :state .invalid,
                    '\r', '\n' => result.tag = .comment,
                    else => continue :state .comment_body,
                }
            },
            .int => switch (self.buffer[self.index]) {
                '.' => continue :state .int_period,
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .int;
                },
                'e', 'E', 'p', 'P' => {
                    continue :state .int_exponent;
                },
                else => {},
            },
            .int_exponent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-', '+' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    else => continue :state .int,
                }
            },
            .int_period => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    'e', 'E', 'p', 'P' => {
                        continue :state .float_exponent;
                    },
                    else => self.index -= 1,
                }
            },
            .float => switch (self.buffer[self.index]) {
                '_', 'a'...'d', 'f'...'o', 'q'...'z', 'A'...'D', 'F'...'O', 'Q'...'Z', '0'...'9' => {
                    self.index += 1;
                    continue :state .float;
                },
                'e', 'E', 'p', 'P' => {
                    continue :state .float_exponent;
                },
                else => {},
            },
            .float_exponent => {
                self.index += 1;
                switch (self.buffer[self.index]) {
                    '-', '+' => {
                        self.index += 1;
                        continue :state .float;
                    },
                    else => continue :state .float,
                }
            },
        }
        result.loc.end = self.index;
        return result;
    }
};

test "fuzz example" {
    const Context = struct {
        fn testOne(context: @This(), input: []const u8) anyerror!void {
            _ = context;
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(Context{}, Context.testOne, .{});
}

test "operators" {
    try testTokenize(
        \\+     -     *     /     %     ^     #
        \\&     ~     |     <<    >>    //
        \\==    ~=    <=    >=    <     >     =
        \\(     )     {     }     [     ]     ::
        \\;     :     ,     .     ..    ...
    , &.{
        .@"+",
        .@"-",
        .@"*",
        .@"/",
        .@"%",
        .@"^",
        .@"#",
        .@"&",
        .@"~",
        .@"|",
        .@"<<",
        .@">>",
        .@"//",
        .@"==",
        .@"~=",
        .@"<=",
        .@">=",
        .@"<",
        .@">",
        .@"=",
        .@"(",
        .@")",
        .@"{",
        .@"}",
        .@"[",
        .@"]",
        .@"::",
        .@";",
        .@":",
        .@",",
        .@".",
        .@"..",
        .@"...",
    });
}

test "keywords" {
    try testTokenize(
        \\and       break     do        else      elseif    end
        \\false     for       function  goto      if        in
        \\local     nil       not       or        repeat    return
        \\then      true      until     while
    , &.{
        .keyword_and,
        .keyword_break,
        .keyword_do,
        .keyword_else,
        .keyword_elseif,
        .keyword_end,
        .keyword_false,
        .keyword_for,
        .keyword_function,
        .keyword_goto,
        .keyword_if,
        .keyword_in,
        .keyword_local,
        .keyword_nil,
        .keyword_not,
        .keyword_or,
        .keyword_repeat,
        .keyword_return,
        .keyword_then,
        .keyword_true,
        .keyword_until,
        .keyword_while,
    });
}

test "strings" {
    try testTokenize(
        \\a = 'alo\n123"'
    , &.{ .identifier, .@"=", .literal_string });

    try testTokenize(
        \\a = "alo\n123\""
    , &.{ .identifier, .@"=", .literal_string });

    try testTokenize(
        \\a = '\97lo\10\04923"'
    , &.{ .identifier, .@"=", .literal_string });

    try testTokenize(
        \\a = [[alo
        \\123"]]
    , &.{ .identifier, .@"=", .literal_string });

    try testTokenize(
        \\a = [==[
        \\alo
        \\123"]==]
    , &.{ .identifier, .@"=", .literal_string });
}

test "coroutines" {
    try testTokenize(
        \\function foo (a)
        \\    print("foo", a)
        \\    return coroutine.yield(2*a)
        \\end
        \\
        \\co = coroutine.create(function (a,b)
        \\    print("co-body", a, b)
        \\    local r = foo(a+1)
        \\    print("co-body", r)
        \\    local r, s = coroutine.yield(a+b, a-b)
        \\    print("co-body", r, s)
        \\    return b, "end"
        \\end)
        \\
        \\print("main", coroutine.resume(co, 1, 10))
        \\print("main", coroutine.resume(co, "r"))
        \\print("main", coroutine.resume(co, "x", "y"))
        \\print("main", coroutine.resume(co, "x", "y"))
    , &.{
        .keyword_function, .identifier,     .@"(",             .identifier,     .@")",
        .identifier,       .@"(",           .literal_string,   .@",",           .identifier,
        .@")",             .keyword_return, .identifier,       .@".",           .identifier,
        .@"(",             .literal_number, .@"*",             .identifier,     .@")",
        .keyword_end,      .identifier,     .@"=",             .identifier,     .@".",
        .identifier,       .@"(",           .keyword_function, .@"(",           .identifier,
        .@",",             .identifier,     .@")",             .identifier,     .@"(",
        .literal_string,   .@",",           .identifier,       .@",",           .identifier,
        .@")",             .keyword_local,  .identifier,       .@"=",           .identifier,
        .@"(",             .identifier,     .@"+",             .literal_number, .@")",
        .identifier,       .@"(",           .literal_string,   .@",",           .identifier,
        .@")",             .keyword_local,  .identifier,       .@",",           .identifier,
        .@"=",             .identifier,     .@".",             .identifier,     .@"(",
        .identifier,       .@"+",           .identifier,       .@",",           .identifier,
        .@"-",             .identifier,     .@")",             .identifier,     .@"(",
        .literal_string,   .@",",           .identifier,       .@",",           .identifier,
        .@")",             .keyword_return, .identifier,       .@",",           .literal_string,
        .keyword_end,      .@")",           .identifier,       .@"(",           .literal_string,
        .@",",             .identifier,     .@".",             .identifier,     .@"(",
        .identifier,       .@",",           .literal_number,   .@",",           .literal_number,
        .@")",             .@")",           .identifier,       .@"(",           .literal_string,
        .@",",             .identifier,     .@".",             .identifier,     .@"(",
        .identifier,       .@",",           .literal_string,   .@")",           .@")",
        .identifier,       .@"(",           .literal_string,   .@",",           .identifier,
        .@".",             .identifier,     .@"(",             .identifier,     .@",",
        .literal_string,   .@",",           .literal_string,   .@")",           .@")",
        .identifier,       .@"(",           .literal_string,   .@",",           .identifier,
        .@".",             .identifier,     .@"(",             .identifier,     .@",",
        .literal_string,   .@",",           .literal_string,   .@")",           .@")",
    });
}

fn testTokenize(source: [:0]const u8, expected_tags: []const Token.Tag) !void {
    var tokenizer = Lexer.init(source);
    std.debug.print("source: \"{s}\"\n", .{source});
    defer std.debug.print("\n", .{});

    for (expected_tags) |expected_tag| {
        const token = tokenizer.next();
        std.debug.print("{s}: {s}\n", .{
            @tagName(token.tag),
            source[token.loc.start..token.loc.end],
        });
        try std.testing.expectEqual(expected_tag, token.tag);
    }
    const last_token = tokenizer.next();
    try std.testing.expectEqual(Token.Tag.eof, last_token.tag);
    try std.testing.expectEqual(source.len, last_token.loc.start);
    try std.testing.expectEqual(source.len, last_token.loc.end);
}

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
