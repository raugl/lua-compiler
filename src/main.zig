const std = @import("std");
const lx = @import("lexer.zig");

pub fn main() !void {
    // const source = "hello";
    // var tokens = lx.Tokenizer.init(source);
    // while (true) {
    //     const token = tokens.next();
    //     std.debug.print("{s}: {s}\n", .{ @tagName(token.tag), source[token.loc.start..token.loc.end] });
    //     if (token.tag == .eof) break;
    // }
}

// comptime {
//     std.testing.refAllDeclsRecursive(@This());
// }
