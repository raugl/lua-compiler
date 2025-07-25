const std = @import("std");
const ast = @import("ast.zig");
const parser = @import("parser.zig");

pub fn main() !void {
    const gpa = std.heap.smp_allocator; // TODO: Read about this
    var arena = std.heap.ArenaAllocator.init(gpa);
    const scratch = arena.allocator();
    defer arena.deinit();

    // const source =
    //     \\ -- Bootstrap lazy.nvim
    //     \\ local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
    //     \\ if not (vim.uv or vim.loop).fs_stat(lazypath) then
    //     \\   local lazyrepo = 'https://github.com/folke/lazy.nvim.git'
    //     \\   local out = vim.fn.system { 'git', 'clone', '--filter=blob:none', '--branch=stable', lazyrepo, lazypath }
    //     \\   if vim.v.shell_error ~= 0 then
    //     \\     vim.api.nvim_echo({
    //     \\       { 'Failed to clone lazy.nvim:\n', 'ErrorMsg' },
    //     \\       { out,                            'WarningMsg' },
    //     \\       { '\nPress any key to exit...' },
    //     \\     }, true, {})
    //     \\     vim.fn.getchar()
    //     \\     os.exit(1)
    //     \\   end
    //     \\ end
    //     \\ vim.opt.rtp:prepend(lazypath)
    //     \\
    //     \\ require 'options'
    //     \\ require 'keymaps'
    //     \\ require 'autocmds'
    //     \\
    //     \\ require('lazy').setup {
    //     \\   spec = {
    //     \\     { import = 'plugins' }
    //     \\   },
    //     \\   ui = {
    //     \\     border = 'single',
    //     \\     backdrop = 100,
    //     \\   },
    //     \\   change_detection = { notify = false },
    //     \\ }
    // ;

    const result = try parser.parseText(gpa,
        \\ if not (vim.uv or vim.loop).fs_stat(lazypath) then end
    );
    defer result.deinit(gpa);

    const out = std.io.getStdOut().writer().any();
    try result.print(out, .root);
}

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
