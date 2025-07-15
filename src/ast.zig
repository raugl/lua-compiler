const std = @import("std");

pub const NodeID = enum(u32) {
    none = 0xffff_ffff,
    _,
};

pub const Node = struct {
    tag: Tag,
    lhs: u32,
    rhs: u32,

    const Tag = enum {
        @"break",
        label,
        goto,
        block,
        repeat_loop,
        while_loop,
        if_block,
        if_else,
        for_loop,
        for_each,
        func_decl,
        func_local,
        func_anonym,
        local_decl,
        func_call,
        asignment,

        logic_or,
        logic_not,
        logic_and,
        less_than,
        greater_than,
        less_or_equal,
        greater_or_equal,
        not_equal,
        equal,
        bit_or,
        bit_not,
        bit_and,
        bit_shift_left,
        bit_shift_right,
        str_concat,
        add,
        sub,
        mul,
        div,
        int_div,
        modulo,
        length,
        negate,
        exponent,
        identifier,
        literal_nil,
        literal_true,
        literal_false,
        literal_int,
        literal_float,
        literal_string,
    };
};

pub const Slice = struct {
    start: u32,
    len: u32,
};

pub const Conditional = struct {
    cond: NodeID,
    block: NodeID,
};

pub const Key = union(enum) {
    @"break",
    label: Slice,
    goto: Slice,
    block: struct {
        statements: []const NodeID,
        ret_vals: []const NodeID,
    },
    repeat_loop: Conditional,
    while_loop: Conditional,
    if_block: struct {
        branches: []const Conditional,
    },
    if_else: struct {
        branches: []const Conditional,
        else_block: NodeID,
    },
    for_loop: struct {
        name: Slice,
        start_exp: NodeID,
        step_exp: ?NodeID,
        end_exp: NodeID,
        block: NodeID,
    },
    for_each: struct {
        names: []const Slice,
        values: []const NodeID,
        block: NodeID,
    },
    func_decl: struct {
        name: []const Slice,
        params: []const Slice,
        block: NodeID,
        member: bool,
        variadic: bool,
    },
    func_local: struct {
        name: Slice,
        params: []const Slice,
        block: NodeID,
        variadic: bool,
    },
    func_anonym: struct {
        params: []const Slice,
        block: NodeID,
        variadic: bool,
    },
    local_decl: struct {
        names: []const NodeID,
        values: []const NodeID,
    },
    func_call: struct {
        func: NodeID,
        args: []const NodeID,
        member: bool,
    },
    asignment: struct {
        vars: []const NodeID,
        values: []const NodeID,
    },
};

pub fn encodeKey(
    alloc: std.mem.Allocator,
    nodes: *std.ArrayListUnmanaged(Node),
    extra: *std.ArrayListUnmanaged(u32),
    node: Key,
) !NodeID {
    switch (node) {
        .@"break" => {
            // PERF: This could be encoded into the NodeID, like a small integer
            try nodes.append(alloc, .{ .tag = .@"break", .lhs = undefined, .rhs = undefined });
        },
        .label => |data| {
            try nodes.append(alloc, .{
                .tag = .label,
                .lhs = data.start,
                .rhs = data.len,
            });
        },
        .goto => |data| {
            try nodes.append(alloc, .{
                .tag = .goto,
                .lhs = data.start,
                .rhs = data.len,
            });
        },
        .block => |data| {
            try nodes.append(alloc, .{
                .tag = .block,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.statements.len),
            });
            try extra.append(alloc, @intCast(data.ret_vals.len));
            try extra.appendSlice(alloc, data.statements);
            try extra.appendSlice(alloc, data.ret_vals);
        },
        .repeat_loop => |data| {
            try node.append(alloc, .{
                .tag = .repeat_loop,
                .lhs = data.cond,
                .rhs = data.block,
            });
        },
        .while_loop => |data| {
            try node.append(alloc, .{
                .tag = .repeat_loop,
                .lhs = data.cond,
                .rhs = data.block,
            });
        },
        .if_block => |data| {
            try node.append(alloc, .{
                .tag = .if_block,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.branches.len),
            });
            try extra.appendSlice(alloc, @ptrCast(@alignCast(data.branches)));
        },
        .if_else => |data| {
            try node.append(alloc, .{
                .tag = .if_else,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.branches.len),
            });
            try extra.append(data.else_block);
            try extra.appendSlice(alloc, @ptrCast(@alignCast(data.branches)));
        },
        .for_loop => |data| {
            try node.append(alloc, .{
                .tag = .for_loop,
                .lhs = @intCast(extra.items.len),
                .rhs = data.block,
            });
            try extra.append(alloc, data.name.start);
            try extra.append(alloc, data.name.len);
            try extra.append(alloc, data.start_exp);
            if (data.step_exp) |step| try extra.append(alloc, step);
            try extra.append(alloc, data.end_exp);
        },
        .for_each => |data| {
            try nodes.append(alloc, .{
                .tag = .for_each,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.names.len),
            });
            try extra.append(alloc, @intCast(data.values.len));
            try extra.append(alloc, data.block);
            try extra.append(alloc, @ptrCast(@alignCast(data.names)));
            try extra.append(alloc, @ptrCast(@alignCast(data.values)));
        },
        .func_decl => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.member) {
                    true => if (data.variadic) .func_decl_member_variadic else .func_decl_memeber,
                    false => if (data.variadic) .func_decl_variadic else .func_decl,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.name.len),
            });
            try extra.append(alloc, @intCast(data.params.len));
            try extra.append(alloc, data.block);
            try extra.append(alloc, @ptrCast(@alignCast(data.name)));
            try extra.append(alloc, @ptrCast(@alignCast(data.params)));
        },
        .func_local => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.variadic) {
                    true => .func_local_variadic,
                    false => .func_local,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = data.block,
            });
            try extra.append(alloc, @intCast(data.name.len));
            try extra.append(alloc, @intCast(data.params.len));
            try extra.append(alloc, @ptrCast(@alignCast(data.name)));
            try extra.append(alloc, @ptrCast(@alignCast(data.params)));
        },
        .func_anonym => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.variadic) {
                    true => .func_anonym_variadic,
                    false => .func_anonym,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = data.block,
            });
            try extra.append(alloc, @intCast(data.params.len));
            try extra.append(alloc, @ptrCast(@alignCast(data.params)));
        },
        .local_decl => |data| {
            try nodes.append(alloc, .{
                .tag = .local_decl,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.names.len),
            });
            try extra.append(alloc, @intCast(data.values.len));
            try extra.append(alloc, data.names);
            try extra.append(alloc, data.values);
        },
        .func_call => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.member) {
                    true => .func_call_member,
                    false => .func_call,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.args.len),
            });
            try extra.appendSlice(alloc, data.args);
        },
        .asignment => |data| {
            try nodes.append(alloc, .{
                .tag = .assignment,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.vars.len),
            });
            try extra.append(alloc, @intCast(data.values.len));
            try extra.append(alloc, data.vars);
            try extra.append(alloc, data.values);
        },
    }
    return @intCast(nodes.items.len - 1);
}

const Decoder = struct {
    idx: u32,
    buffer: []const u32,

    fn init(buffer: []const u32, start_idx: u32) Decoder {
        return Decoder{ .idx = start_idx, .buffer = buffer };
    }

    fn decode(self: *Decoder, comptime T: type, count: u32) []const T {
        std.debug.assert(@alignOf(T) <= @alignOf(u32));

        const len = count * @sizeOf(T) / @sizeOf(u32);
        defer self.idx += len;
        return @ptrCast(@alignCast(self.buffer[self.idx .. self.idx + len]));
    }

    fn decodeOne(self: *Decoder) u32 {
        defer self.idx += 1;
        return @ptrCast(@alignCast(self.buffer[self.idx .. self.idx + 1]));
    }
};

pub fn decodeKey(
    nodes: []const Node,
    extra: []const u32,
    node_id: NodeID,
) Key {
    const node = nodes[node_id]; // TODO: Distinct type for NodeID

    switch (node.tag) {
        .@"break" => {
            return .@"break";
        },
        .label => {
            return Key{ .label = .{
                .start = node.lhs,
                .len = node.rhs,
            } };
        },
        .goto => {
            return Key{ .goto = .{
                .start = node.lhs,
                .len = node.rhs,
            } };
        },
        .block => {
            var d = Decoder.init(extra, node.lhs);
            const statements_len = node.rsh;
            const ret_vals_len = d.decode(u32, 1);

            var res: @FieldType(Key, "block") = undefined;
            res.statements = d.decode(NodeID, statements_len);
            res.ret_vals = d.decode(NodeID, ret_vals_len);
            return Key{ .block = res };
        },
        .repeat_loop => {
            return Key{ .repeat_loop = .{
                .cond = node.lhs,
                .block = node.rsh,
            } };
        },
        .while_loop => {
            return Key{ .while_loop = .{
                .cond = node.lhs,
                .block = node.rsh,
            } };
        },
        .if_block => {
            var res: @FieldType(Key, "if_block") = undefined;
            var d = Decoder.init(extra, node.lhs);

            const branches_len = node.rsh;
            res.branches = d.decode(NodeID, branches_len);
            return Key{ .if_block = res };
        },
        .if_else => {
            var res: @FieldType(Key, "if_else") = undefined;
            var d = Decoder.init(extra, node.lhs);

            const branches_len = node.rhs;
            res.else_block = d.decodeOne();
            res.branches = d.decode(NodeID, branches_len);
            return Key{ .if_else = res };
        },
        .for_loop => {
            var res: @FieldType(Key, "for_loop") = undefined;
            var d = Decoder.init(extra, node.lhs);

            res.block = node.rhs;
            res.name.start = d.decodeOne();
            res.name.len = d.decodeOne();
            res.start_exp = d.decodeOne();
            res.step_exp = null;
            res.end_exp = d.decodeOne();
            return Key{ .for_loop = res };
        },
        .for_loop_step => {
            var res: @FieldType(Key, "for_loop") = undefined;
            var d = Decoder.init(extra, node.lhs);

            res.block = node.rhs;
            res.name.start = d.decodeOne();
            res.name.len = d.decodeOne();
            res.start_exp = d.decodeOne();
            res.step_exp = d.decodeOne();
            res.end_exp = d.decodeOne();
            return Key{ .for_loop = res };
        },
        .for_each => {
            var res: @FieldType(Key, "for_each") = undefined;
            var d = Decoder.init(extra, node.lhs);

            const names_len = node.rhs;
            const values_len = d.decodeOne();
            res.block = d.decodeOne();
            res.names = d.decode(Slice, names_len);
            res.values = d.decode(NodeID, values_len);
            return Key{ .for_each = res };
        },
        .func_decl,
        .func_decl_variadic,
        .func_decl_member,
        .func_decl_member_variadic,
        => {
            var res: @FieldType(Key, "func_decl") = undefined;
            var d = Decoder.init(extra, node.lhs);

            res.member = node.tag == .func_decl_member or node.tag == .func_decl_member_variadic;
            res.variadic = node.tag == .func_decl_variadic or node.tag == .func_decl_member_variadic;

            const name_len = node.rhs;
            const params_len = d.decodeOne();
            res.block = d.decodeOne();
            res.name = d.decode(Slice, name_len);
            res.params = d.decode(Slice, params_len);
            return Key{ .func_decl = res };
        },
        .func_local => {},
        .func_anonym => {},
        .local_decl => {},
        .func_call => {},
        .asignment => {},
    }
}

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
