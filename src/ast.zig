const std = @import("std");
const meta = std.meta;
const assert = std.debug.assert;

// TODO: Should also contain the token for further error reporting could use
// just `start_idx: u32` and retokenize the `tag` and `end_idx`
pub const Node = struct {
    tag: Tag,
    lhs: u32,
    rhs: u32,

    const Tag = enum(u8) {
        assignment,
        @"break",
        label,
        goto,
        block,
        repeat_loop,
        while_loop,
        if_block,
        if_else,
        for_loop,
        for_loop_step,
        for_each,
        func_decl,
        func_decl_variadic,
        func_decl_member,
        func_decl_member_variadic,
        func_local,
        func_local_variadic,
        func_anonym,
        func_anonym_variadic,
        local_decl,
        local_decl_init,

        identifier,
        literal_nil,
        literal_true,
        literal_false,
        literal_ellipsis,
        literal_int,
        literal_float,
        literal_string,
        table_contructor,
        func_call,
        func_call_member,

        logic_not,
        bit_not,
        negate,
        length,

        logic_or,
        logic_and,
        less_than,
        greater_than,
        less_equal,
        greater_equal,
        not_equal,
        equal,
        bit_or,
        bit_and,
        l_bit_shift,
        r_bit_shift,
        str_concat,
        add,
        sub,
        mul,
        div,
        int_div,
        modulo,
        exponent,
        dot_access,
        index_access,
    };
};

pub const NodeID = enum(u32) {
    root = 0,
    none = 0xffff_ffff,
    _,
};

pub const UnaryOp = enum(u8) {
    logic_not = @intFromEnum(Node.Tag.logic_not),
    bit_not = @intFromEnum(Node.Tag.bit_not),
    negate = @intFromEnum(Node.Tag.negate),
    length = @intFromEnum(Node.Tag.length),
};

pub const BinaryOp = enum(u8) {
    logic_or = @intFromEnum(Node.Tag.logic_or),
    logic_and = @intFromEnum(Node.Tag.logic_and),
    equal = @intFromEnum(Node.Tag.equal),
    not_equal = @intFromEnum(Node.Tag.not_equal),
    less_than = @intFromEnum(Node.Tag.less_than),
    greater_than = @intFromEnum(Node.Tag.greater_than),
    less_equal = @intFromEnum(Node.Tag.less_equal),
    greater_equal = @intFromEnum(Node.Tag.greater_equal),
    bit_or = @intFromEnum(Node.Tag.bit_or),
    bit_and = @intFromEnum(Node.Tag.bit_and),
    l_bit_shift = @intFromEnum(Node.Tag.l_bit_shift),
    r_bit_shift = @intFromEnum(Node.Tag.r_bit_shift),
    str_concat = @intFromEnum(Node.Tag.str_concat),
    add = @intFromEnum(Node.Tag.add),
    sub = @intFromEnum(Node.Tag.sub),
    mul = @intFromEnum(Node.Tag.mul),
    div = @intFromEnum(Node.Tag.div),
    int_div = @intFromEnum(Node.Tag.int_div),
    modulo = @intFromEnum(Node.Tag.modulo),
    exponent = @intFromEnum(Node.Tag.exponent),
    dot_access = @intFromEnum(Node.Tag.dot_access),
    index_access = @intFromEnum(Node.Tag.index_access),
};

pub const Slice = struct {
    start: u32,
    len: u32,

    pub const empty = Slice{
        .start = undefined,
        .len = 0,
    };
};

pub const Conditional = struct {
    cond: NodeID,
    block: NodeID,
};

pub const Field = struct {
    key: ?NodeID = null,
    value: NodeID,
};

// TODO: Add missing variants
pub const Key = union(enum) {
    asignment: struct {
        names: []const NodeID,
        values: []const NodeID,
    },
    @"break",
    label: Slice,
    goto: Slice,
    block: struct {
        statements: []const NodeID,
        return_list: []const NodeID,
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
        start: NodeID,
        step: ?NodeID = null,
        end: NodeID,
        block: NodeID,
    },
    for_each: struct {
        names: []const Slice,
        values: []const NodeID,
        block: NodeID,
    },
    func_decl: struct {
        names: []const Slice,
        member: ?Slice = null, // TODO: change everywhere else
        params: []const Slice,
        variadic: bool,
        block: NodeID,
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
        names: []const Slice,
        values: []const NodeID,
    },
    identifier: Slice,
    literal_nil,
    literal_true,
    literal_false,
    literal_ellipsis,
    literal_int: i64,
    literal_float: f64,
    literal_string: Slice,
    table_constructor: struct {
        fields: []const Field,
    },
    func_call: struct {
        object: NodeID,
        member: ?Slice = null,
        args: []const NodeID,
    },
    unary_op: struct {
        op: UnaryOp,
        exp: NodeID,
    },
    binary_op: struct {
        op: BinaryOp,
        lhs: NodeID,
        rhs: NodeID,
    },
};

// TODO: Do the missing nodes
pub fn encodeKey(
    alloc: std.mem.Allocator,
    node: Key,
    nodes: *std.ArrayListUnmanaged(Node),
    extra: *std.ArrayListUnmanaged(u32),
) !NodeID {
    switch (node) {
        .asignment => |data| {
            try nodes.append(alloc, .{
                .tag = .assignment,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.names.len),
            });
            try extra.append(alloc, @intCast(data.values.len));
            try extra.appendSlice(alloc, @ptrCast(data.names));
            try extra.appendSlice(alloc, @ptrCast(data.values));
        },
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
            try extra.append(alloc, @intCast(data.return_list.len));
            try extra.appendSlice(alloc, @ptrCast(data.statements));
            try extra.appendSlice(alloc, @ptrCast(data.return_list));
        },
        .repeat_loop => |data| {
            try nodes.append(alloc, .{
                .tag = .repeat_loop,
                .lhs = @intFromEnum(data.cond),
                .rhs = @intFromEnum(data.block),
            });
        },
        .while_loop => |data| {
            try nodes.append(alloc, .{
                .tag = .repeat_loop,
                .lhs = @intFromEnum(data.cond),
                .rhs = @intFromEnum(data.block),
            });
        },
        .if_block => |data| {
            try nodes.append(alloc, .{
                .tag = .if_block,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.branches.len),
            });
            try extra.appendSlice(alloc, @ptrCast(@alignCast(data.branches)));
        },
        .if_else => |data| {
            try nodes.append(alloc, .{
                .tag = .if_else,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.branches.len),
            });
            try extra.append(alloc, @intFromEnum(data.else_block));
            try extra.appendSlice(alloc, @ptrCast(data.branches));
        },
        .for_loop => |data| {
            try nodes.append(alloc, .{
                .tag = .for_loop,
                .lhs = @intCast(extra.items.len),
                .rhs = @intFromEnum(data.block),
            });
            try extra.append(alloc, data.name.start);
            try extra.append(alloc, data.name.len);
            try extra.append(alloc, @intFromEnum(data.start));
            if (data.step) |step| {
                nodes.items[nodes.items.len - 1].tag = .for_loop_step;
                try extra.append(alloc, @intFromEnum(step));
            }
            try extra.append(alloc, @intFromEnum(data.end));
        },
        .for_each => |data| {
            try nodes.append(alloc, .{
                .tag = .for_each,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.names.len),
            });
            try extra.append(alloc, @intCast(data.values.len));
            try extra.append(alloc, @intFromEnum(data.block));
            try extra.appendSlice(alloc, @ptrCast(data.names));
            try extra.appendSlice(alloc, @ptrCast(data.values));
        },
        .func_decl => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.member != null) {
                    true => if (data.variadic) .func_decl_member_variadic else .func_decl_member,
                    false => if (data.variadic) .func_decl_variadic else .func_decl,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.names.len),
            });
            try extra.append(alloc, @intCast(data.params.len));
            if (data.member) |member| {
                try extra.append(alloc, member.start);
                try extra.append(alloc, member.len);
            }
            try extra.append(alloc, @intFromEnum(data.block));
            try extra.appendSlice(alloc, @ptrCast(data.names));
            try extra.appendSlice(alloc, @ptrCast(data.params));
        },
        .func_local => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.variadic) {
                    true => .func_local_variadic,
                    false => .func_local,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = @intFromEnum(data.block),
            });
            try extra.append(alloc, @intCast(data.name.start));
            try extra.append(alloc, @intCast(data.name.len));
            try extra.append(alloc, @intCast(data.params.len));
            try extra.appendSlice(alloc, @ptrCast(data.params));
        },
        .func_anonym => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.variadic) {
                    true => .func_anonym_variadic,
                    false => .func_anonym,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = @intFromEnum(data.block),
            });
            try extra.append(alloc, @intCast(data.params.len));
            try extra.appendSlice(alloc, @ptrCast(data.params));
        },
        .local_decl => |data| {
            try nodes.append(alloc, .{
                .tag = .local_decl,
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.names.len),
            });
            try extra.append(alloc, @intCast(data.values.len));
            try extra.appendSlice(alloc, @ptrCast(data.names));
            try extra.appendSlice(alloc, @ptrCast(data.values));
        },
        .identifier => {},
        .literal_nil => {},
        .literal_true => {},
        .literal_false => {},
        .literal_ellipsis => {},
        .literal_int => {},
        .literal_float => {},
        .literal_string => {},
        .table_constructor => {},
        .func_call => |data| {
            try nodes.append(alloc, .{
                .tag = switch (data.member == null) {
                    true => .func_call_member,
                    false => .func_call,
                },
                .lhs = @intCast(extra.items.len),
                .rhs = @intCast(data.args.len),
            });
            try extra.appendSlice(alloc, @ptrCast(data.args));
        },
        .unary_op => {},
        .binary_op => {},
    }
    return @enumFromInt(nodes.items.len - 1);
}

// const Decoder = struct {
//     idx: u32,
//     buffer: []const u32,
//
//     fn init(buffer: []const u32, start_idx: u32) Decoder {
//         return Decoder{ .idx = start_idx, .buffer = buffer };
//     }
//
//     fn decode(self: *Decoder, comptime T: type, count: u32) []const T {
//         std.debug.assert(@alignOf(T) <= @alignOf(u32));
//
//         const len = count * @sizeOf(T) / @sizeOf(u32);
//         defer self.idx += len;
//         return @ptrCast(@alignCast(self.buffer[self.idx .. self.idx + len]));
//     }
//
//     fn decodeOne(self: *Decoder) u32 {
//         defer self.idx += 1;
//         return self.buffer[self.idx];
//     }
// };

// TODO: Do the missing nodes
pub fn decodeKey(
    node_id: NodeID,
    nodes: []const Node,
    extra: []const u32,
) Key {
    const node = nodes[@intFromEnum(node_id)];

    switch (node.tag) {
        .assignment => {
            // unreachable;
            return .@"break";
        },
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
            const stat_len = node.rhs;
            const ret_len = d.decodeOne();

            var res: @FieldType(Key, "block") = undefined;
            res.statements = d.decode(NodeID, stat_len);
            res.return_list = d.decode(NodeID, ret_len);
            return Key{ .block = res };
        },
        .repeat_loop => {
            return Key{ .repeat_loop = .{
                .cond = @enumFromInt(node.lhs),
                .block = @enumFromInt(node.rhs),
            } };
        },
        .while_loop => {
            return Key{ .while_loop = .{
                .cond = @enumFromInt(node.lhs),
                .block = @enumFromInt(node.rhs),
            } };
        },
        .if_block => {
            var res: @FieldType(Key, "if_block") = undefined;
            var d = Decoder.init(extra, node.lhs);

            const branches_len = node.rhs;
            res.branches = d.decode(Conditional, branches_len);
            return Key{ .if_block = res };
        },
        .if_else => {
            var res: @FieldType(Key, "if_else") = undefined;
            var d = Decoder.init(extra, node.lhs);

            const branches_len = node.rhs;
            res.else_block = @enumFromInt(d.decodeOne());
            res.branches = d.decode(Conditional, branches_len);
            return Key{ .if_else = res };
        },
        .for_loop => {
            var res: @FieldType(Key, "for_loop") = undefined;
            var d = Decoder.init(extra, node.lhs);

            res.block = @enumFromInt(node.rhs);
            res.name.start = d.decodeOne();
            res.name.len = d.decodeOne();
            res.start = @enumFromInt(d.decodeOne());
            res.step = null;
            res.end = @enumFromInt(d.decodeOne());
            return Key{ .for_loop = res };
        },
        .for_loop_step => {
            var res: @FieldType(Key, "for_loop") = undefined;
            var d = Decoder.init(extra, node.lhs);

            res.block = @enumFromInt(node.rhs);
            res.name.start = d.decodeOne();
            res.name.len = d.decodeOne();
            res.start = @enumFromInt(d.decodeOne());
            res.step = @enumFromInt(d.decodeOne());
            res.end = @enumFromInt(d.decodeOne());
            return Key{ .for_loop = res };
        },
        .for_each => {
            var res: @FieldType(Key, "for_each") = undefined;
            var d = Decoder.init(extra, node.lhs);

            const names_len = node.rhs;
            const values_len = d.decodeOne();
            res.block = @enumFromInt(d.decodeOne());
            res.names = d.decode(Slice, names_len);
            res.values = d.decode(NodeID, values_len);
            return Key{ .for_each = res };
        },
        .func_decl,
        .func_decl_variadic,
        .func_decl_member,
        .func_decl_member_variadic,
        => {
            const member = node.tag == .func_decl_member or node.tag == .func_decl_member_variadic;
            const variadic = node.tag == .func_decl_variadic or node.tag == .func_decl_member_variadic;
            return Key{
                .func_decl = decodePayload(.func_decl, node, extra, .{
                    .member = member,
                    .variadic = variadic,
                }),
            };

            // var res: @FieldType(Key, "func_decl") = undefined;
            // var d = Decoder.init(extra, node.lhs);
            //
            // const names_len = node.rhs;
            // const params_len = d.decodeOne();
            // if (node.tag == .func_decl_member or node.tag == .func_decl_member_variadic) {
            //     const start = d.decodeOne();
            //     const len = d.decodeOne();
            //     res.member = .{ .start = start, .len = len };
            // }
            // res.variadic = node.tag == .func_decl_variadic or node.tag == .func_decl_member_variadic;
            // res.block = @enumFromInt(d.decodeOne());
            // res.names = d.decode(Slice, names_len);
            // res.params = d.decode(Slice, params_len);
            // return Key{ .func_decl = res };
        },
        .func_local,
        .func_local_variadic,
        .func_anonym,
        .func_anonym_variadic,
        .local_decl,
        .local_decl_init,

        .identifier,
        .literal_nil,
        .literal_true,
        .literal_false,
        .literal_ellipsis,
        .literal_int,
        .literal_float,
        .literal_string,
        .table_contructor,
        .func_call,
        .func_call_member,

        .logic_not,
        .bit_not,
        .negate,
        .length,

        .logic_or,
        .logic_and,
        .less_than,
        .greater_than,
        .less_equal,
        .greater_equal,
        .not_equal,
        .equal,
        .bit_or,
        .bit_and,
        .l_bit_shift,
        .r_bit_shift,
        .str_concat,
        .add,
        .sub,
        .mul,
        .div,
        .int_div,
        .modulo,
        .exponent,
        .dot_access,
        .index_access,
        => return .@"break",
    }
}

fn Decoder(comptime U: type) type {
    return struct {
        node: Node,
        extra: []const u32,
        index: u32 = 0,
        uses_extra: bool = false,

        pub fn init(node: Node, extra: []const u32, sizeof: u32) Decoder {
            const uses_extra = (sizeof > @sizeOf(u32) * 2);
            return Decoder{
                .node = node,
                .extra = extra,
                .index = if (uses_extra) 1 else 0,
                .uses_extra = uses_extra,
            };
        }

        pub fn get(self: *Decoder, comptime T: type, comptime name: [:0]const u8) T {
            const info = @typeInfo(T);
            switch (info) {
                .bool => {
                    return @field(U, name);
                },
                .optional => |opt| {
                    return if (@field(U, name)) self.get(opt.child) else null;
                },
                .@"enum" => |e| {
                    assert(@sizeOf(e.tag_type) == @sizeOf(u32));
                    const foo = self.get(u32);
                    return @enumFromInt(foo);
                },
                .pointer => |ptr| if (ptr.size == .slice) {
                    assert(self.index > 0);
                    const len = self.get(u32);
                    const slice = self.extra[self.index..(self.index + len)];
                    self.index += len;
                    return slice;
                } else {
                    @compileError("Unsuported pointer type: " ++ @typeName(T));
                },
                else => {
                    //
                },
            }
        }
    };
}

fn decodePayload(
    comptime tag: meta.Tag(Key),
    node: Node,
    extra: []const u32,
    args: anytype,
) meta.TagPayload(Key, tag) {
    const T = meta.TagPayload(Key, tag);
    const U = @TypeOf(args);

    comptime var sizeof: u32 = 0;
    inline for (meta.fields(T)) |field| {
        const info = @typeInfo(field.type);
        sizeof += switch (info) {
            .bool => 0,
            .optional => |opt| if (@field(U, field.name)) @sizeOf(opt.child) else 0,
            else => @sizeOf(field.type),
        };
    }

    var res: T = undefined;
    comptime var d = Decoder(U).init(node, extra, sizeof);

    inline for (meta.fields(T)) |field| {
        @field(res, field.name) = d.get(field.type);
    }
    return res;
}

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
