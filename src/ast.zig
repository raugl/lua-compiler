const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const assert = std.debug.assert;

pub const NodeID = enum(u32) {
    root = 0,
    none = 0xffff_ffff,
    _,
};

pub const UnaryOp = enum(u8) {
    logic_not,
    bit_not,
    negate,
    length,
};

pub const BinaryOp = enum(u8) {
    logic_or,
    logic_and,
    equal,
    not_equal,
    less_than,
    greater_than,
    less_equal,
    greater_equal,
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


// TODO: Make this enum determine the values and set the others based on it
pub const NodeTag = enum(u8) {
    for_loop,
    for_loop_step,

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

    func_call,
    func_call_member,

    assignment = @intFromEnum(KeyTag.assignment),
    @"break" = @intFromEnum(KeyTag.@"break"),
    label = @intFromEnum(KeyTag.label),
    goto = @intFromEnum(KeyTag.goto),
    block = @intFromEnum(KeyTag.block),
    repeat_loop = @intFromEnum(KeyTag.repeat_loop),
    while_loop = @intFromEnum(KeyTag.while_loop),
    if_block = @intFromEnum(KeyTag.if_block),
    if_else = @intFromEnum(KeyTag.if_else),
    for_each = @intFromEnum(KeyTag.for_each),
    identifier = @intFromEnum(KeyTag.identifier),
    literal_nil = @intFromEnum(KeyTag.literal_nil),
    literal_true = @intFromEnum(KeyTag.literal_true),
    literal_false = @intFromEnum(KeyTag.literal_false),
    literal_ellipsis = @intFromEnum(KeyTag.literal_ellipsis),
    literal_int = @intFromEnum(KeyTag.literal_int),
    literal_float = @intFromEnum(KeyTag.literal_float),
    literal_string = @intFromEnum(KeyTag.literal_string),
    table_constructor = @intFromEnum(KeyTag.table_constructor),

    logic_not = @intFromEnum(UnaryOp.logic_not),
    bit_not = @intFromEnum(UnaryOp.bit_not),
    negate = @intFromEnum(UnaryOp.negate),
    length = @intFromEnum(UnaryOp.length),

    logic_or = @intFromEnum(BinaryOp.logic_or),
    logic_and = @intFromEnum(BinaryOp.logic_and),
    equal = @intFromEnum(BinaryOp.equal),
    not_equal = @intFromEnum(BinaryOp.not_equal),
    less_than = @intFromEnum(BinaryOp.less_than),
    greater_than = @intFromEnum(BinaryOp.greater_than),
    less_equal = @intFromEnum(BinaryOp.less_equal),
    greater_equal = @intFromEnum(BinaryOp.greater_equal),
    bit_or = @intFromEnum(BinaryOp.bit_or),
    bit_and = @intFromEnum(BinaryOp.bit_and),
    l_bit_shift = @intFromEnum(BinaryOp.l_bit_shift),
    r_bit_shift = @intFromEnum(BinaryOp.r_bit_shift),
    str_concat = @intFromEnum(BinaryOp.str_concat),
    add = @intFromEnum(BinaryOp.add),
    sub = @intFromEnum(BinaryOp.sub),
    mul = @intFromEnum(BinaryOp.mul),
    div = @intFromEnum(BinaryOp.div),
    int_div = @intFromEnum(BinaryOp.int_div),
    modulo = @intFromEnum(BinaryOp.modulo),
    exponent = @intFromEnum(BinaryOp.exponent),
    dot_access = @intFromEnum(BinaryOp.dot_access),
    index_access = @intFromEnum(BinaryOp.index_access),
};

// TODO: Should also contain the token for further error reporting could use
// just `start_idx: u32` and retokenize the `tag` and `end_idx`
pub const Node = struct {
    tag: NodeTag,
    data: struct {
        lhs: u32,
        rhs: u32,
    },
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
    key: NodeID = .none,
    value: NodeID,
};

// TODO: Add missing variants
pub const Key = union(enum) {
    assignment: struct {
        names: []const NodeID,
        values: []const NodeID,
    },
    @"break": void,
    label: Slice,
    goto: Slice,
    block: struct {
        statements: []const NodeID,
        return_list: []const NodeID,
    },
    repeat_loop: Conditional,
    while_loop: Conditional,
    if_block: struct {
        // PERF: Introduce an if block without elseifs, this would fully fit in a single Node
        branches: []const Conditional,
    },
    if_else: struct {
        branches: []const Conditional,
        else_block: NodeID,
    },
    for_loop: struct {
        name: Slice,
        start: NodeID,
        step: ?NodeID,
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
        member: ?Slice, // TODO: change everywhere else
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
    literal_nil: void,
    literal_true: void,
    literal_false: void,
    literal_ellipsis: void,
    literal_int: i64,
    literal_float: f64,
    literal_string: Slice,
    table_constructor: struct {
        fields: []const Field,
    },
    func_call: struct {
        object: NodeID,
        member: ?Slice,
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

pub fn encodeKey(
    alloc: mem.Allocator,
    node: Key,
    nodes: *std.ArrayListUnmanaged(Node),
    extra: *std.ArrayListUnmanaged(u32),
) mem.Allocator.Error!NodeID {
    switch (node) {
        .@"break",
        .literal_nil,
        .literal_true,
        .literal_false,
        .literal_ellipsis,
        => {
            const tag = meta.activeTag(node);
            return @enumFromInt(@intFromEnum(tag));
        },
        .for_loop => |data| {
            const tag = if (data.step) |_| .for_loop_step else .for_loop;
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        .func_decl => |data| {
            const tag = switch (data.variadic) {
                true => if (data.member) |_| .func_decl_member_variadic else .func_decl_variadic,
                false => if (data.member) |_| .func_decl_member else .func_decl,
            };
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        .func_local => |data| {
            const tag = if (data.variadic) .func_local_variadic else .func_local;
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        .func_anonym => |data| {
            const tag = if (data.variadic) .func_anonym_variadic else .func_anonym;
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        inline .unary_op, .binary_op => |data| {
            const tag = @intFromEnum(data.op);
            return try encodeKeyImpl(alloc, nodes, extra, @enumFromInt(tag), data);
        },
        inline else => |data| {
            const tag = @tagName(meta.activeTag(node));
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
    }
}

fn encodeKeyImpl(
    alloc: mem.Allocator,
    nodes: *std.ArrayListUnmanaged(Node),
    extra: *std.ArrayListUnmanaged(u32),
    tag: NodeTag,
    data: anytype,
) mem.Allocator.Error!NodeID {
    const T = @TypeOf(data);

    var sizeof: u32 = 0;
    const use_extra = inline for (meta.fields(T)) |field| {
        const info = @typeInfo(field.type);

        sizeof += switch (info) {
            .bool => 0,
            .optional => |opt| if (@field(data, field.name)) |_| @sizeOf(opt.child) else 0,
            .pointer => |ptr| if (ptr.size == .slice) break true else @sizeOf(ptr.child),
            else => @sizeOf(field.type),
        };
    };

    const id = nodes.items.len;
    const node = try nodes.addOne(alloc);
    node.tag = tag;

    var e = Encoder.init(alloc, extra, node, use_extra or sizeof > 8);
    try e.put();
    return @enumFromInt(id);
}

const Encoder = struct {
    alloc: mem.Allocator,
    extra: *std.ArrayListUnmanaged(u32),
    node: *Node,
    index: u32,

    fn init(
        alloc: mem.Allocator,
        extra: *std.ArrayListUnmanaged(u32),
        node: *Node,
        use_extra: bool,
    ) Encoder {
        if (use_extra) node.data.lhs = @intCast(extra.len);
        return Encoder{ .alloc = alloc, .extra = extra, .node = node, .index = @intFromBool(use_extra) };
    }

    fn put(self: *Encoder, data: anytype) mem.Allocator.Error!void {
        const T = @TypeOf(data);
        const info = @typeInfo(T);

        switch (info) {
            .bool => return,
            .@"enum" => {
                if (T != NodeID) return;
                return self.putU32(@intFromEnum(data));
            },
            .optional => {
                if (data) |d| return self.put(d);
            },
            .int, .float => {
                if (self.index < 2) {
                    if (@sizeOf(T) == 8) {
                        assert(self.index == 0);
                        self.index += 2;
                        self.node.data = @bitCast(data);
                    }
                    assert(@sizeOf(T) == 4);
                    defer self.index += 1;
                    switch (self.index) {
                        0 => self.node.lhs = @bitCast(data),
                        1 => self.node.rhs = @bitCast(data),
                        else => unreachable,
                    }
                } else {
                    assert(@sizeOf(T) % 4 == 0);
                    const len = @sizeOf(T) / 4;
                    const ptr: [*]u32 = @ptrCast(@alignCast(&data));

                    try self.extra.appendSlice(self.alloc, ptr[0..len]);
                    self.index += len;
                }
            },
            .pointer => |pointer| if (pointer.size == .slice) {
                assert(self.index > 0);
                const len = data.len * @sizeOf(pointer.child) / 4;
                const ptr: [*]u32 = @ptrCast(data.ptr);

                self.putU32(@intCast(data.len));
                try self.extra.appendSlice(self.alloc, ptr[0..len]);
                self.index += len;
            } else {
                @compileError("Unencodable pointer type: " ++ @typeName(T));
            },
            .@"struct" => {
                inline for (meta.fields(T)) |field| {
                    return self.put(@field(T, field.name));
                }
            },
            else => @compileError("Unencodable type: " ++ @typeName(T)),
        }
    }

    fn putU32(self: *Encoder, data: u32) mem.Allocator.Error!void {
        try self.put(data);
    }
};

pub fn decodeKey(
    node_id: NodeID,
    nodes: []const Node,
    extra: []const u32,
) Key {
    const node = nodes[@intFromEnum(node_id)];

    return switch (node.tag) {
        .for_loop,
        .for_loop_step,
        => decodeKeyImpl("for_loop", node, extra, .{
            .step = (node.tag == .for_loop_step),
        }),
        .func_decl,
        .func_decl_variadic,
        .func_decl_member,
        .func_decl_member_variadic,
        => return decodeKeyImpl("func_decl", node, extra, .{
            .member = (node.tag == .func_decl_member or node.tag == .func_decl_member_variadic),
            .variadic = (node.tag == .func_decl_variadic or node.tag == .func_decl_member_variadic),
        }),
        .func_local,
        .func_local_variadic,
        => decodeKeyImpl("func_local", node, extra, .{
            .variadic = (node.tag == .func_local_variadic),
        }),
        .func_anonym,
        .func_anonym_variadic,
        => decodeKeyImpl("func_anonym", node, extra, .{
            .variadic = (node.tag == .func_anonym_variadic),
        }),
        .func_call,
        .func_call_member,
        => decodeKeyImpl("func_call", node, extra, .{
            .member = (node.tag == .func_call_member),
        }),
        .logic_not,
        .bit_not,
        .negate,
        .length,
        => decodeKeyImpl("unary_op", node, extra, .{
            .op = @intFromEnum(node.tag),
        }),
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
        => decodeKeyImpl("binary_op", node, extra, .{
            .op = @intFromEnum(node.tag),
        }),
        inline else => |tag| decodeKeyImpl(@tagName(tag), node, extra, .{}),
    };
}

// PERF: Encode value-less variants inside the NodeID
fn decodeKeyImpl(
    comptime tag_name: []const u8,
    node: Node,
    extra: []const u32,
    args: anytype,
) Key {
    const T = meta.TagPayloadByName(Key, tag_name);
    var sizeof: u32 = 0;
    const use_extra = inline for (meta.fields(T)) |field| {
        const info = @typeInfo(field.type);

        sizeof += switch (info) {
            .bool => 0,
            .optional => |opt| if (@field(args, field.name)) @sizeOf(opt.child) else 0,
            .pointer => |ptr| if (ptr.size == .slice) break true else @sizeOf(ptr.child),
            else => @sizeOf(field.type),
        };
    };
    var d = Decoder.init(node, extra, use_extra or sizeof > 8);
    return @unionInit(Key, tag_name, d.getImpl(undefined, T, args));
}

const Decoder = struct {
    node: Node,
    extra: []const u32,
    index: u32 = 0,

    fn init(node: Node, extra: []const u32, use_extra: bool) Decoder {
        return Decoder{
            .node = node,
            .extra = extra,
            .index = @intFromBool(use_extra),
        };
    }

    fn get(self: *Decoder, comptime field: anytype, args: anytype) field.type {
        if (@hasField(@TypeOf(args), field.name)) {
            return self.getImpl(field.name, field.type, @field(args, field.name));
        } else {
            return self.getImpl(field.name, field.type, {});
        }
    }

    fn getImpl(self: *Decoder, comptime name: []const u8, comptime T: type, args: anytype) T {
        const U = @TypeOf(args);
        const info = @typeInfo(T);
        switch (info) {
            .bool => {
                return @hasField(U, name) and @field(args, name);
            },
            .@"enum" => |e| {
                if (@sizeOf(e.tag_type) < 4) { // TODO: if its a NodeID
                    return @enumFromInt(@field(args, name));
                }
                assert(@sizeOf(e.tag_type) == 4);
                const tag = self.getImpl(undefined, u32, void);
                return @enumFromInt(tag);
            },
            .optional => |opt| {
                // TODO: if T == ?NodeID, use .none
                if (@hasField(U, name) and @field(args, name)) {
                    return self.getImpl(name, opt.child, args);
                } else {
                    return null;
                }
            },
            .int, .float => {
                if (self.index < 2) {
                    if (@sizeOf(T) == 8) {
                        assert(self.index == 0);
                        self.index += 2;
                        return @bitCast(self.node.data);
                    }
                    assert(@sizeOf(T) == 4);
                    defer self.index += 1;
                    switch (self.index) {
                        0 => return @bitCast(self.node.data.lhs),
                        1 => return @bitCast(self.node.data.rhs),
                        else => unreachable,
                    }
                } else {
                    assert(@sizeOf(T) % 4 == 0);
                    const len = @sizeOf(T) / 4;
                    const idx = self.node.data.lhs + self.index - 2;
                    self.index += len;

                    const ptr = self.extra[idx..].ptr;
                    return @as(*T, @ptrCast(@alignCast(ptr))).*;
                }
            },
            .pointer => |ptr| if (ptr.size == .slice) {
                assert(self.index > 0);
                const len = self.getImpl(undefined, u32, void);
                const idx = self.node.data.lhs + self.index - 2;
                const slice = self.extra[idx..][0..len];
                self.index += len;
                return slice;
            } else {
                @compileError("Undecodable pointer type: " ++ @typeName(T));
            },
            .@"struct" => {
                var res: T = undefined;
                inline for (meta.fields(T)) |field| {
                    @field(res, field.name) = self.get(field, U);
                }
                return res;
            },
            else => @compileError("Undecodable type: " ++ @typeName(T)),
        }
    }
};

// TODO: Do the missing nodes
pub fn old_encodeKey(
    alloc: mem.Allocator,
    node: Key,
    nodes: *std.ArrayListUnmanaged(Node),
    extra: *std.ArrayListUnmanaged(u32),
) !NodeID {
    switch (node) {
        .assignment => |data| {
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

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
