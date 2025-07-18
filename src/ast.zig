const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const assert = std.debug.assert;

pub const NodeID = enum(u32) {
    root = 0,
    none = 0xffff_ffff,
    _,
};

const NodeTag = enum(u8) {
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
    table_constructor,
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

pub const UnaryOp = enum(u8) {
    logic_not = @intFromEnum(NodeTag.logic_not),
    bit_not = @intFromEnum(NodeTag.bit_not),
    negate = @intFromEnum(NodeTag.negate),
    length = @intFromEnum(NodeTag.length),
};

pub const BinaryOp = enum(u8) {
    logic_or = @intFromEnum(NodeTag.logic_or),
    logic_and = @intFromEnum(NodeTag.logic_and),
    equal = @intFromEnum(NodeTag.equal),
    not_equal = @intFromEnum(NodeTag.not_equal),
    less_than = @intFromEnum(NodeTag.less_than),
    greater_than = @intFromEnum(NodeTag.greater_than),
    less_equal = @intFromEnum(NodeTag.less_equal),
    greater_equal = @intFromEnum(NodeTag.greater_equal),
    bit_or = @intFromEnum(NodeTag.bit_or),
    bit_and = @intFromEnum(NodeTag.bit_and),
    l_bit_shift = @intFromEnum(NodeTag.l_bit_shift),
    r_bit_shift = @intFromEnum(NodeTag.r_bit_shift),
    str_concat = @intFromEnum(NodeTag.str_concat),
    add = @intFromEnum(NodeTag.add),
    sub = @intFromEnum(NodeTag.sub),
    mul = @intFromEnum(NodeTag.mul),
    div = @intFromEnum(NodeTag.div),
    int_div = @intFromEnum(NodeTag.int_div),
    modulo = @intFromEnum(NodeTag.modulo),
    exponent = @intFromEnum(NodeTag.exponent),
    dot_access = @intFromEnum(NodeTag.dot_access),
    index_access = @intFromEnum(NodeTag.index_access),
};

const KeyTag = enum(u8) {
    assignment = @intFromEnum(NodeTag.assignment),
    @"break" = @intFromEnum(NodeTag.@"break"),
    label = @intFromEnum(NodeTag.label),
    goto = @intFromEnum(NodeTag.goto),
    block = @intFromEnum(NodeTag.block),
    repeat_loop = @intFromEnum(NodeTag.repeat_loop),
    while_loop = @intFromEnum(NodeTag.while_loop),
    if_block = @intFromEnum(NodeTag.if_block),
    if_else = @intFromEnum(NodeTag.if_else),
    for_loop = @intFromEnum(NodeTag.for_loop),
    for_each = @intFromEnum(NodeTag.for_each),
    func_decl = @intFromEnum(NodeTag.func_decl),
    func_local = @intFromEnum(NodeTag.func_local),
    func_anonym = @intFromEnum(NodeTag.func_anonym),
    local_decl = @intFromEnum(NodeTag.local_decl),
    identifier = @intFromEnum(NodeTag.identifier),
    literal_nil = @intFromEnum(NodeTag.literal_nil),
    literal_true = @intFromEnum(NodeTag.literal_true),
    literal_false = @intFromEnum(NodeTag.literal_false),
    literal_ellipsis = @intFromEnum(NodeTag.literal_ellipsis),
    literal_int = @intFromEnum(NodeTag.literal_int),
    literal_float = @intFromEnum(NodeTag.literal_float),
    literal_string = @intFromEnum(NodeTag.literal_string),
    table_constructor = @intFromEnum(NodeTag.table_constructor),
    func_call = @intFromEnum(NodeTag.func_call),
    unary_op,
    binary_op,
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

pub const NodeLabel = Slice;
pub const NodeGoto = Slice;
pub const NodeRepeat = Conditional;
pub const NodeWhile = Conditional;
pub const NodeIdentifier = Slice;
pub const NodeLiteralInt = i64;
pub const NodeLiteralFloat = f64;
pub const NodeLiteralString = Slice;

pub const NodeAssignment = struct {
    names: []const NodeID,
    values: []const NodeID,
};

pub const NodeBlock = struct {
    statements: []const NodeID,
    return_list: []const NodeID,
};

pub const NodeIf = struct {
    // PERF: Introduce an if block without elseifs, this would fully fit in a single Node
    branches: []const Conditional,
};

pub const NodeIfElse = struct {
    branches: []const Conditional,
    else_block: NodeID,
};

pub const NodeFor = struct {
    name: Slice,
    start: NodeID,
    step: ?NodeID,
    end: NodeID,
    block: NodeID,
};

pub const NodeForEach = struct {
    names: []const Slice,
    values: []const NodeID,
    block: NodeID,
};

pub const NodeFuncDecl = struct {
    names: []const Slice,
    member: ?Slice, // TODO: change everywhere else
    params: []const Slice,
    variadic: bool,
    block: NodeID,
};

pub const NodeFuncLocal = struct {
    name: Slice,
    params: []const Slice,
    block: NodeID,
    variadic: bool,
};

pub const NodeFuncAnonym = struct {
    params: []const Slice,
    block: NodeID,
    variadic: bool,
};

pub const NodeLocalDecl = struct {
    names: []const Slice,
    values: []const NodeID,
};

pub const NodeTableConstructor = struct {
    fields: []const Field,
};

pub const NodeFuncCall = struct {
    object: NodeID,
    member: ?Slice,
    args: []const NodeID,
};

pub const NodeUnaryOp = struct {
    op: UnaryOp,
    exp: NodeID,
};

pub const NodeBinaryOp = struct {
    op: BinaryOp,
    lhs: NodeID,
    rhs: NodeID,
};

pub const Conditional = struct {
    cond: NodeID,
    block: NodeID,
};

pub const Field = struct {
    key: NodeID = .none,
    value: NodeID,
};

pub const Slice = struct {
    start: u32,
    len: u32,

    pub const empty = Slice{
        .start = undefined,
        .len = 0,
    };
};

// TODO: Add missing variants
pub const Key = union(KeyTag) {
    assignment: NodeAssignment,
    @"break",
    label: NodeLabel,
    goto: NodeGoto,
    block: NodeBlock,
    repeat_loop: NodeRepeat,
    while_loop: NodeWhile,
    if_block: NodeIf,
    if_else: NodeIfElse,
    for_loop: NodeFor,
    for_each: NodeForEach,
    func_decl: NodeFuncDecl,
    func_local: NodeFuncLocal,
    func_anonym: NodeFuncAnonym,
    local_decl: NodeLocalDecl,
    identifier: NodeIdentifier,
    literal_nil,
    literal_true,
    literal_false,
    literal_ellipsis,
    literal_int: NodeLiteralInt,
    literal_float: NodeLiteralFloat,
    literal_string: NodeLiteralString,
    table_constructor: NodeTableConstructor,
    func_call: NodeFuncCall,
    unary_op: NodeUnaryOp,
    binary_op: NodeBinaryOp,
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
            return @enumFromInt(@intFromEnum(node));
        },
        .for_loop => |data| {
            const tag: NodeTag = if (data.step) |_| .for_loop_step else .for_loop;
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        .func_decl => |data| {
            const tag: NodeTag = switch (data.variadic) {
                true => if (data.member) |_| .func_decl_member_variadic else .func_decl_variadic,
                false => if (data.member) |_| .func_decl_member else .func_decl,
            };
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        .func_local => |data| {
            const tag: NodeTag = if (data.variadic) .func_local_variadic else .func_local;
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        .func_anonym => |data| {
            const tag: NodeTag = if (data.variadic) .func_anonym_variadic else .func_anonym;
            return try encodeKeyImpl(alloc, nodes, extra, tag, data);
        },
        inline .unary_op, .binary_op => |data| {
            const tag = @intFromEnum(data.op);
            return try encodeKeyImpl(alloc, nodes, extra, @enumFromInt(tag), data);
        },
        inline else => |data| {
            const tag: NodeTag = @enumFromInt(@intFromEnum(node));
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
    var use_extra = false;

    inline for (meta.fields(T)) |field| {
        const info = @typeInfo(field.type);
        switch (info) {
            .optional => |opt| if (@field(data, field.name)) |_| {
                sizeof += @sizeOf(opt.child);
            },
            .pointer => |ptr| {
                if (ptr.size == .slice) {
                    use_extra = true;
                    break;
                }
                @compileError("Unencodable pointner type: " ++ @typeName(T));
            },
            .bool => continue,
            else => sizeof += @sizeOf(field.type),
        }
    }
    const id = nodes.items.len;
    const node = try nodes.addOne(alloc);
    node.tag = tag;

    var e = Encoder.init(alloc, extra, node, use_extra or sizeof > 8);
    try e.put(data);
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
        if (use_extra) node.data.lhs = @intCast(extra.items.len);
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
                        0 => self.node.data.lhs = @bitCast(data),
                        1 => self.node.data.rhs = @bitCast(data),
                        else => unreachable,
                    }
                } else {
                    assert(@sizeOf(T) % 4 == 0);
                    const len = @sizeOf(T) / 4;
                    const ptr: [*]const u32 = @ptrCast(@alignCast(&data));

                    try self.extra.appendSlice(self.alloc, ptr[0..len]);
                    self.index += len;
                }
            },
            .pointer => |pointer| if (pointer.size == .slice) {
                assert(self.index > 0);
                const len = data.len * @sizeOf(pointer.child) / 4;
                const ptr: [*]const u32 = @ptrCast(data.ptr);

                try self.putU32(@intCast(len));
                try self.extra.appendSlice(self.alloc, ptr[0..len]);
                self.index += @intCast(len);
            } else {
                @compileError("Unencodable pointer type: " ++ @typeName(T));
            },
            .@"struct" => {
                inline for (meta.fields(T)) |field| {
                    return self.put(@field(data, field.name));
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
        => decodeKeyImpl(.for_loop, node, extra, .{
            .step = (node.tag == .for_loop_step),
        }),
        .func_decl,
        .func_decl_variadic,
        .func_decl_member,
        .func_decl_member_variadic,
        => return decodeKeyImpl(.func_decl, node, extra, .{
            .member = (node.tag == .func_decl_member or node.tag == .func_decl_member_variadic),
            .variadic = (node.tag == .func_decl_variadic or node.tag == .func_decl_member_variadic),
        }),
        .func_local,
        .func_local_variadic,
        => decodeKeyImpl(.func_local, node, extra, .{
            .variadic = (node.tag == .func_local_variadic),
        }),
        .func_anonym,
        .func_anonym_variadic,
        => decodeKeyImpl(.func_anonym, node, extra, .{
            .variadic = (node.tag == .func_anonym_variadic),
        }),
        .func_call,
        .func_call_member,
        => decodeKeyImpl(.func_call, node, extra, .{
            .member = (node.tag == .func_call_member),
        }),
        .local_decl,
        .local_decl_init,
        => decodeKeyImpl(.local_decl, node, extra, .{
            // .member = (node.tag == .func_call_member), // TODO: Does it need anything??
        }),
        .logic_not,
        .bit_not,
        .negate,
        .length,
        => decodeKeyImpl(.unary_op, node, extra, .{
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
        => decodeKeyImpl(.binary_op, node, extra, .{
            .op = @intFromEnum(node.tag),
        }),
        inline else => |tag| decodeKeyImpl(@enumFromInt(@intFromEnum(tag)), node, extra, .{}),
    };
}

// PERF: Encode value-less variants inside the NodeID
fn decodeKeyImpl(
    comptime tag: KeyTag,
    node: Node,
    extra: []const u32,
    args: anytype,
) Key {
    const T = meta.TagPayload(Key, tag);
    var sizeof: u32 = 0;
    var use_extra = false;

    inline for (meta.fields(T)) |field| {
        const info = @typeInfo(field.type);
        switch (info) {
            .optional => |opt| if (@field(args, field.name)) {
                sizeof += @sizeOf(opt.child);
            },
            .pointer => |ptr| {
                if (ptr.size == .slice) {
                    use_extra = true;
                    break;
                }
                @compileError("Undecodable pointner type: " ++ @typeName(T));
            },
            .bool => continue,
            else => sizeof += @sizeOf(field.type),
        }
    }
    var d = Decoder.init(node, extra, use_extra or sizeof > 8);
    return @unionInit(Key, @tagName(tag), d.get(T, args));
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

    fn get(self: *Decoder, comptime T: type, args: anytype) T {
        const info = @typeInfo(T);
        switch (info) {
            .bool => {
                return args;
            },
            .@"enum" => return switch (T) {
                NodeID => @enumFromInt(self.get(u32, .{})),
                else => @enumFromInt(args),
            },
            .optional => |opt| {
                if (@TypeOf(args) == bool and !args) return null;
                return self.get(opt.child, args);
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
                    return @as(*const T, @ptrCast(@alignCast(ptr))).*;
                }
            },
            .pointer => |ptr| if (ptr.size == .slice) {
                assert(self.index > 0);
                const len = self.get(u32, .{});
                const idx = self.node.data.lhs + self.index - 2;
                const slice = self.extra[idx..][0..len];
                self.index += len;
                return @ptrCast(slice);
            } else {
                @compileError("Undecodable pointer type: " ++ @typeName(T));
            },
            .@"struct" => {
                var res: T = undefined;
                inline for (meta.fields(T)) |field| {
                    if (tryDrill(args, field.name)) |arg| {
                        @field(res, field.name) = self.get(field.type, arg);
                    } else {
                        @field(res, field.name) = self.get(field.type, .{});
                    }
                }
                return res;
            },
            else => @compileError("Undecodable type: " ++ @typeName(T)),
        }
    }
};

fn TryDrillRes(comptime T: type, comptime field_name: []const u8) type {
    if (@typeInfo(T) == .@"struct" and @hasField(T, field_name)) {
        return @FieldType(T, field_name);
    }
    return void;
}

fn tryDrill(value: anytype, comptime field_name: []const u8) ?TryDrillRes(@TypeOf(value), field_name) {
    const T = @TypeOf(value);
    if (@typeInfo(T) == .@"struct" and @hasField(T, field_name)) {
        return @field(value, field_name);
    }
    return null;
}

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
