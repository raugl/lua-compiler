const std = @import("std");

pub const NodeID = enum(u32) {
    root = 0,
    null = 0xffff_ffff,
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

pub const Conditional = struct {
    cond: NodeID,
    block: NodeID,
};

pub const Field = struct {
    key: ?NodeID,
    value: NodeID,
};

pub const String = CompactSlice(u8);

// TODO: Change from slices of NodeIDs to linked lists
pub fn CompactSlice(comptime T: type) type {
    return struct {
        start: u32,
        len: u32,

        pub const empty = CompactSlice{ .start = undefined, .len = 0 };

        pub fn init(buffer: []const u8, ptr: []const T) @This() {
            const start = @intFromPtr(ptr.ptr) - @intFromPtr(buffer.ptr);
            return @This(){ .start = start, .len = ptr.len };
        }

        pub fn items(self: @This(), buffer: []const u8) []const T {
            const ptr: [*]const T = @ptrFromInt(@intFromPtr(buffer.ptr) + self.start);
            return ptr[0..self.len];
        }
    };
}

// Corelate file size with the maximum AST size, page allocate an array of that
// size. Make an arena allocator with that fixed size buffer. Build the AST in
// post order DFS to be able to precisely allocate all extra data one shot.
// Convert the allocation pointers into indices into the original fixed size
// buffer. Store the nodes in a MultiArrayList(Node union), they point into the
// fixed size buffer for extra data.

pub fn LinkedList(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const Node = struct {
            data: T,
            next: ?u32 = null,
        };

        pub const Iterator = struct {
            next: ?NodeID = null,

            pub fn next(self: *Iterator) ?T {
                const next = self.next orelse return null;
                return self.nodes[@intFromEnum(next)];
            }
        };

        pub fn iterator(self: Self) Iterator {}

        head: ?NodeID = null,
    };
}

pub const NodeList = struct {
    first: NodeID = .null,
};

pub const Node = struct {
    data: NodeData,
    next: NodeID = .null,
};

// TODO: Add missing variants
pub const NodeData = union(enum) {
    block: struct {
        statements: NodeList,
        ret_values: NodeList,
    },
    @"break",
    label: String,
    goto: String,
    identifier: String,
    literal_nil,
    literal_true,
    literal_false,
    literal_ellipsis,
    literal_int: i64,
    literal_float: f64,
    literal_string: String,
    assignment: struct {
        names: NodeList,
        values: NodeList,
    },
    local_decl: struct {
        names: NodeList,
        values: NodeList,
    },
    repeat_loop: struct {
        cond: NodeID,
        block: NodeID,
    },
    while_loop: struct {
        cond: NodeID,
        block: NodeID,
    },
    elseif_block: struct {
        cond: NodeID,
        block: NodeID,
    },
    if_block: struct {
        cond: NodeID,
        block: NodeID,
        elseif: CompactSlice(Conditional),
        @"else": NodeID = .null,
    },
    for_loop: struct {
        name: String,
        start: NodeID,
        step: NodeID = .null,
        end: NodeID,
        block: NodeID,
    },
    for_each: struct {
        names: NodeList,
        values: NodeList,
        block: NodeID,
    },
    func_decl: struct {
        names: NodeList,
        member: String = .empty,
        params: NodeList,
        variadic: bool,
        block: NodeID,
    },
    func_local: struct {
        name: String,
        params: NodeList,
        variadic: bool,
        block: NodeID,
    },
    func_exp: struct {
        params: NodeList,
        variadic: bool,
        block: NodeID,
    },
    func_call: struct {
        object: NodeID,
        member: String = .empty,
        args: NodeList,
    },
    table_constructor: struct {
        fields: CompactSlice(Field),
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

test "semantic analyses" {
    std.testing.refAllDecls(@This());
}
