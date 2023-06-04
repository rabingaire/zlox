const std = @import("std");

const lexer = @import("lexer.zig");
const Token = lexer.Token;

pub const Expression = union(enum) {
    const Self = @This();

    binary: Binary,
    unary: Unary,
    grouping: Grouping,
    literal: Literal,

    pub const Binary = struct {
        left: *Self,
        operator: Token,
        right: *Self,
    };

    pub const Unary = struct {
        operator: Token,
        right: *Self,
    };

    pub const Grouping = struct {
        expression: *Self,
    };

    pub const Literal = union(enum) {
        number: f64,
        string: []const u8,
        boolean: bool,
        nil: void,
    };

    pub fn deinit(self: *const Self, allocator: std.mem.Allocator) void {
        defer allocator.destroy(self);
        switch (self.*) {
            .binary => {
                self.binary.left.deinit(allocator);
                self.binary.right.deinit(allocator);
            },
            .unary => {
                self.unary.right.deinit(allocator);
            },
            .grouping => {
                self.grouping.expression.deinit(allocator);
            },
            else => {},
        }
    }
};
