const std = @import("std");

const lexer = @import("lexer.zig");
const Token = lexer.Token;

pub const Ast = struct {
    const Self = @This();

    root: *Expression,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *Self) void {
        self.root.deinit(self.allocator);
    }
};

pub const Expression = union(enum) {
    const Self = @This();

    binary: Binary,
    unary: Unary,
    grouping: Grouping,
    literal: Literal,

    pub fn create(allocator: std.mem.Allocator) !*Self {
        var new = try allocator.create(Self);
        return new;
    }

    pub fn copy(self: Self, allocator: std.mem.Allocator) !*Self {
        var new = try create(allocator);
        new.* = self;
        return new;
    }

    fn deinit(self: *Self, allocator: std.mem.Allocator) void {
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

    pub fn addBinary(self: *Self, left: *Self, operator: Token, right: *Self) void {
        self.* = Self{
            .binary = .{
                .left = left,
                .operator = operator,
                .right = right,
            },
        };
    }

    pub fn addUnary(self: *Self, operator: Token, right: *Self) void {
        self.* = Self{
            .unary = .{
                .operator = operator,
                .right = right,
            },
        };
    }

    pub fn addLiteral(self: *Self, literal: Literal) void {
        self.* = Self{
            .literal = literal,
        };
    }

    const Binary = struct {
        left: *Self,
        operator: Token,
        right: *Self,
    };

    const Unary = struct {
        operator: Token,
        right: *Self,
    };

    const Grouping = struct {
        expression: *Self,
    };

    pub const Literal = union(enum) {
        number: f64,
        string: []const u8,
        boolean: bool,
        nil: void,
    };
};
