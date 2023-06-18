const std = @import("std");
const builtin = @import("builtin");

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const parser = @import("parser.zig");
const Parser = parser.Parser;

pub const Ast = struct {
    const Self = @This();

    root: *Expression,
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokens: []Token,

    pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Self {
        var prsr = try Parser.init(allocator, source);
        var root = try prsr.parseRoot();
        if (builtin.mode == .Debug) {
            var debug_value = try root.debugPrint(
                allocator,
                source,
            );
            defer allocator.free(debug_value);
            std.debug.print("\n\n>>>>>>> AST Debug Info <<<<<<<\n\n", .{});
            std.debug.print("{s}\n", .{debug_value});
        }
        return Self{
            .root = root,
            .allocator = allocator,
            .source = source,
            .tokens = prsr.tokens,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.tokens);
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

    pub const Literal = union(enum) {
        number: f64,
        string: []const u8,
        boolean: bool,
        nil: void,
    };

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
            .literal => {},
        }
    }

    fn debugPrint(self: *Self, allocator: std.mem.Allocator, source: [:0]const u8) ![]const u8 {
        return switch (self.*) {
            .binary => blk: {
                var left = try self.binary.left.debugPrint(
                    allocator,
                    source,
                );
                defer allocator.free(left);
                var right = try self.binary.right.debugPrint(
                    allocator,
                    source,
                );
                defer allocator.free(right);
                var operator = Token.toLiteral(source, self.binary.operator);
                var a = try std.fmt.allocPrint(
                    allocator,
                    "( {s} {s} {s} )",
                    .{ left, operator, right },
                );
                break :blk a;
            },
            .unary => blk: {
                var right = try self.unary.right.debugPrint(
                    allocator,
                    source,
                );
                defer allocator.free(right);
                var operator = Token.toLiteral(source, self.unary.operator);
                break :blk try std.fmt.allocPrint(
                    allocator,
                    "( {s} {s} )",
                    .{ operator, right },
                );
            },
            .grouping => blk: {
                var expr = try self.grouping.expression.debugPrint(
                    allocator,
                    source,
                );
                defer allocator.free(expr);
                break :blk try std.fmt.allocPrint(
                    allocator,
                    "( {s} )",
                    .{expr},
                );
            },
            .literal => switch (self.literal) {
                .number => try std.fmt.allocPrint(
                    allocator,
                    "{d}",
                    .{self.literal.number},
                ),
                .string => self.literal.string,
                .boolean => if (self.literal.boolean) "true" else "false",
                .nil => "nil",
            },
        };
    }
};
