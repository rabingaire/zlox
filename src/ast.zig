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
            const debug_value = try root.debugPrint(
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
        const new = try allocator.create(Self);
        return new;
    }

    pub fn copy(self: Self, allocator: std.mem.Allocator) !*Self {
        const new = try create(allocator);
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

    fn deinit(self: *Self, allocator: std.mem.Allocator) void {
        defer allocator.destroy(self);
        switch (self.*) {
            .binary => |binary| {
                binary.left.deinit(allocator);
                binary.right.deinit(allocator);
            },
            .unary => |unary| {
                unary.right.deinit(allocator);
            },
            .grouping => |grouping| {
                grouping.expression.deinit(allocator);
            },
            .literal => {},
        }
    }

    fn debugPrint(self: *Self, allocator: std.mem.Allocator, source: [:0]const u8) ![]const u8 {
        return switch (self.*) {
            .binary => |binary| blk: {
                const left = try binary.left.debugPrint(
                    allocator,
                    source,
                );
                defer allocator.free(left);
                const right = try binary.right.debugPrint(
                    allocator,
                    source,
                );
                defer allocator.free(right);
                const operator = Token.toLiteral(source, binary.operator);
                break :blk try std.fmt.allocPrint(
                    allocator,
                    "( {s} {s} {s} )",
                    .{ left, operator, right },
                );
            },
            .unary => |unary| blk: {
                const right = try unary.right.debugPrint(
                    allocator,
                    source,
                );
                defer allocator.free(right);
                const operator = Token.toLiteral(source, unary.operator);
                break :blk try std.fmt.allocPrint(
                    allocator,
                    "( {s} {s} )",
                    .{ operator, right },
                );
            },
            .grouping => |grouping| blk: {
                const expr = try grouping.expression.debugPrint(
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
            .literal => |literal| switch (literal) {
                .number => try std.fmt.allocPrint(
                    allocator,
                    "{d}",
                    .{literal.number},
                ),
                .string => try std.fmt.allocPrint(
                    allocator,
                    "\"{s}\"",
                    .{literal.string},
                ),
                .boolean => try std.fmt.allocPrint(
                    allocator,
                    "{any}",
                    .{literal.boolean},
                ),
                .nil => try std.fmt.allocPrint(
                    allocator,
                    "nil",
                    .{},
                ),
            },
        };
    }
};
