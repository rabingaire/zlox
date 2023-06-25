const std = @import("std");
const builtin = @import("builtin");

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const parser = @import("parser.zig");
const Parser = parser.Parser;

pub const Ast = struct {
    const Self = @This();

    errors: []Error,
    nodes: []Node,
    root: NodeIndex,
    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tokens: []Token,

    pub fn parse(allocator: std.mem.Allocator, source: [:0]const u8) !Self {
        var prsr = try Parser.init(allocator, source);
        defer {
            prsr.nodes.deinit();
            prsr.errors.deinit();
        }
        errdefer allocator.free(prsr.tokens);

        try prsr.parseRoot();
        const root = prsr.root;
        const nodes = try prsr.nodes.toOwnedSlice();
        const errors = try prsr.errors.toOwnedSlice();

        if (builtin.mode == .Debug and errors.len == 0) {
            const debug_value = try Expression.debugPrint(
                root,
                nodes,
                allocator,
                source,
            );
            defer allocator.free(debug_value);
            std.debug.print("\n\n>>>>>>> AST Debug Info <<<<<<<\n\n", .{});
            std.debug.print("{s}\n", .{debug_value});
        }

        return Self{
            .errors = errors,
            .nodes = nodes,
            .root = root,
            .allocator = allocator,
            .source = source,
            .tokens = prsr.tokens,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.tokens);
        self.allocator.free(self.nodes);
        self.allocator.free(self.errors);
    }
};

pub const Node = Expression;
pub const NodeIndex = u32;

pub const Expression = union(enum) {
    binary: Binary,
    unary: Unary,
    grouping: Grouping,
    literal: Literal,

    pub const Literal = union(enum) {
        number: f64,
        string: []const u8,
        boolean: bool,
        nil: void,
    };

    pub const Binary = struct {
        left: NodeIndex,
        operator: Token,
        right: NodeIndex,
    };

    pub const Unary = struct {
        operator: Token,
        right: NodeIndex,
    };

    pub const Grouping = struct {
        expression: NodeIndex,
    };

    fn debugPrint(
        node_index: NodeIndex,
        nodes: []Node,
        allocator: std.mem.Allocator,
        source: [:0]const u8,
    ) ![]const u8 {
        const node = nodes[node_index];
        return switch (node) {
            .binary => |binary| blk: {
                const left = try debugPrint(
                    binary.left,
                    nodes,
                    allocator,
                    source,
                );
                defer allocator.free(left);
                const right = try debugPrint(
                    binary.right,
                    nodes,
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
                const right = try debugPrint(
                    unary.right,
                    nodes,
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
                const expr = try debugPrint(
                    grouping.expression,
                    nodes,
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

pub const Error = struct {
    error_type: Type,
    current_token: Token,
    expected_token_type: ?Token.Type,

    pub const Type = enum {
        expected_expression,
        expected_token,
    };

    pub fn print(tree: Ast) !void {
        const stderr = std.io.getStdErr().writer();

        for (tree.errors) |parse_error| {
            try stderr.print("\nError: at line {d} column {d}\n\t", .{
                parse_error.current_token.line,
                parse_error.current_token.column,
            });
            switch (parse_error.error_type) {
                .expected_expression => {
                    try stderr.print("Expected expression\n", .{});
                },
                .expected_token => {
                    try stderr.print(
                        "Expected '{s}' got '{s}'\n",
                        .{
                            Token.Type.toLiteral(
                                parse_error.expected_token_type.?,
                            ).?,
                            Token.toLiteral(
                                tree.source,
                                parse_error.current_token,
                            ),
                        },
                    );
                },
            }
        }
    }
};
