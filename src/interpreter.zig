const std = @import("std");
const AllocatorError = std.mem.Allocator.Error;

const ast = @import("ast.zig");
const Ast = ast.Ast;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const Literal = Node.Literal;
const lexer = @import("lexer.zig");
const Token = lexer.Token;

pub const Error = error{EvalError} || AllocatorError;

pub const Interpreter = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tree: Ast,
    result_literal: Literal,
    errors: std.ArrayList(RuntimeError),

    pub fn deinit(self: *Self) void {
        self.errors.deinit();
    }

    pub fn hasErrors(self: *Self) bool {
        return self.errors.items.len != 0;
    }

    pub fn evaluate(tree: Ast) !Self {
        var inter = Self{
            .allocator = tree.allocator,
            .source = tree.source,
            .tree = tree,
            .result_literal = undefined,
            .errors = std.ArrayList(RuntimeError).init(tree.allocator),
        };
        inter.evaluateNode(
            inter.tree.root,
            inter.tree.nodes,
        ) catch |err| switch (err) {
            error.EvalError => {},
            else => return err,
        };
        return inter;
    }

    fn print(self: *Self) !void {
        const stdout = std.io.getStdOut().writer();
        switch (self.result_literal) {
            .number => |value| try stdout.print("{d}\n", .{value}),
            .string => |value| {
                defer self.allocator.free(value);
                try stdout.print("{s}\n", .{value});
            },
            .boolean => |value| try stdout.print("{any}\n", .{value}),
            .nil => try stdout.print("nil\n", .{}),
        }
    }

    fn evaluateNode(
        self: *Self,
        node_index: NodeIndex,
        nodes: []Node,
    ) !void {
        const node = nodes[node_index];
        const literal = switch (node) {
            // Statement
            .print => |expr_node| {
                self.result_literal = try self.evaluateExpression(
                    expr_node,
                    nodes,
                );
                try self.print();
            },
            // Expression
            else => {
                self.result_literal = try self.evaluateExpression(
                    node_index,
                    nodes,
                );
            },
        };
        return literal;
    }

    fn evaluateExpression(
        self: *Self,
        node_index: NodeIndex,
        nodes: []Node,
    ) Error!Literal {
        const node = nodes[node_index];
        const literal = switch (node) {
            .binary => |b_node| try self.evaluateBinary(
                b_node,
                nodes,
            ),
            .unary => |u_node| try self.evaluateUnary(
                u_node,
                nodes,
            ),
            .grouping => |g_node| try self.evaluateExpression(
                g_node.expression,
                nodes,
            ),
            .literal => |l_node| l_node,
            else => unreachable,
        };
        return literal;
    }

    fn evaluateBinary(
        self: *Self,
        expr: Node.Binary,
        nodes: []Node,
    ) !Literal {
        const left = try self.evaluateExpression(
            expr.left,
            nodes,
        );
        const left_type = @tagName(left);
        const right = try self.evaluateExpression(
            expr.right,
            nodes,
        );
        const right_type = @tagName(right);

        defer {
            const expected_type = @tagName(Literal.string);
            if (isType(
                left_type,
                expected_type,
            )) {
                self.allocator.free(left.string);
            }
            if (isType(
                right_type,
                expected_type,
            )) {
                self.allocator.free(right.string);
            }
        }

        const operator = expr.operator;

        const is_number_type = isBothType(
            left_type,
            right_type,
            @tagName(Literal.number),
        );
        const is_string_type = isBothType(
            left_type,
            right_type,
            @tagName(Literal.string),
        );
        switch (operator.token_type) {
            .PLUS => {
                if (is_number_type) {
                    return Literal{ .number = left.number + right.number };
                }
                if (is_string_type) {
                    const value = try std.fmt.allocPrint(
                        self.allocator,
                        "{s}{s}",
                        .{ left.string, right.string },
                    );
                    return Literal{ .string = value };
                }
            },
            .MINUS => {
                if (is_number_type) {
                    return Literal{ .number = left.number - right.number };
                }
            },
            .STAR => {
                if (is_number_type) {
                    return Literal{ .number = left.number * right.number };
                }
            },
            .SLASH => {
                if (is_number_type) {
                    return Literal{ .number = left.number / right.number };
                }
            },
            .GREATER => {
                if (is_number_type) {
                    return Literal{ .boolean = left.number > right.number };
                }

                if (is_string_type) {
                    return Literal{ .boolean = compareString(isGreater, left.string, right.string) };
                }
            },
            .GREATER_EQUAL => {
                if (is_number_type) {
                    return Literal{ .boolean = left.number >= right.number };
                }

                if (is_string_type) {
                    return Literal{ .boolean = compareString(isGreaterEqual, left.string, right.string) };
                }
            },
            .LESS => {
                if (is_number_type) {
                    return Literal{ .boolean = left.number < right.number };
                }

                if (is_string_type) {
                    return Literal{ .boolean = compareString(isLess, left.string, right.string) };
                }
            },
            .LESS_EQUAL => {
                if (is_number_type) {
                    return Literal{ .boolean = left.number <= right.number };
                }

                if (is_string_type) {
                    return Literal{ .boolean = compareString(isLessEqual, left.string, right.string) };
                }
            },
            .BANG_EQUAL => {
                if (is_number_type) {
                    return Literal{ .boolean = left.number != right.number };
                }

                if (is_string_type) {
                    return Literal{
                        .boolean = !std.mem.eql(
                            u8,
                            left.string,
                            right.string,
                        ),
                    };
                }

                return Literal{ .boolean = isTruthy(left) != isTruthy(right) };
            },
            .EQUAL_EQUAL => {
                if (is_number_type) {
                    return Literal{ .boolean = left.number == right.number };
                }

                if (is_string_type) {
                    return Literal{
                        .boolean = std.mem.eql(
                            u8,
                            left.string,
                            right.string,
                        ),
                    };
                }

                return Literal{ .boolean = isTruthy(left) == isTruthy(right) };
            },
            else => unreachable,
        }
        return self.addError(
            .invalid_binary_operation,
            operator,
            RuntimeError.DataTypeInfo{
                .left = left_type,
                .right = right_type,
            },
        );
    }

    fn isGreater(left: usize, right: usize) bool {
        return left > right;
    }
    fn isGreaterEqual(left: usize, right: usize) bool {
        return left >= right;
    }
    fn isLess(left: usize, right: usize) bool {
        return left < right;
    }
    fn isLessEqual(left: usize, right: usize) bool {
        return left <= right;
    }
    fn compareString(comptime func: fn (usize, usize) bool, left: []const u8, right: []const u8) bool {
        var idx: usize = 0;
        while (idx < @min(left.len, right.len)) : (idx += 1) {
            if (left[idx] == right[idx]) {
                continue;
            }
            return func(left[idx], right[idx]);
        }
        return func(left.len, right.len);
    }

    fn isType(
        value_type: []const u8,
        expected_type: []const u8,
    ) bool {
        return std.mem.eql(
            u8,
            value_type,
            expected_type,
        );
    }

    fn isBothType(
        left_type: []const u8,
        right_type: []const u8,
        expected_type: []const u8,
    ) bool {
        const is_equal_type = std.mem.eql(
            u8,
            left_type,
            right_type,
        );
        return is_equal_type and std.mem.eql(
            u8,
            left_type,
            expected_type,
        );
    }

    fn evaluateUnary(
        self: *Self,
        expr: Node.Unary,
        nodes: []Node,
    ) !Literal {
        const right = try self.evaluateExpression(
            expr.right,
            nodes,
        );
        const operator = expr.operator;
        return switch (operator.token_type) {
            .BANG => Literal{ .boolean = !isTruthy(right) },
            .MINUS => blk: {
                if (!std.mem.eql(u8, @tagName(right), @tagName(Literal.number))) {
                    return self.addError(
                        .invalid_unary_operation,
                        operator,
                        RuntimeError.DataTypeInfo{
                            .right = @tagName(right),
                        },
                    );
                }
                break :blk Literal{ .number = -right.number };
            },
            else => unreachable,
        };
    }

    fn addError(
        self: *Self,
        error_type: RuntimeError.Type,
        token: Token,
        data_type: RuntimeError.DataTypeInfo,
    ) Error {
        try self.errors.append(
            RuntimeError{
                .error_type = error_type,
                .token = token,
                .data_type = data_type,
            },
        );
        return Error.EvalError;
    }

    fn isTruthy(lit: Literal) bool {
        return switch (lit) {
            .number => |value| value != 0,
            .string => |value| !std.mem.eql(u8, value, ""),
            .boolean => |value| value,
            .nil => false,
        };
    }
};

pub const RuntimeError = struct {
    pub const DataTypeInfo = struct {
        left: ?[]const u8 = null,
        right: []const u8,
    };

    error_type: Type,
    token: Token,
    data_type: DataTypeInfo,

    pub const Type = enum {
        invalid_binary_operation,
        invalid_unary_operation,
    };

    pub fn print(inter: Interpreter) !void {
        const stderr = std.io.getStdErr().writer();

        for (inter.errors.items) |eval_error| {
            try stderr.print("\nRuntime::Error: at line {d} column {d}\n\t", .{
                eval_error.token.line,
                eval_error.token.column,
            });
            switch (eval_error.error_type) {
                .invalid_binary_operation => {
                    try stderr.print(
                        "Bad binary operator '{s}' for operand of type '{s}' and '{s}'\n",
                        .{
                            Token.toLiteral(inter.source, eval_error.token),
                            eval_error.data_type.left.?,
                            eval_error.data_type.right,
                        },
                    );
                },
                .invalid_unary_operation => {
                    try stderr.print(
                        "Bad unary operator '{s} for operand of type '{s}'\n",
                        .{
                            Token.toLiteral(inter.source, eval_error.token),
                            eval_error.data_type.right,
                        },
                    );
                },
            }
        }
    }
};
