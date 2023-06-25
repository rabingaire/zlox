const std = @import("std");
const AllocatorError = std.mem.Allocator.Error;

const ast = @import("ast.zig");
const Ast = ast.Ast;
const Expression = ast.Expression;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const Literal = Expression.Literal;
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

    pub fn evaluate(tree: Ast) AllocatorError!Self {
        var inter = Self{
            .allocator = tree.allocator,
            .source = tree.source,
            .tree = tree,
            .result_literal = undefined,
            .errors = std.ArrayList(RuntimeError).init(tree.allocator),
        };
        inter.result_literal = inter.evaluateExpression(
            inter.tree.root,
            inter.tree.nodes,
        ) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.EvalError => undefined,
        };
        return inter;
    }

    pub fn print(self: *Self) !void {
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
            .literal => |l_node| l_node,
            else => unreachable,
        };
        return literal;
    }

    fn evaluateBinary(
        self: *Self,
        expr: Expression.Binary,
        nodes: []Node,
    ) !Literal {
        const left = try self.evaluateExpression(
            expr.left,
            nodes,
        );
        const right = try self.evaluateExpression(
            expr.right,
            nodes,
        );
        const operator = expr.operator;
        return switch (operator.token_type) {
            .PLUS => blk: {
                if (std.mem.eql(u8, @tagName(left), @tagName(right))) {
                    const value_type = @tagName(left);
                    if (std.mem.eql(u8, value_type, @tagName(Literal.number))) {
                        break :blk Literal{ .number = left.number + right.number };
                    }
                    if (std.mem.eql(u8, value_type, @tagName(Literal.string))) {
                        defer {
                            self.allocator.free(left.string);
                            self.allocator.free(right.string);
                        }
                        const value = try std.fmt.allocPrint(
                            self.allocator,
                            "{s}{s}",
                            .{ left.string, right.string },
                        );
                        break :blk Literal{ .string = value };
                    }
                }
                return self.addError(
                    .invalid_binary_operation,
                    operator,
                    RuntimeError.DataTypeInfo{
                        .left = @tagName(left),
                        .right = @tagName(right),
                    },
                );
            },
            .MINUS => blk: {
                const value_type = @tagName(left);
                const isNumber = (std.mem.eql(u8, value_type, @tagName(right)) and
                    std.mem.eql(u8, value_type, @tagName(Literal.number)));
                if (!isNumber) {
                    return self.addError(
                        .invalid_binary_operation,
                        operator,
                        RuntimeError.DataTypeInfo{
                            .left = @tagName(left),
                            .right = @tagName(right),
                        },
                    );
                }
                break :blk Literal{ .number = left.number - right.number };
            },
            else => unreachable,
        };
    }

    fn evaluateUnary(
        self: *Self,
        expr: Expression.Unary,
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
