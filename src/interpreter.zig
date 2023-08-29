const std = @import("std");
const AllocatorError = std.mem.Allocator.Error;
const builtin = @import("builtin");

const ast = @import("ast.zig");
const Ast = ast.Ast;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const Literal = Node.Expression.Literal;
const lexer = @import("lexer.zig");
const Token = lexer.Token;

pub const Error = error{EvalError} || AllocatorError;

pub const Interpreter = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    source: [:0]const u8,
    tree: Ast,
    result_literal: Literal,
    final_result: []const u8,
    errors: std.ArrayList(RuntimeError),
    environments: std.ArrayList(*std.StringHashMap(NodeIndex)),

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.final_result);
        self.errors.deinit();
        self.environments.deinit();
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
            .final_result = undefined,
            .errors = std.ArrayList(RuntimeError).init(tree.allocator),
            .environments = std.ArrayList(*std.StringHashMap(NodeIndex)).init(tree.allocator),
        };

        inter.final_result = inter.evaluateNode(
            inter.tree.root,
            inter.tree.nodes,
        ) catch |err| switch (err) {
            error.EvalError => "",
            else => return err,
        };

        return inter;
    }

    fn result_value(self: *Self) ![]const u8 {
        return switch (self.result_literal) {
            .number => |value| try std.fmt.allocPrint(self.allocator, "{d}", .{value}),
            .string => |value| value,
            .boolean => |value| try std.fmt.allocPrint(self.allocator, "{any}", .{value}),
            .nil => try std.fmt.allocPrint(self.allocator, "nil", .{}),
            else => unreachable,
        };
    }

    fn print(result: []const u8) !void {
        const stdout = std.io.getStdOut().writer();
        if (!builtin.is_test) {
            try stdout.print("{s}\n", .{result});
        }
    }

    fn evaluateNode(
        self: *Self,
        node_index: NodeIndex,
        nodes: []Node,
    ) ![]const u8 {
        const node = nodes[node_index];
        return switch (node) {
            // Program
            .program => |node_indexes| {
                var new_environment = std.StringHashMap(NodeIndex).init(self.allocator);
                try self.environments.append(&new_environment);
                defer self.environments.pop().deinit();

                var result: []const u8 = "";
                for (node_indexes, 1..) |program_node_index, i| {
                    result = try self.evaluateNode(
                        program_node_index,
                        nodes,
                    );
                    if (node_indexes.len != i) {
                        self.allocator.free(result);
                    }
                }
                return result;
            },
            // Statement
            .print => |expr_node| {
                self.result_literal = try self.evaluateExpression(
                    expr_node,
                    nodes,
                );
                const result = try self.result_value();
                try print(result);
                return result;
            },
            .variable => |var_node| {
                const environment = self.environments.getLast();
                try environment.put(
                    Token.toLiteral(self.source, var_node.symbol),
                    var_node.value,
                );
                return "";
            },
            .block => |block_node| {
                var new_environment = std.StringHashMap(NodeIndex).init(self.allocator);
                try self.environments.append(&new_environment);
                defer self.environments.pop().deinit();

                var result: []const u8 = "";
                for (block_node.statement_indexes, 1..) |statement_index, i| {
                    result = try self.evaluateNode(
                        statement_index,
                        nodes,
                    );
                    if (block_node.statement_indexes.len != i) {
                        self.allocator.free(result);
                    }
                }
                return result;
            },
            // Expression
            else => {
                self.result_literal = try self.evaluateExpression(
                    node_index,
                    nodes,
                );
                return try self.result_value();
            },
        };
    }

    fn evaluateExpression(
        self: *Self,
        node_index: NodeIndex,
        nodes: []Node,
    ) Error!Literal {
        const node = nodes[node_index];
        const literal = switch (node) {
            .expression => |e_node| switch (e_node) {
                .literal => |l_node| switch (l_node) {
                    .string => |s_literal| Literal{
                        .string = try std.fmt.allocPrint(
                            self.allocator,
                            "{s}",
                            .{s_literal},
                        ),
                    },
                    .ident => |i_node| blk: {
                        var idx = self.environments.items.len;
                        while (idx > 0) : (idx -= 1) {
                            const environment = self.environments.items[idx - 1];
                            const value = environment.get(
                                Token.toLiteral(self.source, i_node),
                            ) orelse {
                                continue;
                            };
                            break :blk self.evaluateExpression(
                                value,
                                nodes,
                            );
                        }
                        return self.addError(
                            .undefined_variable,
                            i_node,
                            RuntimeError.DataTypeInfo{
                                .right = "",
                            },
                        );
                    },
                    else => l_node,
                },
                .grouping => |g_node| try self.evaluateExpression(
                    g_node.expression,
                    nodes,
                ),
                .unary => |u_node| try self.evaluateUnary(
                    u_node,
                    nodes,
                ),
                .binary => |b_node| try self.evaluateBinary(
                    b_node,
                    nodes,
                ),
            },
            else => unreachable,
        };
        return literal;
    }

    fn evaluateBinary(
        self: *Self,
        expr: Node.Expression.Binary,
        nodes: []Node,
    ) !Literal {
        const left = try self.evaluateExpression(
            expr.left,
            nodes,
        );
        const left_type = @tagName(left);

        defer {
            if (isType(
                left_type,
                @tagName(Literal.string),
            )) {
                self.allocator.free(left.string);
            }
        }

        const right = try self.evaluateExpression(
            expr.right,
            nodes,
        );
        const right_type = @tagName(right);

        defer {
            if (isType(
                right_type,
                @tagName(Literal.string),
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
        expr: Node.Expression.Unary,
        nodes: []Node,
    ) !Literal {
        const right = try self.evaluateExpression(
            expr.right,
            nodes,
        );
        const right_type = @tagName(right);

        defer {
            if (isType(
                right_type,
                @tagName(Literal.string),
            )) {
                self.allocator.free(right.string);
            }
        }

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
        return error.EvalError;
    }

    fn isTruthy(lit: Literal) bool {
        return switch (lit) {
            .number => |value| value != 0,
            .string => |value| !std.mem.eql(u8, value, ""),
            .boolean => |value| value,
            .nil => false,
            else => unreachable,
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
        undefined_variable,
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
                        "Bad unary operator '{s}' for operand of type '{s}'\n",
                        .{
                            Token.toLiteral(inter.source, eval_error.token),
                            eval_error.data_type.right,
                        },
                    );
                },
                .undefined_variable => {
                    try stderr.print(
                        "Undefined variable '{s}'\n",
                        .{
                            Token.toLiteral(inter.source, eval_error.token),
                        },
                    );
                },
            }
        }
    }
};

test "check if interpreter evaluates to correct value" {
    try testEvaluate(
        \\ "hello"
    ,
        "hello",
    );

    try testEvaluate(
        \\ var foo = "hello";
        \\var bar = "world"
        \\ print foo + " " + bar;
    ,
        "hello world",
    );

    try testEvaluate(
        \\ var a = -2 + 3;
        \\ print a;
    ,
        "1",
    );

    try testEvaluate(
        \\ var a = 2 + 3 - (4 / 5) * 2;
        \\ print a;
    ,
        "3.4",
    );

    try testEvaluate(
        \\ var a = 5 == 2
        \\ var b = "hello" == "world"
        \\ print a == b
    ,
        "true",
    );

    try testEvaluate(
        \\ var a = 5 == 5
        \\ var b = "hello" == "hello"
        \\ print a == b
    ,
        "true",
    );

    try testEvaluate(
        \\ var a = 5 >= 5;
        \\ var b = 3 <= 5;
        \\ print a == b;
    ,
        "true",
    );

    try testEvaluate(
        \\ var a = !true;
        \\ print a;
    ,
        "false",
    );

    try testEvaluate(
        \\ var a = !false;
        \\ print a;
    ,
        "true",
    );

    try testEvaluate(
        \\ var a = !"cat";
        \\ print a;
    ,
        "false",
    );

    try testEvaluate(
        \\ var a = !!"0";
        \\ print a;
    ,
        "true",
    );

    try testEvaluate(
        \\ var a = !!"0";
        \\ var b = !a;
        \\ print b;
    ,
        "false",
    );
}

test "check if interpreter detects runtime errors correctly" {
    try testError(
        \\ print "hello" + 2
    ,
        &.{.invalid_binary_operation},
    );

    try testError(
        \\ var a = "world"
        \\ print a + a - a
    ,
        &.{.invalid_binary_operation},
    );

    try testError(
        \\ var a = "world"
        \\ print a * a / a
    ,
        &.{.invalid_binary_operation},
    );

    try testError(
        \\ var a = 2
        \\ print (a * a) / "cat"
    ,
        &.{.invalid_binary_operation},
    );

    try testError(
        \\ print -"world"
    ,
        &.{.invalid_unary_operation},
    );

    try testError(
        \\ var a = "world"
        \\ print b
    ,
        &.{.undefined_variable},
    );

    try testError(
        \\ var a = "world"
        \\ print a + a + b
    ,
        &.{.undefined_variable},
    );
}

fn testEvaluate(source: [:0]const u8, expected_result: []const u8) !void {
    var tree = try Ast.parse(std.testing.allocator, source);
    defer tree.deinit();

    std.testing.expect(!tree.hasErrors()) catch |err| {
        std.debug.print("parser errors: {any}\n", .{tree.errors});
        return err;
    };

    var inter = try Interpreter.evaluate(tree);
    defer inter.deinit();

    std.testing.expect(!inter.hasErrors()) catch |err| {
        std.debug.print("interpreter errors: {any}\n", .{tree.errors});
        return err;
    };

    try std.testing.expectEqualDeep(expected_result, inter.final_result);
}

fn testError(source: [:0]const u8, expected_errors: []const RuntimeError.Type) !void {
    var tree = try Ast.parse(std.testing.allocator, source);
    defer tree.deinit();

    std.testing.expect(!tree.hasErrors()) catch |err| {
        std.debug.print("parser errors: {any}\n", .{tree.errors});
        return err;
    };

    var inter = try Interpreter.evaluate(tree);
    defer inter.deinit();

    std.testing.expect(inter.hasErrors()) catch |err| {
        std.debug.print("expected errors: {any}\n", .{expected_errors});
        return err;
    };

    for (expected_errors, 0..) |expected, i| {
        try std.testing.expectEqual(expected, inter.errors.items[i].error_type);
    }
}
