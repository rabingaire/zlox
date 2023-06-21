const std = @import("std");

const ast = @import("ast.zig");
const Ast = ast.Ast;
const Expression = ast.Expression;
const Node = ast.Node;
const NodeIndex = ast.NodeIndex;
const Literal = Expression.Literal;
const lexer = @import("lexer.zig");
const Token = lexer.Token;
const Type = Token.Type;

pub const Interpreter = struct {
    pub fn evaluate(tree: Ast) !void {
        const literal = try evaluateExpression(
            tree.allocator,
            tree.root,
            tree.nodes,
        );
        print(tree.allocator, literal);
    }

    fn print(allocator: std.mem.Allocator, lit: Literal) void {
        switch (lit) {
            .number => |value| std.debug.print("{d}\n", .{value}),
            .string => |value| {
                defer allocator.free(value);
                std.debug.print("{s}\n", .{value});
            },
            .boolean => |value| std.debug.print("{any}\n", .{value}),
            .nil => std.debug.print("nil\n", .{}),
        }
    }

    fn evaluateExpression(
        allocator: std.mem.Allocator,
        node_index: NodeIndex,
        nodes: []Node,
    ) anyerror!Literal {
        const node = nodes[node_index];
        const literal = switch (node) {
            .binary => |b_node| try evaluateBinary(
                allocator,
                b_node,
                nodes,
            ),
            .unary => |u_node| try evaluateUnary(
                allocator,
                u_node,
                nodes,
            ),
            .literal => |l_node| l_node,
            else => unreachable,
        };
        return literal;
    }

    fn evaluateBinary(allocator: std.mem.Allocator, expr: Expression.Binary, nodes: []Node) !Literal {
        const left = try evaluateExpression(
            allocator,
            expr.left,
            nodes,
        );
        const right = try evaluateExpression(
            allocator,
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
                        const value = try std.fmt.allocPrint(
                            allocator,
                            "{s}{s}",
                            .{ left.string, right.string },
                        );
                        break :blk Literal{ .string = value };
                    }
                }
                std.debug.print(
                    "Bad binary operator '+' at line {d} column {d} for operand of type '{s}' & '{s}'\n",
                    .{ operator.line, operator.column, @tagName(left), @tagName(right) },
                );
                std.os.exit(85);
            },
            .MINUS => blk: {
                const value_type = @tagName(left);
                const isNumber = (std.mem.eql(u8, value_type, @tagName(right)) and
                    std.mem.eql(u8, value_type, @tagName(Literal.number)));
                if (!isNumber) {
                    std.debug.print(
                        "Bad binary operator '-' at line {d} column {d} for operand of type '{s}' & '{s}'\n",
                        .{ operator.line, operator.column, @tagName(left), @tagName(right) },
                    );
                    std.os.exit(85);
                }
                break :blk Literal{ .number = left.number - right.number };
            },
            else => unreachable,
        };
    }

    fn evaluateUnary(
        allocator: std.mem.Allocator,
        expr: Expression.Unary,
        nodes: []Node,
    ) !Literal {
        const right = try evaluateExpression(
            allocator,
            expr.right,
            nodes,
        );
        const operator = expr.operator;
        return switch (operator.token_type) {
            .BANG => Literal{ .boolean = !isTruthy(right) },
            .MINUS => blk: {
                if (!std.mem.eql(u8, @tagName(right), @tagName(Literal.number))) {
                    std.debug.print(
                        "Bad unary operator '-' at line {d} column {d} for operand of type '{s}'\n",
                        .{ operator.line, operator.column, @tagName(right) },
                    );
                    std.os.exit(85);
                }
                break :blk Literal{ .number = -right.number };
            },
            else => unreachable,
        };
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
