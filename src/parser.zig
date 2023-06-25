const std = @import("std");
const AllocatorError = std.mem.Allocator.Error;
const builtin = @import("builtin");

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const Scanner = lexer.Scanner;
const ast = @import("ast.zig");
const Ast = ast.Ast;
const Node = ast.Node;
const AstError = ast.Error;
const NodeIndex = ast.NodeIndex;
const Expression = ast.Expression;

pub const Error = error{ParseError} || AllocatorError;

pub const Parser = struct {
    const Self = @This();

    source: [:0]const u8,
    tokens: []Token,
    errors: std.ArrayList(AstError),
    nodes: std.ArrayList(Node),
    root: NodeIndex,
    current: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) Error!Self {
        var scanner = Scanner.init(allocator, source);
        defer scanner.tokens.deinit();

        try scanner.scanTokens();

        const tokens = try scanner.tokens.toOwnedSlice();

        if (builtin.mode == .Debug) {
            std.debug.print(">>>>>>> Lexer Debug Info <<<<<<<\n\n", .{});
            for (tokens) |token| {
                std.debug.print(
                    "Type: {any} Literal: {s}\n",
                    .{ token.token_type, Token.toLiteral(source, token) },
                );
                std.debug.print("\t{any}\n", .{token});
            }
        }

        return Self{
            .source = source,
            .tokens = tokens,
            .root = undefined,
            .errors = std.ArrayList(AstError).init(allocator),
            .nodes = std.ArrayList(Node).init(allocator),
        };
    }

    pub fn parseRoot(self: *Self) AllocatorError!void {
        self.root = self.parseExpression() catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ParseError => return,
        };
        if (builtin.mode == .Debug) {
            std.debug.print("\n\n>>>>>>> Parser Debug Info <<<<<<<\n\n", .{});
            std.debug.print("{any}\n", .{self.nodes.items});
        }
    }

    fn parseExpression(self: *Self) Error!NodeIndex {
        return try self.term();
    }

    fn term(self: *Self) !NodeIndex {
        var expr = try self.factor();
        while (true) {
            const current_token = self.getCurrentToken();
            switch (current_token.token_type) {
                Token.Type.PLUS,
                Token.Type.MINUS,
                => {
                    self.advance();
                    expr = try self.addNode(
                        Expression{
                            .binary = .{
                                .left = expr,
                                .operator = current_token,
                                .right = try self.factor(),
                            },
                        },
                    );
                },
                else => return expr,
            }
        }
    }

    fn factor(self: *Self) !NodeIndex {
        var expr = try self.unary();
        while (true) {
            const current_token = self.getCurrentToken();
            switch (current_token.token_type) {
                Token.Type.SLASH,
                Token.Type.STAR,
                => {
                    self.advance();
                    expr = try self.addNode(
                        Expression{
                            .binary = .{
                                .left = expr,
                                .operator = current_token,
                                .right = try self.unary(),
                            },
                        },
                    );
                },
                else => return expr,
            }
        }
    }

    fn unary(self: *Self) !NodeIndex {
        const current_token = self.getCurrentToken();
        switch (current_token.token_type) {
            Token.Type.BANG,
            Token.Type.MINUS,
            => {
                self.advance();
                return try self.addNode(
                    Expression{
                        .unary = .{
                            .operator = current_token,
                            .right = try self.unary(),
                        },
                    },
                );
            },
            else => return try self.primary(),
        }
    }

    fn primary(self: *Self) !NodeIndex {
        const current_token = self.getCurrentToken();
        const literal = switch (current_token.token_type) {
            Token.Type.TRUE => Expression.Literal{ .boolean = true },
            Token.Type.FALSE => Expression.Literal{ .boolean = false },
            Token.Type.NUMBER => blk: {
                const value = std.fmt.parseFloat(
                    f64,
                    Token.toLiteral(self.source, current_token),
                ) catch unreachable;
                break :blk Expression.Literal{ .number = value };
            },
            Token.Type.STRING => Expression.Literal{
                .string = Token.toLiteral(self.source, current_token),
            },
            Token.Type.NIL => Expression.Literal{ .nil = {} },
            Token.Type.LEFT_PAREN => {
                self.advance();
                const expr = try self.parseExpression();
                const current = self.getCurrentToken();
                if (current.token_type != Token.Type.RIGHT_PAREN) {
                    return self.addError(
                        AstError.Type.expected_token,
                        Token.Type.RIGHT_PAREN,
                    );
                }
                self.advance();
                return expr;
            },
            else => {
                return self.addError(
                    AstError.Type.expected_expression,
                    null,
                );
            },
        };
        self.advance();
        return try self.addNode(
            Expression{
                .literal = literal,
            },
        );
    }

    fn addError(
        self: *Self,
        error_type: AstError.Type,
        expected_token_type: ?Token.Type,
    ) Error {
        try self.errors.append(AstError{
            .error_type = error_type,
            .current_token = self.getCurrentToken(),
            .expected_token_type = expected_token_type,
        });
        return Error.ParseError;
    }

    fn addNode(self: *Self, node: Node) !NodeIndex {
        const index = @intCast(NodeIndex, self.nodes.items.len);
        try self.nodes.append(node);
        return index;
    }

    fn advance(self: *Self) void {
        const current_token = self.getCurrentToken();
        if (current_token.token_type != Token.Type.EOF) {
            self.current += 1;
        }
    }

    fn getCurrentToken(self: Self) Token {
        return self.tokens[self.current];
    }
};
