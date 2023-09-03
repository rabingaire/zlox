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

pub const Error = error{ParseError} || AllocatorError;

pub const Parser = struct {
    const Self = @This();

    source: [:0]const u8,
    tokens: []Token,
    errors: std.ArrayList(AstError),
    nodes: std.ArrayList(Node),
    root: NodeIndex,
    allocator: std.mem.Allocator,
    current: u32 = 0,
    depth: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) Error!Self {
        var scanner = Scanner.init(allocator, source);
        defer scanner.tokens.deinit();

        try scanner.scanTokens();

        const tokens = try scanner.tokens.toOwnedSlice();

        if (builtin.mode == .Debug and !builtin.is_test) {
            std.debug.print("Lexer Debug Info:\n", .{});
            for (tokens) |token| {
                std.debug.print(
                    "Type: {any} Literal: {s}\n\t{any}\n",
                    .{ token.token_type, Token.toLiteral(source, token), token },
                );
            }
        }

        return Self{
            .source = source,
            .tokens = tokens,
            .root = undefined,
            .allocator = allocator,
            .errors = std.ArrayList(AstError).init(allocator),
            .nodes = std.ArrayList(Node).init(allocator),
        };
    }

    pub fn parseRoot(self: *Self) AllocatorError!void {
        self.root = self.parseStatements() catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.ParseError => return,
        };

        if (builtin.mode == .Debug and !builtin.is_test) {
            std.debug.print(
                "\n\nParser Debug Info:\n{any}\n",
                .{self.nodes.items},
            );
        }
    }

    fn parseStatements(self: *Self) Error!NodeIndex {
        var statements = std.ArrayList(NodeIndex).init(self.allocator);
        defer statements.deinit();

        while (!self.isAtEndToken()) {
            const statement_node = try self.parseStatementOrExpression();
            try statements.append(statement_node);

            const current_token = self.getCurrentToken();
            switch (current_token.token_type) {
                Token.Type.EOF => {},
                Token.Type.SEMICOLON => {
                    self.advance();
                },
                else => {},
            }
        }
        return try self.addNode(.{
            .program = try statements.toOwnedSlice(),
        });
    }

    fn parseStatementOrExpression(self: *Self) Error!NodeIndex {
        const current_token = self.getCurrentToken();
        switch (current_token.token_type) {
            Token.Type.VAR => {
                self.advance();
                return try self.parseVariable();
            },
            Token.Type.PRINT => {
                self.advance();
                return try self.parsePrint();
            },
            else => {
                return try self.parseExpression();
            },
        }
    }

    fn parsePrint(self: *Self) Error!NodeIndex {
        return self.addNode(.{
            .print = try self.parseExpression(),
        });
    }

    fn parseVariable(self: *Self) Error!NodeIndex {
        // Parse identifier
        var ident_token = self.getCurrentToken();
        if (ident_token.token_type != Token.Type.IDENTIFIER) {
            return self.addError(
                AstError.Type.expected_token,
                Token.Type.IDENTIFIER,
            );
        }

        // Parse assignment operator
        self.advance();
        if (self.getCurrentToken().token_type != Token.Type.EQUAL) {
            return self.addError(
                AstError.Type.expected_token,
                Token.Type.EQUAL,
            );
        }

        // Parse expression
        self.advance();
        return try self.addNode(
            .{
                .variable = .{
                    .symbol = ident_token,
                    .value = try self.parseExpression(),
                },
            },
        );
    }

    fn parseExpression(self: *Self) Error!NodeIndex {
        return try self.logicalOr();
    }

    fn logicalOr(self: *Self) !NodeIndex {
        var expr = try self.logicalAnd();
        while (true) {
            const current_token = self.getCurrentToken();
            switch (current_token.token_type) {
                Token.Type.OR => {
                    self.advance();
                    expr = try self.addNode(
                        .{
                            .expression = .{
                                .binary = .{
                                    .left = expr,
                                    .operator = current_token,
                                    .right = try self.logicalAnd(),
                                },
                            },
                        },
                    );
                },
                else => return expr,
            }
        }
    }

    fn logicalAnd(self: *Self) !NodeIndex {
        var expr = try self.comparison();
        while (true) {
            const current_token = self.getCurrentToken();
            switch (current_token.token_type) {
                Token.Type.AND => {
                    self.advance();
                    expr = try self.addNode(
                        .{
                            .expression = .{
                                .binary = .{
                                    .left = expr,
                                    .operator = current_token,
                                    .right = try self.comparison(),
                                },
                            },
                        },
                    );
                },
                else => return expr,
            }
        }
    }

    fn comparison(self: *Self) !NodeIndex {
        var expr = try self.term();
        while (true) {
            const current_token = self.getCurrentToken();
            switch (current_token.token_type) {
                Token.Type.BANG_EQUAL,
                Token.Type.EQUAL_EQUAL,
                Token.Type.GREATER,
                Token.Type.GREATER_EQUAL,
                Token.Type.LESS,
                Token.Type.LESS_EQUAL,
                => {
                    self.advance();
                    expr = try self.addNode(
                        .{
                            .expression = .{
                                .binary = .{
                                    .left = expr,
                                    .operator = current_token,
                                    .right = try self.term(),
                                },
                            },
                        },
                    );
                },
                else => return expr,
            }
        }
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
                        .{
                            .expression = .{
                                .binary = .{
                                    .left = expr,
                                    .operator = current_token,
                                    .right = try self.factor(),
                                },
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
                        .{
                            .expression = .{
                                .binary = .{
                                    .left = expr,
                                    .operator = current_token,
                                    .right = try self.unary(),
                                },
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
                    .{
                        .expression = .{
                            .unary = .{
                                .operator = current_token,
                                .right = try self.unary(),
                            },
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
            Token.Type.TRUE => Node.Expression.Literal{ .boolean = true },
            Token.Type.FALSE => Node.Expression.Literal{ .boolean = false },
            Token.Type.NUMBER => blk: {
                const value = std.fmt.parseFloat(
                    f64,
                    Token.toLiteral(self.source, current_token),
                ) catch unreachable;
                break :blk Node.Expression.Literal{ .number = value };
            },
            Token.Type.STRING => blk: {
                const value = try std.fmt.allocPrint(
                    self.allocator,
                    "{s}",
                    .{Token.toLiteral(self.source, current_token)},
                );
                break :blk Node.Expression.Literal{
                    .string = value,
                };
            },
            Token.Type.NIL => Node.Expression.Literal{ .nil = {} },
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
            Token.Type.LEFT_BRACE => {
                self.advance();
                self.depth += 1;
                return try self.parseBlockStatements();
            },
            Token.Type.IF => {
                self.advance();

                var l_paren_token = self.getCurrentToken();
                if (l_paren_token.token_type != Token.Type.LEFT_PAREN) {
                    return self.addError(
                        AstError.Type.expected_token,
                        Token.Type.LEFT_PAREN,
                    );
                }
                self.advance();

                const condition = try self.parseExpression();

                var r_paren_token = self.getCurrentToken();
                if (r_paren_token.token_type != Token.Type.RIGHT_PAREN) {
                    return self.addError(
                        AstError.Type.expected_token,
                        Token.Type.RIGHT_PAREN,
                    );
                }
                self.advance();

                const if_expression = try self.parseExpression();

                var else_expression: NodeIndex = undefined;
                var else_token = self.getCurrentToken();
                if (else_token.token_type == Token.Type.ELSE) {
                    self.advance();
                    else_expression = try self.parseExpression();
                } else {
                    else_expression = try self.addNode(.{
                        .expression = .{
                            .literal = .{
                                .nil = {},
                            },
                        },
                    });
                }

                return try self.addNode(.{
                    .expression = .{
                        .conditional = .{
                            .condition = condition,
                            .if_expression = if_expression,
                            .else_expression = else_expression,
                        },
                    },
                });
            },
            Token.Type.IDENTIFIER => blk: {
                break :blk Node.Expression.Literal{
                    .ident = current_token,
                };
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
            .{
                .expression = .{ .literal = literal },
            },
        );
    }

    fn parseBlockStatements(self: *Self) Error!NodeIndex {
        var statements = std.ArrayList(NodeIndex).init(self.allocator);
        defer statements.deinit();

        var seen_return = false;
        var return_expression_index: ?NodeIndex = null;

        while (!self.isAtEndToken()) {
            const current_token = self.getCurrentToken();
            switch (current_token.token_type) {
                Token.Type.SEMICOLON => {
                    self.advance(); // skipping semicolon
                },
                Token.Type.RETURN => {
                    defer {
                        seen_return = true;
                    }
                    self.advance();
                    return_expression_index = try self.parseExpression();
                },
                Token.Type.RIGHT_BRACE => {
                    defer {
                        self.depth -= 1;
                        self.advance();
                    }

                    return try self.addNode(.{
                        .expression = .{
                            .block = .{
                                .depth = self.depth,
                                .statement_indexes = try statements.toOwnedSlice(),
                                .return_index = return_expression_index orelse try self.addNode(.{
                                    .expression = .{
                                        .literal = .{
                                            .nil = {},
                                        },
                                    },
                                }),
                            },
                        },
                    });
                },
                else => {
                    const statement_node = try self.parseStatementOrExpression();
                    if (!seen_return) {
                        try statements.append(statement_node);
                    }
                },
            }
        }
        return self.addError(
            AstError.Type.expected_token,
            Token.Type.RIGHT_BRACE,
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
        const index: NodeIndex = @intCast(self.nodes.items.len);
        try self.nodes.append(node);
        return index;
    }

    fn advance(self: *Self) void {
        if (!isAtEndToken(self)) {
            self.current += 1;
        }
    }

    fn isAtEndToken(self: *Self) bool {
        const current_token = self.getCurrentToken();
        return current_token.token_type == Token.Type.EOF;
    }

    fn getCurrentToken(self: Self) Token {
        return self.tokens[self.current];
    }
};

test "check if parser detects parse errors correctly" {
    try testError(
        \\ "hello" +
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ var a = (("hello" + "world") +;
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ print (("hello" + "world") +;
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ print 5 - 3 *;
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ print + 5 - 3;
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ var a : 12
    ,
        &.{.expected_token},
    );

    try testError(
        \\ var 12 = 13
    ,
        &.{.expected_token},
    );

    try testError(
        \\ var a = ((2 + 3) + 4;
    ,
        &.{.expected_token},
    );

    try testError(
        \\ print ("world" + "cat";
    ,
        &.{.expected_token},
    );

    try testError(
        \\ { print "world"
    ,
        &.{.expected_token},
    );

    try testError(
        \\ } print "world"
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ {
        \\ print "world"
        \\ {
        \\      print "hello"
        \\ }
    ,
        &.{.expected_token},
    );

    try testError(
        \\ var a = (1 > 2) and
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ var a = (1 < 2) or
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ var a = ("Hello" == "World") and (1 == 2) or
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ var a = if "Hello" == "World") 42
    ,
        &.{.expected_token},
    );

    try testError(
        \\ var a = if ("Hello" == "World" 34
    ,
        &.{.expected_token},
    );

    try testError(
        \\ var a = if ("Hello" == "World") else 32
    ,
        &.{.expected_expression},
    );

    try testError(
        \\ var a = if ("Hello" == "World") 2 + 2 else
    ,
        &.{.expected_expression},
    );
}

fn testError(source: [:0]const u8, expected_errors: []const AstError.Type) !void {
    var tree = try Ast.parse(std.testing.allocator, source);
    defer tree.deinit();

    std.testing.expect(tree.hasErrors()) catch |err| {
        std.debug.print("expected errors: {any}\n", .{expected_errors});
        return err;
    };

    for (expected_errors, 0..) |expected, i| {
        try std.testing.expectEqual(expected, tree.errors[i].error_type);
    }
}
