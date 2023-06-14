const std = @import("std");
const builtin = @import("builtin");

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const Scanner = lexer.Scanner;
const ast = @import("ast.zig");
const Ast = ast.Ast;
const Expression = ast.Expression;

pub const Parser = struct {
    const Self = @This();

    scanner: Scanner,
    tokens: []Token,
    allocator: std.mem.Allocator,
    current: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) !Self {
        var scanner = Scanner.init(allocator, source);
        const tokens = try scanner.scanTokens();

        if (builtin.mode == .Debug) {
            std.debug.print(">>>>>>> Lexer Debug Info <<<<<<<\n\n", .{});
            for (tokens) |token| {
                std.debug.print(
                    "Type: {any} Literal: {s}\n",
                    .{ token.token_type, scanner.toLiteral(token) },
                );
                std.debug.print("\t{any}\n", .{token});
            }
        }

        return Self{
            .allocator = allocator,
            .scanner = scanner,
            .tokens = tokens,
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.tokens);
    }

    pub fn parse(self: *Self) !Ast {
        return Ast{
            .allocator = self.allocator,
            .root = try self.term(),
        };
    }

    fn term(self: *Self) !*Expression {
        var expr = try self.factor();
        while (true) {
            const current_token = self.get_current_token();
            switch (current_token.token_type) {
                Token.Type.PLUS,
                Token.Type.MINUS,
                => {
                    self.advance();
                    expr.addBinary(
                        try expr.copy(self.allocator),
                        current_token,
                        try self.factor(),
                    );
                },
                else => return expr,
            }
        }
    }

    fn factor(self: *Self) !*Expression {
        var expr = try self.unary();
        while (true) {
            const current_token = self.get_current_token();
            switch (current_token.token_type) {
                Token.Type.SLASH,
                Token.Type.STAR,
                => {
                    self.advance();
                    expr.addBinary(
                        try expr.copy(self.allocator),
                        current_token,
                        try self.unary(),
                    );
                },
                else => return expr,
            }
        }
    }

    fn unary(self: *Self) !*Expression {
        const current_token = self.get_current_token();
        switch (current_token.token_type) {
            Token.Type.BANG,
            Token.Type.MINUS,
            => {
                self.advance();
                var expr = try Expression.create(self.allocator);
                expr.addUnary(current_token, try self.unary());
                return expr;
            },
            else => return try self.primary(),
        }
    }

    fn primary(self: *Self) !*Expression {
        const current_token = self.get_current_token();
        var literal = switch (current_token.token_type) {
            Token.Type.TRUE => Expression.Literal{ .boolean = true },
            Token.Type.FALSE => Expression.Literal{ .boolean = false },
            Token.Type.NUMBER => blk: {
                const value = std.fmt.parseFloat(
                    f64,
                    self.scanner.toLiteral(current_token),
                ) catch unreachable;
                break :blk Expression.Literal{ .number = value };
            },
            Token.Type.STRING => Expression.Literal{
                .string = self.scanner.toLiteral(current_token),
            },
            Token.Type.NIL => Expression.Literal{ .nil = {} },
            else => unreachable,
        };
        self.advance();
        var expr = try Expression.create(self.allocator);
        expr.addLiteral(literal);
        return expr;
    }

    fn advance(self: *Self) void {
        const current_token = self.get_current_token();
        if (current_token.token_type != Token.Type.EOF) {
            self.current += 1;
        }
    }

    fn get_current_token(self: Self) Token {
        return self.tokens[self.current];
    }
};
