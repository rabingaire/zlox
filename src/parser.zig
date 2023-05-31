const std = @import("std");
const builtin = @import("builtin");

const lexer = @import("lexer.zig");
const Token = lexer.Token;
const Scanner = lexer.Scanner;
const ast = @import("ast.zig");
const Expression = ast.Expression;
const BinaryExpression = ast.BinaryExpression;
const Literal = ast.Literal;

pub const Parser = struct {
    const Self = @This();

    scanner: Scanner,
    tokens: []Token,
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
            .scanner = scanner,
            .tokens = tokens,
        };
    }

    pub fn expression(self: *Self) Expression {
        return self.equality();
    }

    fn equality(self: *Self) Expression {
        // TODO: think about this pointers is making me confused
        var expr = self.literal();
        while (self.match(&[_]Token.Type{ Token.Type.BANG_EQUAL, Token.Type.EQUAL_EQUAL })) {
            const operator_token = self.advance();
            var left = expr;
            var right = self.literal();
            expr = Expression{
                .binary = BinaryExpression{
                    .left = &left,
                    .operator = operator_token,
                    .right = &right,
                },
            };
        }

        return expr;
    }

    fn literal(self: *Self) Expression {
        const literal_token = self.advance();
        return switch (literal_token.token_type) {
            Token.Type.NUMBER => ret: {
                const value = std.fmt.parseFloat(f32, self.scanner.toLiteral(literal_token)) catch unreachable;
                break :ret Expression{ .literal = Literal{ .number = value } };
            },
            else => unreachable,
        };
    }

    fn match(self: *Self, token_types: []const Token.Type) bool {
        for (token_types) |token_type| {
            if (self.check(token_type)) {
                return true;
            }
        }

        return false;
    }

    fn advance(self: *Self) Token {
        const current_token = self.peek();
        if (!self.isAtEnd()) {
            self.current += 1;
        }
        return current_token;
    }

    fn check(self: *const Self, token_type: Token.Type) bool {
        if (self.isAtEnd()) return false;
        return self.peek().token_type == token_type;
    }

    fn peek(self: *const Self) Token {
        return self.tokens[self.current];
    }

    fn isAtEnd(self: *const Self) bool {
        return self.peek().token_type == Token.Type.EOF;
    }
};
