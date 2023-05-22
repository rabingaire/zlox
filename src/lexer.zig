const std = @import("std");

const MyError = error{RandomError};

const TokenType = enum {
    // Single-character tokens.
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,

    // One or two character tokens.
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,

    // Literals.
    IDENTIFIER,
    STRING,
    NUMBER,

    // Keywords.
    AND,
    CLASS,
    ELSE,
    FALSE,
    FUN,
    FOR,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    EOF,
};

const Token = struct {
    const Self = @This();

    token_type: TokenType,
    start: u32,
    end: u32,
    line: u32,
    column: u32,

    pub fn init(token_type: TokenType, start: u32, end: u32, line: u32, column: u32) Self {
        return Self{
            .token_type = token_type,
            .start = start,
            .end = end,
            .line = line,
            .column = column,
        };
    }
};

pub const Scanner = struct {
    const Self = @This();

    source: []const u8,
    tokens: std.ArrayList(Token),
    start: u32 = 0,
    current: u32 = 0,
    line: u32 = 1,
    column: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Self {
        return Self{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn scanTokens(self: *Self) ![]Token {
        errdefer self.tokens.deinit();

        while (!self.isAtEnd()) {
            self.start = self.current;
            const ch = self.advance();
            switch (ch) {
                ' ', '\r', '\t' => {},
                '\n' => self.newline(),
                '(' => try self.addToken(TokenType.LEFT_PAREN, 1),
                ')' => try self.addToken(TokenType.RIGHT_PAREN, 1),
                '{' => try self.addToken(TokenType.LEFT_BRACE, 1),
                '}' => try self.addToken(TokenType.RIGHT_BRACE, 1),
                ',' => try self.addToken(TokenType.COMMA, 1),
                '.' => try self.addToken(TokenType.DOT, 1),
                '-' => try self.addToken(TokenType.MINUS, 1),
                '+' => try self.addToken(TokenType.PLUS, 1),
                ';' => try self.addToken(TokenType.SEMICOLON, 1),
                '*' => try self.addToken(TokenType.STAR, 1),
                '!' => {
                    if (self.match('=')) {
                        try self.addToken(TokenType.BANG_EQUAL, 2);
                    } else {
                        try self.addToken(TokenType.BANG, 1);
                    }
                },
                '=' => {
                    if (self.match('=')) {
                        try self.addToken(TokenType.EQUAL_EQUAL, 2);
                    } else {
                        try self.addToken(TokenType.EQUAL, 1);
                    }
                },
                '<' => {
                    if (self.match('=')) {
                        try self.addToken(TokenType.LESS_EQUAL, 2);
                    } else {
                        try self.addToken(TokenType.LESS, 1);
                    }
                },
                '>' => {
                    if (self.match('=')) {
                        try self.addToken(TokenType.GREATER_EQUAL, 2);
                    } else {
                        try self.addToken(TokenType.GREATER, 1);
                    }
                },
                '/' => {
                    if (self.match('/')) {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
                        }
                    } else {
                        try self.addToken(TokenType.SLASH, 1);
                    }
                },
                '"' => try self.parseString(),
                0 => try self.addToken(TokenType.EOF, 1),
                else => {
                    if (isDigit(ch)) {
                        try self.parseNumber();
                        continue;
                    }

                    std.debug.print(
                        "Unexpected character '{c}' at line {d} column {d}\n",
                        .{ ch, self.line, self.column },
                    );
                    std.os.exit(65);
                },
            }
        }
        return try self.tokens.toOwnedSlice();
    }

    fn addToken(self: *Self, token_type: TokenType, length: u32) !void {
        const token = Token.init(
            token_type,
            self.start,
            (self.start + length) - 1,
            self.line,
            self.column,
        );
        try self.tokens.append(token);
    }

    fn parseString(self: *Self) !void {
        const start_line = self.line;
        const start_column = self.column;

        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.line += 1;
            }
            _ = self.advance();
        }

        if (self.isAtEnd()) {
            std.debug.print(
                "Unterminated string literal starting at line {d} column {d}\n",
                .{ start_line, start_column },
            );
            std.os.exit(65);
        }

        const token = Token.init(
            TokenType.STRING,
            self.start + 1,
            self.current - 1,
            start_line,
            start_column,
        );
        try self.tokens.append(token);

        _ = self.advance();
    }

    fn parseNumber(self: *Self) !void {
        while (isDigit(self.peek())) _ = self.advance();

        if (self.peek() == '.') {
            _ = self.advance();

            while (isDigit(self.peek())) _ = self.advance();
        }

        try self.addToken(TokenType.NUMBER, self.current - self.start);
    }

    fn advance(self: *Self) u8 {
        self.column += 1;
        const ch = self.source[self.current];
        self.current += 1;
        return ch;
    }

    fn peek(self: *const Self) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn newline(self: *Self) void {
        self.line += 1;
        self.column = 0;
    }

    fn isDigit(ch: u8) bool {
        return ch >= '0' and ch <= '9';
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        const ch = self.source[self.current];
        if (ch != expected) return false;
        _ = self.advance();
        return true;
    }

    fn isAtEnd(self: *const Self) bool {
        return self.current >= self.source.len;
    }
};
