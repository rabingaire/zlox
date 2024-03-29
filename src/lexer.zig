const std = @import("std");

pub const Token = struct {
    const Self = @This();

    pub const Type = enum {
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
        ILLEGAL,

        pub fn toLiteral(token_type: Token.Type) ?[]const u8 {
            return switch (token_type) {
                .LEFT_PAREN => "(",
                .RIGHT_PAREN => ")",
                .LEFT_BRACE => "{",
                .RIGHT_BRACE => "}",
                .COMMA => ",",
                .DOT => ".",
                .MINUS => "-",
                .PLUS => "+",
                .SEMICOLON => ";",
                .SLASH => "/",
                .STAR => "*",
                .BANG => "!",
                .BANG_EQUAL => "!=",
                .EQUAL => "=",
                .EQUAL_EQUAL => "==",
                .GREATER => ">",
                .GREATER_EQUAL => ">=",
                .LESS => "<",
                .LESS_EQUAL => "<=",
                .AND => "and",
                .CLASS => "class",
                .ELSE => "else",
                .FALSE => "false",
                .FUN => "fun",
                .FOR => "for",
                .IF => "if",
                .NIL => "nil",
                .OR => "or",
                .PRINT => "print",
                .RETURN => "return",
                .SUPER => "super",
                .THIS => "this",
                .TRUE => "true",
                .VAR => "var",
                .WHILE => "while",
                .EOF => "eof",
                else => null,
            };
        }

        pub fn toLiteralForLiteralType(token_type: Token.Type) ?[]const u8 {
            return switch (token_type) {
                .IDENTIFIER => "IDENTIFIER",
                .STRING => "STRING",
                .NUMBER => "NUMBER",
                else => null,
            };
        }
    };

    const keywords = std.ComptimeStringMap(Type, .{
        .{ "and", .AND },
        .{ "class", .CLASS },
        .{ "else", .ELSE },
        .{ "false", .FALSE },
        .{ "for", .FOR },
        .{ "fun", .FUN },
        .{ "if", .IF },
        .{ "nil", .NIL },
        .{ "or", .OR },
        .{ "print", .PRINT },
        .{ "return", .RETURN },
        .{ "super", .SUPER },
        .{ "this", .THIS },
        .{ "true", .TRUE },
        .{ "var", .VAR },
        .{ "while", .WHILE },
    });

    pub fn toLiteral(source: [:0]const u8, token: Token) []const u8 {
        return Token.Type.toLiteral(token.token_type) orelse {
            return source[token.start..(token.end + 1)];
        };
    }

    fn getKeyword(literal: []const u8) ?Type {
        return keywords.get(literal);
    }

    token_type: Type,
    start: u32,
    end: u32,
    line: u32,
    column: u32,

    fn init(token_type: Type, start: u32, end: u32, line: u32, column: u32) Self {
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

    source: [:0]const u8,
    tokens: std.ArrayList(Token),
    start: u32 = 0,
    current: u32 = 0,
    line: u32 = 1,
    column: u32 = 0,

    pub fn init(allocator: std.mem.Allocator, source: [:0]const u8) Self {
        return Self{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
        };
    }

    pub fn scanTokens(self: *Self) std.mem.Allocator.Error!void {
        while (!self.isAtEnd()) {
            self.start = self.current;
            const ch = self.getCurrentCharAndAdvance();
            switch (ch) {
                ' ', '\r', '\t' => {},
                '\n' => {
                    const last_token = self.tokens.getLast();
                    switch (last_token.token_type) {
                        Token.Type.SEMICOLON,
                        Token.Type.LEFT_BRACE,
                        Token.Type.RIGHT_BRACE,
                        => {},
                        else => {
                            try self.addToken(.SEMICOLON, 1);
                        },
                    }
                    self.newline();
                },
                '(' => try self.addToken(.LEFT_PAREN, 1),
                ')' => try self.addToken(.RIGHT_PAREN, 1),
                '{' => try self.addToken(.LEFT_BRACE, 1),
                '}' => try self.addToken(.RIGHT_BRACE, 1),
                ',' => try self.addToken(.COMMA, 1),
                '.' => try self.addToken(.DOT, 1),
                '-' => try self.addToken(.MINUS, 1),
                '+' => try self.addToken(.PLUS, 1),
                ';' => try self.addToken(.SEMICOLON, 1),
                '*' => try self.addToken(.STAR, 1),
                '!' => {
                    if (self.match('=')) {
                        try self.addToken(.BANG_EQUAL, 2);
                    } else {
                        try self.addToken(.BANG, 1);
                    }
                },
                '=' => {
                    if (self.match('=')) {
                        try self.addToken(.EQUAL_EQUAL, 2);
                    } else {
                        try self.addToken(.EQUAL, 1);
                    }
                },
                '<' => {
                    if (self.match('=')) {
                        try self.addToken(.LESS_EQUAL, 2);
                    } else {
                        try self.addToken(.LESS, 1);
                    }
                },
                '>' => {
                    if (self.match('=')) {
                        try self.addToken(.GREATER_EQUAL, 2);
                    } else {
                        try self.addToken(.GREATER, 1);
                    }
                },
                '/' => {
                    if (self.match('/')) {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            self.advance();
                        }
                        if (self.isAtEnd()) {
                            try self.addToken(.EOF, 1);
                        }
                    } else {
                        try self.addToken(.SLASH, 1);
                    }
                },
                '"' => try self.parseString(),
                0 => try self.addToken(.EOF, 1),
                else => {
                    if (isDigit(ch)) {
                        try self.parseNumber();
                        continue;
                    }

                    if (isAlpha(ch)) {
                        try self.parseIdentifier();
                        continue;
                    }

                    try self.addToken(.ILLEGAL, 1);
                },
            }
        }
    }

    fn addToken(self: *Self, token_type: Token.Type, length: u32) !void {
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
            if (self.getCurrentCharAndAdvance() == '\n') {
                self.newline();
            }
        }

        const token = Token.init(
            .STRING,
            self.start + 1,
            self.current - 1,
            start_line,
            start_column,
        );
        try self.tokens.append(token);

        if (self.isAtEnd()) {
            try self.addToken(.EOF, 1);
        } else {
            self.advance(); // eat end double qote (")
        }
    }

    fn parseNumber(self: *Self) !void {
        const start_line = self.line;
        const start_column = self.column;

        while (isDigit(self.peek())) self.advance();

        if (self.peek() == '.') {
            self.advance();

            while (isDigit(self.peek())) self.advance();
        }

        const token = Token.init(
            .NUMBER,
            self.start,
            self.current - 1,
            start_line,
            start_column,
        );
        try self.tokens.append(token);
    }

    fn parseIdentifier(self: *Self) !void {
        const start_line = self.line;
        const start_column = self.column;

        while (isAlpha(self.peek()) or isDigit(self.peek())) self.advance();

        var token = Token.init(
            .IDENTIFIER,
            self.start,
            self.current - 1,
            start_line,
            start_column,
        );
        if (Token.getKeyword(
            Token.toLiteral(self.source, token),
        )) |token_type| {
            token.token_type = token_type;
        }
        try self.tokens.append(token);
    }

    fn getCurrentCharAndAdvance(self: *Self) u8 {
        const ch = self.source[self.current];
        self.advance();
        return ch;
    }

    fn advance(self: *Self) void {
        self.column += 1;
        self.current += 1;
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

    fn isAlpha(ch: u8) bool {
        return ((ch >= 'a' and ch <= 'z') or
            (ch >= 'A' and ch <= 'Z') or
            ch == '_');
    }

    fn match(self: *Self, expected: u8) bool {
        if (self.isAtEnd()) return false;
        const ch = self.source[self.current];
        if (ch != expected) return false;
        self.advance();
        return true;
    }

    fn isAtEnd(self: *const Self) bool {
        return self.current > self.source.len;
    }
};

test "check if lexer is correct" {
    const source =
        \\class Breakfast {
        \\  cook() {
        \\    print "Eggs a-fryin' at 19.28!";
        \\  }
        \\
        \\  serve(who) {
        \\    print "Enjoy your breakfast, " + who + ".";
        \\  }
        \\}
        \\
        \\var breakfast = Breakfast();
        \\print breakfast; // "Breakfast instance".
        \\var randomnumber = 1.234;
        \\// cool comment
        \\var multi_line_string = "
        \\I love zig
        \\";^
    ;
    const allocator = std.testing.allocator;

    var scanner = Scanner.init(allocator, source);
    try scanner.scanTokens();
    defer scanner.tokens.deinit();

    try std.testing.expect(47 == scanner.tokens.items.len);

    const expected_tokens = [_]Token{
        .{ .token_type = Token.Type.CLASS, .start = 0, .end = 4, .line = 1, .column = 1 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 6, .end = 14, .line = 1, .column = 7 },
        .{ .token_type = Token.Type.LEFT_BRACE, .start = 16, .end = 16, .line = 1, .column = 17 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 20, .end = 23, .line = 2, .column = 3 },
        .{ .token_type = Token.Type.LEFT_PAREN, .start = 24, .end = 24, .line = 2, .column = 7 },
        .{ .token_type = Token.Type.RIGHT_PAREN, .start = 25, .end = 25, .line = 2, .column = 8 },
        .{ .token_type = Token.Type.LEFT_BRACE, .start = 27, .end = 27, .line = 2, .column = 10 },
        .{ .token_type = Token.Type.PRINT, .start = 33, .end = 37, .line = 3, .column = 5 },
        .{ .token_type = Token.Type.STRING, .start = 40, .end = 62, .line = 3, .column = 11 },
        .{ .token_type = Token.Type.SEMICOLON, .start = 64, .end = 64, .line = 3, .column = 36 },
        .{ .token_type = Token.Type.RIGHT_BRACE, .start = 68, .end = 68, .line = 4, .column = 3 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 73, .end = 77, .line = 6, .column = 3 },
        .{ .token_type = Token.Type.LEFT_PAREN, .start = 78, .end = 78, .line = 6, .column = 8 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 79, .end = 81, .line = 6, .column = 9 },
        .{ .token_type = Token.Type.RIGHT_PAREN, .start = 82, .end = 82, .line = 6, .column = 12 },
        .{ .token_type = Token.Type.LEFT_BRACE, .start = 84, .end = 84, .line = 6, .column = 14 },
        .{ .token_type = Token.Type.PRINT, .start = 90, .end = 94, .line = 7, .column = 5 },
        .{ .token_type = Token.Type.STRING, .start = 97, .end = 118, .line = 7, .column = 11 },
        .{ .token_type = Token.Type.PLUS, .start = 121, .end = 121, .line = 7, .column = 36 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 123, .end = 125, .line = 7, .column = 38 },
        .{ .token_type = Token.Type.PLUS, .start = 127, .end = 127, .line = 7, .column = 42 },
        .{ .token_type = Token.Type.STRING, .start = 130, .end = 130, .line = 7, .column = 44 },
        .{ .token_type = Token.Type.SEMICOLON, .start = 132, .end = 132, .line = 7, .column = 47 },
        .{ .token_type = Token.Type.RIGHT_BRACE, .start = 136, .end = 136, .line = 8, .column = 3 },
        .{ .token_type = Token.Type.RIGHT_BRACE, .start = 138, .end = 138, .line = 9, .column = 1 },
        .{ .token_type = Token.Type.VAR, .start = 141, .end = 143, .line = 11, .column = 1 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 145, .end = 153, .line = 11, .column = 5 },
        .{ .token_type = Token.Type.EQUAL, .start = 155, .end = 155, .line = 11, .column = 15 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 157, .end = 165, .line = 11, .column = 17 },
        .{ .token_type = Token.Type.LEFT_PAREN, .start = 166, .end = 166, .line = 11, .column = 26 },
        .{ .token_type = Token.Type.RIGHT_PAREN, .start = 167, .end = 167, .line = 11, .column = 27 },
        .{ .token_type = Token.Type.SEMICOLON, .start = 168, .end = 168, .line = 11, .column = 28 },
        .{ .token_type = Token.Type.PRINT, .start = 170, .end = 174, .line = 12, .column = 1 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 176, .end = 184, .line = 12, .column = 7 },
        .{ .token_type = Token.Type.SEMICOLON, .start = 185, .end = 185, .line = 12, .column = 16 },
        .{ .token_type = Token.Type.VAR, .start = 212, .end = 214, .line = 13, .column = 1 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 216, .end = 227, .line = 13, .column = 5 },
        .{ .token_type = Token.Type.EQUAL, .start = 229, .end = 229, .line = 13, .column = 18 },
        .{ .token_type = Token.Type.NUMBER, .start = 231, .end = 235, .line = 13, .column = 20 },
        .{ .token_type = Token.Type.SEMICOLON, .start = 236, .end = 236, .line = 13, .column = 25 },
        .{ .token_type = Token.Type.VAR, .start = 254, .end = 256, .line = 15, .column = 1 },
        .{ .token_type = Token.Type.IDENTIFIER, .start = 258, .end = 274, .line = 15, .column = 5 },
        .{ .token_type = Token.Type.EQUAL, .start = 276, .end = 276, .line = 15, .column = 23 },
        .{ .token_type = Token.Type.STRING, .start = 279, .end = 290, .line = 15, .column = 25 },
        .{ .token_type = Token.Type.SEMICOLON, .start = 292, .end = 292, .line = 17, .column = 2 },
        .{ .token_type = Token.Type.ILLEGAL, .start = 293, .end = 293, .line = 17, .column = 3 },
        .{ .token_type = Token.Type.EOF, .start = 294, .end = 294, .line = 17, .column = 4 },
    };

    const expected_literals = [_][]const u8{
        "class",
        "Breakfast",
        "{",
        "cook",
        "(",
        ")",
        "{",
        "print",
        "Eggs a-fryin' at 19.28!",
        ";",
        "}",
        "serve",
        "(",
        "who",
        ")",
        "{",
        "print",
        "Enjoy your breakfast, ",
        "+",
        "who",
        "+",
        ".",
        ";",
        "}",
        "}",
        "var",
        "breakfast",
        "=",
        "Breakfast",
        "(",
        ")",
        ";",
        "print",
        "breakfast",
        ";",
        "var",
        "randomnumber",
        "=",
        "1.234",
        ";",
        "var",
        "multi_line_string",
        "=",
        \\
        \\I love zig
        \\
        ,
        ";",
        "^",
        "eof",
    };

    for (scanner.tokens.items, 0..) |token, index| {
        try std.testing.expectEqualDeep(
            expected_tokens[index],
            token,
        );
        try std.testing.expectEqualStrings(
            expected_literals[index],
            Token.toLiteral(source, token),
        );
    }
}
