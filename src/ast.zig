const std = @import("std");
const lexer = @import("lexer.zig");
const Token = lexer.Token;

pub const BinaryExpression = struct {
    left: *Expression,
    operator: Token,
    right: *Expression,
};

pub const Literal = union(enum) {
    number: f64,
};

pub const Expression = union(enum) {
    binary: BinaryExpression,
    literal: Literal,
};
