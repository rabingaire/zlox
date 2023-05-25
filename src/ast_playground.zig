// INFO: code sample here is an concept of AST for zlox

const std = @import("std");

const IfExpession = struct {
    condition: *const Expression,
    true_block: BlockExpession,
    false_block: BlockExpession,
};

const BlockExpession = struct {
    statements: []const Statement,
    return_value: *const Expression,
};

const BinaryExpression = struct {
    left: *const Expression,
    operator: u8,
    right: *const Expression,
};

const Variable = struct {
    symbol: []const u8,
    value: *const Expression,
};

const Literal = union(enum) {
    number: f64,
    string: []const u8,
    boolean: bool,
};

const Expression = union(enum) {
    if_block: IfExpession,
    block: BlockExpession,
    literal: Literal,
    binary: BinaryExpression,
    symbol: []const u8,
};

const Statement = union(enum) { variable: Variable };

const Program = union(enum) { statements: []const Statement };

// var cute = true;
// var a = if (cute) { var b = "cat"; return b} else { var c = "dog"; return c};

pub fn main() !void {
    const program = Program{
        .statements = &[_]Statement{
            .{
                .variable = Variable{
                    .symbol = "cute",
                    .value = &Expression{
                        .literal = Literal{
                            .boolean = true,
                        },
                    },
                },
            },
            .{
                .variable = Variable{
                    .symbol = "a",
                    .value = &Expression{
                        .if_block = IfExpession{
                            .condition = &Expression{
                                .symbol = "cute",
                            },
                            .true_block = BlockExpession{
                                .statements = &[_]Statement{
                                    .{
                                        .variable = Variable{
                                            .symbol = "b",
                                            .value = &Expression{
                                                .literal = Literal{
                                                    .string = "cat",
                                                },
                                            },
                                        },
                                    },
                                },
                                .return_value = &Expression{
                                    .symbol = "b",
                                },
                            },
                            .false_block = BlockExpession{
                                .statements = &[_]Statement{
                                    .{
                                        .variable = Variable{
                                            .symbol = "c",
                                            .value = &Expression{
                                                .literal = Literal{
                                                    .string = "dog",
                                                },
                                            },
                                        },
                                    },
                                },
                                .return_value = &Expression{
                                    .symbol = "c",
                                },
                            },
                        },
                    },
                },
            },
        },
    };

    std.debug.print("{any}\n", .{program});
}
