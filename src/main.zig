const std = @import("std");

const ast = @import("ast.zig");
const Ast = ast.Ast;
const interpreter = @import("interpreter.zig");
const Interpreter = interpreter.Interpreter;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const allocator = gpa.allocator();
    var args = try std.process.argsWithAllocator(allocator);
    const args_count = args.inner.count;
    if (args_count > 2) {
        std.debug.print("Usage: zlox [usage]\n", .{});
        std.os.exit(0);
    }

    if (args_count == 2) {
        _ = args.skip();
        const file_name = args.next().?;

        const file_contents = try runFile(allocator, file_name);
        defer allocator.free(file_contents);

        try run(allocator, file_contents);
    }
}

fn runFile(allocator: std.mem.Allocator, file_name: []const u8) ![:0]const u8 {
    const file = try std.fs.cwd().openFile(file_name, .{});
    const size = @intCast(usize, try file.getEndPos());

    const buffer = try allocator.alloc(u8, size);
    errdefer allocator.free(buffer);

    const size_read = try file.readAll(buffer);
    std.debug.assert(size == size_read);

    buffer[size - 1] = 0;
    return buffer[0 .. size - 1 :0];
}

fn run(allocator: std.mem.Allocator, file_contents: [:0]const u8) !void {
    var tree = try Ast.parse(allocator, file_contents);
    defer tree.deinit();
    const literal = try Interpreter.evaluate(allocator, tree.root);
    Interpreter.print(allocator, literal);
}
