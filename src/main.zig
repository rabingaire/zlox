const std = @import("std");

const lexer = @import("lexer.zig");

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

// TODO: fix type here figure out why can't I use [:0]const u8 type
fn runFile(allocator: std.mem.Allocator, file_name: []const u8) ![]const u8 {
    const file = try std.fs.cwd().openFile(file_name, .{});
    const file_stats = try file.stat();
    const byte_length = file_stats.size;

    const buffer = try allocator.alloc(u8, byte_length + 1);
    errdefer allocator.free(buffer);

    _ = try file.readAll(buffer);
    // TODO: look if there is better way to create null terminated string
    buffer[byte_length] = 0;
    return buffer;
}

fn run(allocator: std.mem.Allocator, file_contents: []const u8) !void {
    var scanner = lexer.Scanner.init(allocator, file_contents);
    const tokens = try scanner.scanTokens();
    defer allocator.free(tokens);

    for (tokens) |token| {
        std.debug.print(
            "Type: {any} Literal: {s}\n",
            .{ token.token_type, scanner.toLiteral(token) },
        );
        std.debug.print("\t{any}\n", .{token});
    }
}
