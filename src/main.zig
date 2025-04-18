const std = @import("std");
const lib = @import("parser_lib");

const Parser = lib.Parser(@embedFile("example.g"));

pub fn main() !void {
    const file_name = "examples/example";

    const file = try std.fs.cwd().openFile(file_name, .{});
    defer file.close();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();

    const data = try file.readToEndAlloc(gpa.allocator(), 4096);
    defer gpa.allocator().free(data);

    var tokenizer = Parser.Tokenizer.init(data, file_name);

    while (try tokenizer.next()) |token| {
        std.debug.print("{s}: '{s}'\n", .{@tagName(token.type), token.text});
    }
}
