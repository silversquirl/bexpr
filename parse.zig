const std = @import("std");
const unitab = @import("unicode_table.zig");

pub const Value = union(enum) {
    symbol: []const u8,
    string: []const u8,
    number: []const u8,
    operator: []const u8,
    call: []const Value,
    block: []const Value,

    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .symbol => |sym| allocator.free(sym),
            .string => |str| allocator.free(str),
            .number => |num| allocator.free(num),
            .operator => |op| allocator.free(op),
            .call => |call| freeTree(allocator, call),
            .block => |blk| freeTree(allocator, blk),
        }
    }
};

pub fn freeTree(allocator: std.mem.Allocator, vals: []const Value) void {
    for (vals) |val| {
        val.deinit(allocator);
    }
    allocator.free(vals);
}

pub const Parser = struct {
    source: []const u8,
    allocator: std.mem.Allocator,
    offset: usize = 0,
    err: Error = .none,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Parser {
        return .{ .allocator = allocator, .source = source };
    }

    pub const Error = union(enum) {
        none,

        empty_expression,
        unterminated_paren,

        operator_in_call,
        call_in_operator,
        operator_in_operator,
        missing_operand,
        mixed_operators,

        invalid_ident_start_char: u21,
        invalid_char: u21,

        missing_exponent,

        unterminated_string,
        string_literal: std.zig.string_literal.Error, // TODO: improve this
    };
    pub const ReadError = error{
        ParseError,
        OutOfMemory,
        Utf8Invalid,
    };

    pub fn read(self: *Parser) ReadError![]const Value {
        var array = std.ArrayList(Value).init(self.allocator);
        errdefer freeTree(self.allocator, array.toOwnedSlice());
        while (self.peekByte() != null) {
            const val = try self.readExpr();
            try array.append(val);
        }
        return array.toOwnedSlice();
    }

    fn readExpr(self: *Parser) ReadError!Value {
        var val1_is_op = false;
        const val = self.readValue() catch |err| switch (err) {
            error.Operator => blk: {
                val1_is_op = true;
                break :blk try self.makeOpValue(try self.readOp());
            },

            error.EndOfExpr => {
                self.err = .empty_expression;
                return error.ParseError;
            },
            else => |e| return e,
        };

        if (self.readValue()) |val2| {
            return self.readCallExpr(val, val2);
        } else |err| switch (err) {
            // This control flow is a bit weird. Sorry.
            error.Operator => {
                if (val1_is_op) {
                    self.err = .operator_in_operator;
                    return error.ParseError;
                }
                return self.readOpExpr(val);
            },

            error.EndOfExpr => return val,
            else => |e| return e,
        }
    }

    fn readOpExpr(self: *Parser, first_val: Value) ReadError!Value {
        const op = try self.readOp();
        var vals = std.ArrayList(Value).init(self.allocator);
        errdefer freeTree(self.allocator, vals.toOwnedSlice());
        try vals.append(try self.makeOpValue(op));
        try vals.append(first_val);

        while (self.readValue()) |val| {
            try vals.append(val);

            if (self.readValue()) |val2| {
                val2.deinit(self.allocator);
                self.err = .call_in_operator;
                return error.ParseError;
            } else |err| switch (err) {
                error.Operator => {},
                error.EndOfExpr => break,
                else => |e| return e,
            }

            const op_again = try self.readOp();
            if (!std.mem.eql(u8, op, op_again)) {
                self.err = .mixed_operators;
                return error.ParseError;
            }
        } else |err| switch (err) {
            error.EndOfExpr => {
                self.err = .missing_operand;
                return error.ParseError;
            },
            error.Operator => {},
            else => |e| return e,
        }

        return Value{ .call = vals.toOwnedSlice() };
    }

    fn readCallExpr(self: *Parser, func: Value, first_arg: Value) ReadError!Value {
        var vals = std.ArrayList(Value).init(self.allocator);
        errdefer freeTree(self.allocator, vals.toOwnedSlice());
        try vals.append(func);
        try vals.append(first_arg);

        while (self.readValue()) |val| {
            try vals.append(val);
        } else |err| switch (err) {
            error.EndOfExpr => {},
            error.Operator => {
                self.err = .operator_in_call;
                return error.ParseError;
            },
            else => |e| return e,
        }

        return Value{ .call = vals.toOwnedSlice() };
    }

    fn readValue(self: *Parser) !Value {
        self.skipWs();
        const byte = self.peekByte() orelse {
            return error.EndOfExpr;
        };
        switch (byte) {
            ';' => {
                self.nextByte();
                return error.EndOfExpr;
            },
            '}', ')' => return error.EndOfExpr,

            '{' => return self.readBlock(),
            '(' => return self.readParen(),
            '"' => return self.readString(),
            // TODO: signed numbers - annoying because of operators
            '0'...'9' => return self.readNum(),
            else => return self.readSym(),
        }
    }

    fn readBlock(self: *Parser) !Value {
        std.debug.assert(self.peekByte() == @as(u8, '{'));
        self.nextByte();

        var array = std.ArrayList(Value).init(self.allocator);
        errdefer freeTree(self.allocator, array.toOwnedSlice());
        while (self.peekByte() != @as(u8, '}')) {
            const val = try self.readExpr();
            try array.append(val);
            self.skipWs();
        }
        self.nextByte();

        return Value{ .block = array.toOwnedSlice() };
    }

    fn readParen(self: *Parser) !Value {
        std.debug.assert(self.peekByte() == @as(u8, '('));
        self.nextByte();

        const val = try self.readExpr();

        self.skipWs();
        if (self.peekByte() != @as(u8, ')')) {
            self.err = .unterminated_paren;
            return error.ParseError;
        }
        self.nextByte();

        return val;
    }

    fn readString(self: *Parser) !Value {
        const start = self.offset;
        std.debug.assert(self.peekByte() == @as(u8, '"'));
        self.nextByte();

        while (self.peekByte() != @as(u8, '"')) {
            if (self.peekByte() == @as(u8, '\\')) {
                self.nextByte();
            }
            if (self.peekByte() != null) {
                self.nextByte();
            } else {
                self.err = .unterminated_string;
                return error.ParseError;
            }
        }
        self.nextByte();

        var buf = std.ArrayList(u8).init(self.allocator);
        defer buf.deinit();

        switch (try std.zig.string_literal.parseAppend(&buf, self.source[start..self.offset])) {
            .success => return Value{ .string = buf.toOwnedSlice() },
            .failure => |err| {
                self.err = .{ .string_literal = err };
                return error.ParseError;
            },
        }
    }

    fn readNum(self: *Parser) !Value {
        const start = self.offset;
        {
            const byte = self.peekByte().?;
            std.debug.assert(std.ascii.isDigit(byte));
            self.nextByte();
        }

        while (self.peekByte()) |byte| {
            if (!std.ascii.isDigit(byte)) break;
            self.nextByte();
        }

        if (self.peekByte() == @as(u8, '.')) {
            self.nextByte();
            while (self.peekByte()) |byte| {
                if (!std.ascii.isDigit(byte)) break;
                self.nextByte();
            }
        }

        if (self.peekByte() == @as(u8, 'e')) {
            self.nextByte();

            {
                const b = self.peekByte();
                if (b == null or !(std.ascii.isDigit(b.?) or b.? == '-' or b.? == '+')) {
                    self.err = .missing_exponent;
                    return error.ParseError;
                }
            }
            self.nextByte();

            while (self.peekByte()) |byte| {
                if (!std.ascii.isDigit(byte)) break;
                self.nextByte();
            }
        }

        return Value{
            .number = try self.allocator.dupe(u8, self.source[start..self.offset]),
        };
    }

    fn readSym(self: *Parser) !Value {
        const cp = (try self.peekCodepoint()).?;
        const class = classify(cp) orelse {
            self.err = .{ .invalid_char = cp };
            return error.ParseError;
        };
        switch (class) {
            .ident_alpha => return self.readIdent(),
            .operator => return error.Operator,
            .ident_num => {
                self.err = .{ .invalid_ident_start_char = cp };
                return error.ParseError;
            },
        }
    }

    fn readIdent(self: *Parser) !Value {
        const start = self.offset;
        while (try self.peekCodepoint()) |cp| {
            const class = classify(cp) orelse break;
            switch (class) {
                .ident_alpha, .ident_num => self.nextCodepoint(),
                .operator => break,
            }
        }
        return Value{
            .symbol = try self.allocator.dupe(u8, self.source[start..self.offset]),
        };
    }
    fn readOp(self: *Parser) ![]const u8 {
        const start = self.offset;
        while (try self.peekCodepoint()) |cp| {
            const class = classify(cp) orelse break;
            switch (class) {
                .operator => self.nextCodepoint(),
                .ident_alpha, .ident_num => break,
            }
        }
        std.debug.assert(self.offset > start);
        return self.source[start..self.offset];
    }
    fn makeOpValue(self: *Parser, op: []const u8) !Value {
        return Value{ .operator = try self.allocator.dupe(u8, op) };
    }

    fn skipWs(self: *Parser) void {
        while (self.peekByte()) |byte| {
            if (std.ascii.isSpace(byte)) {
                self.nextByte();
            } else {
                break;
            }
        }
    }

    fn peekByte(self: Parser) ?u8 {
        if (self.offset >= self.source.len) {
            return null;
        }
        return self.source[self.offset];
    }
    fn peekCodepoint(self: Parser) !?u21 {
        const first_byte = self.peekByte() orelse return null;
        const count = std.unicode.utf8ByteSequenceLength(first_byte) catch return error.Utf8Invalid;
        if (self.offset + count > self.source.len) {
            return error.Utf8Invalid;
        }
        return std.unicode.utf8Decode(
            self.source[self.offset .. self.offset + count],
        ) catch error.Utf8Invalid;
    }
    fn nextByte(self: *Parser) void {
        std.debug.assert(self.offset < self.source.len);
        self.offset += 1;
    }
    fn nextCodepoint(self: *Parser) void {
        const first_byte = self.peekByte() orelse unreachable;
        const count = std.unicode.utf8ByteSequenceLength(first_byte) catch unreachable;
        std.debug.assert(self.offset + count <= self.source.len);
        self.offset += count;
    }
};

fn classify(cp: u21) ?unitab.Class {
    switch (cp) {
        '{', '}', '(', ')', ';' => return null,
        else => {},
    }

    const idx = std.sort.binarySearch(
        unitab.Entry,
        .{ .codepoint = cp, .class = undefined },
        &unitab.table,
        {},
        classifyOrder,
    ) orelse return null;

    return unitab.table[idx].class;
}
fn classifyOrder(_: void, lhs: unitab.Entry, rhs: unitab.Entry) std.math.Order {
    return std.math.order(lhs.codepoint, rhs.codepoint);
}

test "symbol" {
    var p = Parser.init(std.testing.allocator,
        \\abc; def
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .symbol = "abc" },
        .{ .symbol = "def" },
    }, vals);
}

test "parenthesized symbol" {
    var p = Parser.init(std.testing.allocator,
        \\(abc); (def)
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .symbol = "abc" },
        .{ .symbol = "def" },
    }, vals);
}

test "block" {
    var p = Parser.init(std.testing.allocator,
        \\{abc; def}
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .block = &.{
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
        } },
    }, vals);
}

test "operator" {
    var p = Parser.init(std.testing.allocator,
        \\abc + def; ghi * jkl * mno * p
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .operator = "+" },
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
        } },
        .{ .call = &.{
            .{ .operator = "*" },
            .{ .symbol = "ghi" },
            .{ .symbol = "jkl" },
            .{ .symbol = "mno" },
            .{ .symbol = "p" },
        } },
    }, vals);
}

test "nested operator" {
    var p = Parser.init(std.testing.allocator,
        \\abc + def + (ghi * jkl * mno) + p
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .operator = "+" },
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
            .{ .call = &.{
                .{ .operator = "*" },
                .{ .symbol = "ghi" },
                .{ .symbol = "jkl" },
                .{ .symbol = "mno" },
            } },
            .{ .symbol = "p" },
        } },
    }, vals);
}

test "prefix operator" {
    var p = Parser.init(std.testing.allocator,
        \\+ abc def; * ghi jkl mno p; -qrs
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .operator = "+" },
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
        } },
        .{ .call = &.{
            .{ .operator = "*" },
            .{ .symbol = "ghi" },
            .{ .symbol = "jkl" },
            .{ .symbol = "mno" },
            .{ .symbol = "p" },
        } },
        .{ .call = &.{
            .{ .operator = "-" },
            .{ .symbol = "qrs" },
        } },
    }, vals);
}

test "operator value" {
    var p = Parser.init(std.testing.allocator,
        \\+; *
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .operator = "+" },
        .{ .operator = "*" },
    }, vals);
}

test "parenthesized operator" {
    var p = Parser.init(std.testing.allocator,
        \\(+); (*)
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .operator = "+" },
        .{ .operator = "*" },
    }, vals);
}

test "first class operator" {
    var p = Parser.init(std.testing.allocator,
        \\reduce (+) 0 list
    );
    errdefer std.debug.print("{} {}\n", .{ p.err, p.offset });
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "reduce" },
            .{ .operator = "+" },
            .{ .number = "0" },
            .{ .symbol = "list" },
        } },
    }, vals);
}

test "mixed operator error" {
    var p = Parser.init(std.testing.allocator,
        \\abc + def - ghi
    );
    try std.testing.expectError(error.ParseError, p.read());
    try std.testing.expectEqual(Parser.Error.mixed_operators, p.err);
}
test "call in operator error" {
    var p = Parser.init(std.testing.allocator,
        \\abc + def ghi
    );
    try std.testing.expectError(error.ParseError, p.read());
    try std.testing.expectEqual(Parser.Error.call_in_operator, p.err);
}

test "call" {
    var p = Parser.init(std.testing.allocator,
        \\abc def; ghi jkl mno p
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
        } },
        .{ .call = &.{
            .{ .symbol = "ghi" },
            .{ .symbol = "jkl" },
            .{ .symbol = "mno" },
            .{ .symbol = "p" },
        } },
    }, vals);
}

test "nested call" {
    var p = Parser.init(std.testing.allocator,
        \\abc def (ghi jkl) mno
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
            .{ .call = &.{
                .{ .symbol = "ghi" },
                .{ .symbol = "jkl" },
            } },
            .{ .symbol = "mno" },
        } },
    }, vals);
}

test "numbers" {
    var p = Parser.init(std.testing.allocator,
        \\123; 456; 1.23; 123.05;
        \\1e7; 1.3e-2; 123.67e+10
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .number = "123" },
        .{ .number = "456" },
        .{ .number = "1.23" },
        .{ .number = "123.05" },
        .{ .number = "1e7" },
        .{ .number = "1.3e-2" },
        .{ .number = "123.67e+10" },
    }, vals);
}

fn expectTree(expected: []const Value, actual: []const Value) error{TestExpectedEqual}!void {
    const Tag = std.meta.Tag(Value);
    try std.testing.expectEqual(expected.len, actual.len);
    for (expected) |val, i| {
        if (@as(Tag, val) != @as(Tag, actual[i])) {
            std.debug.print("Expected {}, got {}\n", .{ val, actual[i] });
            return error.TestExpectedEqual;
        }
        switch (val) {
            .symbol => |sym| try std.testing.expectEqualStrings(sym, actual[i].symbol),
            .string => |str| try std.testing.expectEqualStrings(str, actual[i].string),
            .number => |num| try std.testing.expectEqualStrings(num, actual[i].number),
            .operator => |op| try std.testing.expectEqualStrings(op, actual[i].operator),
            .call => |call| try expectTree(call, actual[i].call),
            .block => |blk| try expectTree(blk, actual[i].block),
        }
    }
}
