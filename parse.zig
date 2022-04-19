const std = @import("std");
const unitab = @import("unicode_table.zig");

pub const Value = union(enum) {
    symbol: []const u8,
    string: []const u8,
    number: []const u8,
    list: []const Value,
    call: []const Value,
    block: []const Value,

    pub fn deinit(self: Value, allocator: std.mem.Allocator) void {
        switch (self) {
            .symbol => |sym| allocator.free(sym),
            .string => |str| allocator.free(str),
            .number => |num| allocator.free(num),
            .list => |list| freeTree(allocator, list),
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
            self.skipWs();
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
            '}', ']', ')' => return error.EndOfExpr,

            '{' => return self.readBlock(),
            '[' => return self.readList(),
            '(' => return self.readParen(),
            '"' => return self.readString(),
            // TODO: signed numbers - annoying because of operators
            '0'...'9' => return self.readNum(),
            else => return self.readSym(),
        }
    }

    fn readBlock(self: *Parser) ReadError!Value {
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

    fn readList(self: *Parser) ReadError!Value {
        std.debug.assert(self.peekByte() == @as(u8, '['));
        self.nextByte();

        var array = std.ArrayList(Value).init(self.allocator);
        errdefer freeTree(self.allocator, array.toOwnedSlice());
        while (self.peekByte() != @as(u8, ']')) : (self.skipWs()) {
            const val = self.readValue() catch |err| switch (err) {
                error.Operator => try self.makeOpValue(try self.readOp()),
                error.EndOfExpr => continue,
                else => |e| return e,
            };

            try array.append(val);
        }
        self.nextByte();

        return Value{ .list = array.toOwnedSlice() };
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
        const first_digit = self.peekByte().?;
        std.debug.assert(std.ascii.isDigit(first_digit));
        self.nextByte();

        // Check base
        if (first_digit == '0') {
            if (self.peekByte()) |byte| {
                switch (byte) {
                    'o' => return self.readIntBase(start, octDigit),
                    'x' => return self.readIntBase(start, std.ascii.isXDigit),
                    else => {},
                }
            }
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
    fn readIntBase(self: *Parser, start: usize, comptime isDigit: fn (digit: u8) bool) !Value {
        self.nextByte(); // Skip base char
        while (self.peekByte()) |byte| {
            if (!isDigit(byte)) break;
            self.nextByte();
        }
        return Value{
            .number = try self.allocator.dupe(u8, self.source[start..self.offset]),
        };
    }
    fn octDigit(digit: u8) bool {
        return digit >= '0' and digit < '8';
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
        return Value{ .symbol = try self.allocator.dupe(u8, op) };
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
        '{', '}', '[', ']', '(', ')', ';' => return null,
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
            .{ .symbol = "+" },
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
        } },
        .{ .call = &.{
            .{ .symbol = "*" },
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
            .{ .symbol = "+" },
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
            .{ .call = &.{
                .{ .symbol = "*" },
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
            .{ .symbol = "+" },
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
        } },
        .{ .call = &.{
            .{ .symbol = "*" },
            .{ .symbol = "ghi" },
            .{ .symbol = "jkl" },
            .{ .symbol = "mno" },
            .{ .symbol = "p" },
        } },
        .{ .call = &.{
            .{ .symbol = "-" },
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
        .{ .symbol = "+" },
        .{ .symbol = "*" },
    }, vals);
}

test "parenthesized operator" {
    var p = Parser.init(std.testing.allocator,
        \\(+); (*)
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .symbol = "+" },
        .{ .symbol = "*" },
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
            .{ .symbol = "+" },
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
        \\1e7; 1.3e-2; 123.67e+10;
        \\0x10; 0xae1; 0x23f;
        \\0o10; 0o71; 0o23;
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

        .{ .number = "0x10" },
        .{ .number = "0xae1" },
        .{ .number = "0x23f" },

        .{ .number = "0o10" },
        .{ .number = "0o71" },
        .{ .number = "0o23" },
    }, vals);
}

test "strings" {
    var p = Parser.init(std.testing.allocator,
        \\"Hello, world!"; "\"Hi\", said Jane";
        \\"\x12\x34"; "\u{1234}";
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .string = "Hello, world!" },
        .{ .string = "\"Hi\", said Jane" },
        .{ .string = "\x12\x34" },
        .{ .string = "\u{1234}" },
    }, vals);
}

test "lists" {
    var p = Parser.init(std.testing.allocator,
        \\[1 2 3];
        \\[abc def];
        \\["foo" "bar"];
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .list = &.{
            .{ .number = "1" },
            .{ .number = "2" },
            .{ .number = "3" },
        } },
        .{ .list = &.{
            .{ .symbol = "abc" },
            .{ .symbol = "def" },
        } },
        .{ .list = &.{
            .{ .string = "foo" },
            .{ .string = "bar" },
        } },
    }, vals);
}

test "list in call" {
    var p = Parser.init(std.testing.allocator,
        \\foo [bar baz];
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "foo" },
            .{ .list = &.{
                .{ .symbol = "bar" },
                .{ .symbol = "baz" },
            } },
        } },
    }, vals);
}

test "block in call" {
    var p = Parser.init(std.testing.allocator,
        \\foo {bar baz;};
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "foo" },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "bar" },
                    .{ .symbol = "baz" },
                } },
            } },
        } },
    }, vals);
}

test "trailing whitespace" {
    var p = Parser.init(std.testing.allocator, "foo ");
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .symbol = "foo" },
    }, vals);
}

test "mixed 1" {
    var p = Parser.init(std.testing.allocator,
        \\fn main(x: i32) {
        \\	if (x < 3) {
        \\		print "Hello, world!";
        \\	};
        \\};
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "fn" },
            .{ .symbol = "main" },
            .{ .call = &.{
                .{ .symbol = ":" },
                .{ .symbol = "x" },
                .{ .symbol = "i32" },
            } },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "if" },
                    .{ .call = &.{
                        .{ .symbol = "<" },
                        .{ .symbol = "x" },
                        .{ .number = "3" },
                    } },
                    .{ .block = &.{
                        .{ .call = &.{
                            .{ .symbol = "print" },
                            .{ .string = "Hello, world!" },
                        } },
                    } },
                } },
            } },
        } },
    }, vals);
}

test "mixed 2" {
    var p = Parser.init(std.testing.allocator,
        \\font "Fira Mono" 12;
        \\colors {
        \\	foreground 0xefeade;
        \\	background 0x131420;
        \\};
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "font" },
            .{ .string = "Fira Mono" },
            .{ .number = "12" },
        } },
        .{ .call = &.{
            .{ .symbol = "colors" },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "foreground" },
                    .{ .number = "0xefeade" },
                } },
                .{ .call = &.{
                    .{ .symbol = "background" },
                    .{ .number = "0x131420" },
                } },
            } },
        } },
    }, vals);
}

test "mixed 3" {
    var p = Parser.init(std.testing.allocator,
        \\person { name "Ada Lovelace"; dob "1815-12-10"; };
        \\person { name "Alan Turing"; dob "1912-06-23"; };
        \\person { name "Grace Hopper"; dob "1906-12-09"; };
        \\person { name "Mary Ann Horton"; dob "1955-11-21"; };
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "person" },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "name" },
                    .{ .string = "Ada Lovelace" },
                } },
                .{ .call = &.{
                    .{ .symbol = "dob" },
                    .{ .string = "1815-12-10" },
                } },
            } },
        } },
        .{ .call = &.{
            .{ .symbol = "person" },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "name" },
                    .{ .string = "Alan Turing" },
                } },
                .{ .call = &.{
                    .{ .symbol = "dob" },
                    .{ .string = "1912-06-23" },
                } },
            } },
        } },
        .{ .call = &.{
            .{ .symbol = "person" },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "name" },
                    .{ .string = "Grace Hopper" },
                } },
                .{ .call = &.{
                    .{ .symbol = "dob" },
                    .{ .string = "1906-12-09" },
                } },
            } },
        } },
        .{ .call = &.{
            .{ .symbol = "person" },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "name" },
                    .{ .string = "Mary Ann Horton" },
                } },
                .{ .call = &.{
                    .{ .symbol = "dob" },
                    .{ .string = "1955-11-21" },
                } },
            } },
        } },
    }, vals);
}

test "mixed 4" {
    var p = Parser.init(std.testing.allocator,
        \\use sys [stdout writeLn];
        \\fn main {
        \\	writeLn stdout "Hello, world!";
        \\};
        \\
    );
    const vals = try p.read();
    defer freeTree(p.allocator, vals);
    try expectTree(&.{
        .{ .call = &.{
            .{ .symbol = "use" },
            .{ .symbol = "sys" },
            .{ .list = &.{
                .{ .symbol = "stdout" },
                .{ .symbol = "writeLn" },
            } },
        } },
        .{ .call = &.{
            .{ .symbol = "fn" },
            .{ .symbol = "main" },
            .{ .block = &.{
                .{ .call = &.{
                    .{ .symbol = "writeLn" },
                    .{ .symbol = "stdout" },
                    .{ .string = "Hello, world!" },
                } },
            } },
        } },
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
            .list => |list| try expectTree(list, actual[i].list),
            .call => |call| try expectTree(call, actual[i].call),
            .block => |blk| try expectTree(blk, actual[i].block),
        }
    }
}
