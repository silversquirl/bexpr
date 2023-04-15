#!/bin/sh

{
	cat <<EOF
pub const Class = enum(u2) {
    ident_alpha,
    ident_num,
    operator,
};
pub const Entry = packed struct {
    codepoint: u21,
    class: Class,
};

pub const table = [_]Entry{
EOF

	curl -L https://www.unicode.org/Public/UCD/latest/ucd/UnicodeData.txt |
	while IFS=';' read -r codepoint name category rest; do
		case "$category" in
			Ll|Lm|Lo|Lt|Lu|Pc)
				class="ident_alpha";;
			Nd|Nl|No)
				class="ident_num";;
			Pd|Pe|Pf|Pi|Po|Ps|Sc|Sk|Sm)
				class="operator";;
			*) continue;;
		esac
		echo "    .{ .codepoint = 0x$codepoint, .class = .$class },"
	done

	echo "};"
} | zig fmt --stdin >src/unicode_table.zig
