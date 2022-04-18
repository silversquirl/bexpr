# B-expressions

B-expressions are a simple programming language, configuration file, and data storage syntax.
Similar to the S-expressions used by Lisp and Scheme, B-expressions are simple, highly flexible, and extremely consistent.

Unlike S-expressions, B-expressions are designed around the brace syntax used by C-like languages.
B-expressions also feature infix operators for more readable mathematical expressions.

## Examples

As a programming language syntax:

```zig
fn main(x: i32) {
	if (x < 3) {
		print "Hello, world!";
	};
};
```

As a configuration syntax:

```zig
font "Fira Mono" 12;
colors {
	foreground 0xefeade;
	background 0x131420;
};
```

As a structured data storage format:

```zig
person { name "Ada Lovelace"; dob "1815-12-10"; };
person { name "Alan Turing"; dob "1912-06-23"; };
person { name "Grace Hopper"; dob "1906-12-09"; };
person { name "Mary Ann Horton"; dob "1955-11-21"; };
```
