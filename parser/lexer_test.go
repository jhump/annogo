package parser

import (
	"fmt"
	"go/constant"
	"strings"
	"testing"
	"text/scanner"
)

func TestLexer(t *testing.T) {
	input := `
@pkg.NoValue(

	   Foo: pkg.Bar,
	   Baz: pkg.Boozle,

	)

	true false nil nan inf 400.303i 99i 101im 203.0405ir

	[ 100, 200 300 ] 'a' 'b' 'c'
	! @ # $ % ^ & * ( ) - = + \ | } { "" ; : / ? . > , <
	~ :
	,
	{
	} [
	] (
	),
	:
	+
	&&
	||
	==
	>=
	<=
	>
	<
	&
	|
	^
	-
	*
	/
	!

	complex real imag map struct interface

	"strings"
	"more strings \"with nested quotes\" and\nnew lines\ntoo... maybe even\n\t\ttabs..."
	0377
	.99
	0xaa
	1.0456e50
` + "`raw string literals work, too.\nschweeet!!`"

	cases := []struct {
		tok           int
		lineNo, colNo int
		val           interface{}
	}{
		{_EOL, 1, 1, nil},
		{'@', 2, 1, nil},
		{_IDENT, 2, 2, "pkg"},
		{'.', 2, 5, nil},
		{_IDENT, 2, 6, "NoValue"},
		{'(', 2, 13, nil},
		{_IDENT, 4, 5, "Foo"},
		{':', 4, 8, nil},
		{_IDENT, 4, 10, "pkg"},
		{'.', 4, 13, nil},
		{_IDENT, 4, 14, "Bar"},
		{',', 4, 17, nil},
		{_IDENT, 5, 5, "Baz"},
		{':', 5, 8, nil},
		{_IDENT, 5, 10, "pkg"},
		{'.', 5, 13, nil},
		{_IDENT, 5, 14, "Boozle"},
		{',', 5, 20, nil},
		{')', 7, 2, nil},
		{_EOL, 7, 3, nil},
		{_EOL, 8, 1, nil},
		{_TRUE, 9, 2, nil},
		{_FALSE, 9, 7, nil},
		{_NIL, 9, 13, nil},
		{_NAN, 9, 17, nil},
		{_INF, 9, 21, nil},
		{_IMAG_LIT, 9, 25, 400.303i},
		{_IMAG_LIT, 9, 34, 99i},
		{_INT_LIT, 9, 38, 101},
		{_IDENT, 9, 41, "im"},
		{_FLOAT_LIT, 9, 44, 203.0405},
		{_IDENT, 9, 52, "ir"},
		{_EOL, 9, 54, nil},
		{_EOL, 10, 1, nil},
		{'[', 11, 2, nil},
		{_INT_LIT, 11, 4, 100},
		{',', 11, 7, nil},
		{_INT_LIT, 11, 9, 200},
		{_INT_LIT, 11, 13, 300},
		{']', 11, 17, nil},
		{_RUNE_LIT, 11, 19, 'a'},
		{_RUNE_LIT, 11, 23, 'b'},
		{_RUNE_LIT, 11, 27, 'c'},
		{_EOL, 11, 30, nil},
		{'!', 12, 2, nil},
		{'@', 12, 4, nil},
		{'#', 12, 6, nil},
		{'$', 12, 8, nil},
		{'%', 12, 10, nil},
		{'^', 12, 12, nil},
		{'&', 12, 14, nil},
		{'*', 12, 16, nil},
		{'(', 12, 18, nil},
		{')', 12, 20, nil},
		{'-', 12, 22, nil},
		{'=', 12, 24, nil},
		{'+', 12, 26, nil},
		{'\\', 12, 28, nil},
		{'|', 12, 30, nil},
		{'}', 12, 32, nil},
		{'{', 12, 34, nil},
		{_STRING_LIT, 12, 36, ""},
		{';', 12, 39, nil},
		{':', 12, 41, nil},
		{'/', 12, 43, nil},
		{'?', 12, 45, nil},
		{'.', 12, 47, nil},
		{'>', 12, 49, nil},
		{',', 12, 51, nil},
		{'<', 12, 53, nil},
		{'~', 13, 2, nil},
		{':', 13, 4, nil},
		{',', 14, 2, nil},
		{'{', 15, 2, nil},
		{'}', 16, 2, nil},
		{'[', 16, 4, nil},
		{']', 17, 2, nil},
		{'(', 17, 4, nil},
		{')', 18, 2, nil},
		{',', 18, 3, nil},
		{':', 19, 2, nil},
		{'+', 20, 2, nil},
		{_AND, 21, 2, nil},
		{_OR, 22, 2, nil},
		{_EQ, 23, 2, nil},
		{_GTE, 24, 2, nil},
		{_LTE, 25, 2, nil},
		{'>', 26, 2, nil},
		{'<', 27, 2, nil},
		{'&', 28, 2, nil},
		{'|', 29, 2, nil},
		{'^', 30, 2, nil},
		{'-', 31, 2, nil},
		{'*', 32, 2, nil},
		{'/', 33, 2, nil},
		{'!', 34, 2, nil},
		{_COMPLEX, 36, 2, nil},
		{_REAL, 36, 10, nil},
		{_IMAG, 36, 15, nil},
		{_MAP, 36, 20, nil},
		{_STRUCT, 36, 24, nil},
		{_INTERFACE, 36, 31, nil},
		{_EOL, 36, 40, nil},
		{_EOL, 37, 1, nil},
		{_STRING_LIT, 38, 2, "strings"},
		{_EOL, 38, 11, nil},
		{_STRING_LIT, 39, 2, `more strings "with nested quotes" and
new lines
too... maybe even
		tabs...`},
		{_EOL, 39, 86, nil},
		{_INT_LIT, 40, 2, 0377},
		{_EOL, 40, 6, nil},
		{_FLOAT_LIT, 41, 2, .99},
		{_EOL, 41, 5, nil},
		{_INT_LIT, 42, 2, 0xaa},
		{_EOL, 42, 6, nil},
		{_FLOAT_LIT, 43, 2, 1.0456e50},
		{_EOL, 43, 11, nil},
		{_RAW_STRING_LIT, 44, 1, `raw string literals work, too.
schweeet!!`},
	}

	l := newLexer("foo", strings.NewReader(input))

	for i, tc := range cases {
		var s annotationsSymType
		tok := l.Lex(&s)
		if tok != tc.tok {
			t.Fatalf("case %d: expecting token %s, got %s", i+1, getTokenName(tc.tok), getTokenName(tok))
		}
		var p scanner.Position
		var val interface{}
		switch tok {
		case _RAW_STRING_LIT, _STRING_LIT, _RUNE_LIT, _INT_LIT, _FLOAT_LIT, _IMAG_LIT:
			p = s.l.pos
			val = s.l.Val
		case _IDENT:
			p = s.n.Pos
			val = s.n.Val
		default:
			p = s.p
		}

		if p.Line != tc.lineNo || p.Column != tc.colNo {
			t.Fatalf("case %d: expecting position %d:%d, got %d:%d", i+1, tc.lineNo, tc.colNo, p.Line, p.Column)
		}

		if val == nil && tc.val != nil {
			t.Fatalf("case %d: expecting value %v but got nothing?", i+1, tc.val)
		}

		if cv, ok := val.(constant.Value); ok {
			switch tok {
			case _RAW_STRING_LIT, _STRING_LIT:
				val = constant.StringVal(cv)
			case _RUNE_LIT:
				r, _ := constant.Uint64Val(cv)
				val = rune(r)
			case _INT_LIT:
				in, _ := constant.Uint64Val(cv)
				val = int(in)
			case _FLOAT_LIT:
				val, _ = constant.Float64Val(cv)
			case _IMAG_LIT:
				rv, iv := constant.Real(cv), constant.Imag(cv)
				re, _ := constant.Float64Val(rv)
				im, _ := constant.Float64Val(iv)
				val = complex(re, im)
			}
		}
		if val != tc.val {
			t.Fatalf("case %d: expecting value %v but got %v", i+1, tc.val, val)
		}
	}
}

func getTokenName(token int) string {
	var internal int
	if token < len(annotationsTok1) {
		internal = annotationsTok1[token]
	} else {
		if token >= annotationsPrivate {
			if token < annotationsPrivate+len(annotationsTok2) {
				internal = annotationsTok2[token-annotationsPrivate]
			}
		}
		if internal == 0 {
			for i := 0; i+1 < len(annotationsTok3); i += 2 {
				if annotationsTok3[i] == token {
					internal = annotationsTok3[i+1]
					break
				}
			}
		}
	}

	if internal >= 1 && internal-1 < len(annotationsToknames) {
		return annotationsToknames[internal-1]
	}
	if token >= 32 && token < 127 {
		return fmt.Sprintf("'%c'", token)
	}
	return fmt.Sprintf("%d", token)
}
