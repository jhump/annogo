package parser

import (
	"errors"
	"fmt"
	"go/constant"
	"go/token"
	"io"
	"text/scanner"
)

//go:generate goyacc -o annotations.y.go -p annotations annotations.y

func init() {
	annotationsErrorVerbose = true

	// fix up the generated "token name" array so that error messages are nicer
	setTokenName(_STRING_LIT, "string literal")
	setTokenName(_RAW_STRING_LIT, "raw string literal")
	setTokenName(_IDENT, "identifier")
	setTokenName(_RUNE_LIT, "rune literal")
	setTokenName(_INT_LIT, "int literal")
	setTokenName(_FLOAT_LIT, "float literal")
	setTokenName(_IMAG_LIT, "imaginary literal")
	setTokenName(_EOL, "end-of-line")
	setTokenName(_NIL, `"nil"`)
	setTokenName(_TRUE, `"true"`)
	setTokenName(_FALSE, `"false"`)
	setTokenName(_REAL, `"real"`)
	setTokenName(_IMAG, `"imag"`)
	setTokenName(_COMPLEX, `"complex"`)
	setTokenName(_NAN, `"nan"`)
	setTokenName(_INF, `"inf"`)
	setTokenName(_MAP, `"map"`)
	setTokenName(_STRUCT, `"struct"`)
	setTokenName(_INTERFACE, `"interface"`)
	setTokenName(_SHR, `">>"`)
	setTokenName(_SHL, `"<<"`)
	setTokenName(_AND_NOT, `"&^"`)
	setTokenName(_AND, `"&&"`)
	setTokenName(_OR, `"||"`)
	setTokenName(_EQ, `"=="`)
	setTokenName(_NEQ, `"!="`)
	setTokenName(_GTE, `">="`)
	setTokenName(_LTE, `"<="`)
}

func setTokenName(token int, text string) {
	// NB: this is based on logic in generated parse code that translates the
	// int returned from the lexer into an internal token number.
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
		annotationsToknames[internal-1] = text
		return
	}

	panic(fmt.Sprintf("Unknown token value: %d", token))
}

type annoLex struct {
	res []Annotation
	err error

	nextRune rune
	nextTok  string
	nextPos  scanner.Position

	lastRune rune
	lastTok  string
	lastPos  scanner.Position

	s scanner.Scanner
}

func newLexer(filename string, r io.Reader) *annoLex {
	var l annoLex
	l.s.Init(r)
	l.s.Filename = filename
	l.s.Mode = l.s.Mode &^ (scanner.ScanComments | scanner.SkipComments)
	l.s.Whitespace = 0
	l.s.Error = func(s *scanner.Scanner, msg string) {
		l.err = errors.New(msg)
	}
	return &l
}

type ParseError struct {
	err error
	pos scanner.Position
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("line %d, column %d: %s", e.pos.Line, e.pos.Column, e.err)
}

func (e *ParseError) Underlying() error {
	return e.err
}

func (e *ParseError) Pos() scanner.Position {
	return e.pos
}

func ParseAnnotations(filename string, r io.Reader) ([]Annotation, *ParseError) {
	l := newLexer(filename, r)
	annotationsParse(l)
	if l.err != nil {
		return nil, &ParseError{err: l.err, pos: l.lastPos}
	}
	return l.res, nil
}

var keywords = map[string]int{
	"complex":   _COMPLEX,
	"real":      _REAL,
	"imag":      _IMAG,
	"nan":       _NAN,
	"inf":       _INF,
	"nil":       _NIL,
	"true":      _TRUE,
	"false":     _FALSE,
	"map":       _MAP,
	"struct":    _STRUCT,
	"interface": _INTERFACE,
}

var trailingRunes = map[rune]struct{}{
	',': {},
	'.': {},
	'{': {},
	'(': {},
	'[': {},
	':': {},
	'+': {},
	'-': {},
	'*': {},
	'/': {},
	'%': {},
	'^': {},
	'&': {},
	'|': {},
	'!': {},
	'>': {},
	'<': {},
	'=': {},
}

func (l *annoLex) Lex(lval *annotationsSymType) (t int) {
	if l.err != nil {
		lval.err = l.err
		return _ERROR
	}

	var r rune
	var tok string
	var pos scanner.Position
	defer func() {
		if r != scanner.EOF {
			l.lastRune = r
			l.lastTok = tok
			l.lastPos = pos
		}
		if t == _ERROR && l.err == nil {
			l.err = lval.err
		}
	}()

	for {
		if l.nextRune != 0 {
			r = l.nextRune
			tok = l.nextTok
			pos = l.nextPos
			l.nextRune = 0
			l.nextTok = ""
			l.nextPos = scanner.Position{}
		} else {
			pos = l.s.Pos()
			r = l.s.Scan()
			tok = l.s.TokenText()
			if l.err != nil {
				lval.err = l.err
				return _ERROR
			}
		}

		if r == scanner.EOF {
			return 0
		}

		// we handle whitespace ourselves so that we can easily know the
		// *start* position for a token (otherwise, scanner package only makes
		// easy to determine *end* position for a token)
		if r == ' ' || r == '\t' || r == '\r' {
			continue
		}

		lval.p = pos

		switch r {
		case scanner.Ident:
			if v, ok := keywords[tok]; ok {
				return v
			}
			lval.n = nameNode{Val: tok, Pos: pos}
			return _IDENT

		case scanner.Int:
			if l.s.Peek() == 'i' {
				np := l.s.Pos()
				nr := l.s.Scan()
				nt := l.s.TokenText()
				if nr == scanner.Ident && nt == "i" {
					// it's an imaginary constant
					v := constant.MakeFromLiteral(tok+"i", token.IMAG, 0)
					lval.l = LiteralNode{Val: v, pos: pos}
					return _IMAG_LIT
				} else {
					// make sure we get this token next time
					l.nextRune = nr
					l.nextTok = nt
					l.nextPos = np
				}
			}
			v := constant.MakeFromLiteral(tok, token.INT, 0)
			lval.l = LiteralNode{Val: v, pos: pos}
			return _INT_LIT

		case scanner.Float:
			if l.s.Peek() == 'i' {
				np := l.s.Pos()
				nr := l.s.Scan()
				nt := l.s.TokenText()
				if nr == scanner.Ident && nt == "i" {
					// it's an imaginary constant
					v := constant.MakeFromLiteral(tok+"i", token.IMAG, 0)
					lval.l = LiteralNode{Val: v, pos: pos}
					return _IMAG_LIT
				} else {
					// make sure we get this token next time
					l.nextRune = nr
					l.nextTok = nt
					l.nextPos = np
				}
			}
			v := constant.MakeFromLiteral(tok, token.FLOAT, 0)
			lval.l = LiteralNode{Val: v, pos: pos}
			return _FLOAT_LIT

		case scanner.Char:
			v := constant.MakeFromLiteral(tok, token.CHAR, 0)
			lval.l = LiteralNode{Val: v, pos: pos}
			return _RUNE_LIT

		case scanner.String, scanner.RawString:
			v := constant.MakeFromLiteral(tok, token.STRING, 0)
			lval.l = LiteralNode{Val: v, pos: pos}
			if tok[0] == '`' {
				return _RAW_STRING_LIT
			}
			return _STRING_LIT

		case '\n':
			if r == '\n' {
				if _, ok := trailingRunes[l.lastRune]; ok {
					continue
				}
				return _EOL
			}

		case '&':
			if l.s.Peek() == '^' {
				l.s.Next() // consume it
				return _AND_NOT
			}
			if l.s.Peek() == '&' {
				l.s.Next() // consume it
				return _AND
			}

		case '|':
			if l.s.Peek() == '|' {
				l.s.Next() // consume it
				return _OR
			}

		case '=':
			if l.s.Peek() == '=' {
				l.s.Next() // consume it
				return _EQ
			}

		case '!':
			if l.s.Peek() == '=' {
				l.s.Next() // consume it
				return _NEQ
			}

		case '<':
			if l.s.Peek() == '<' {
				l.s.Next() // consume it
				return _SHL
			}
			if l.s.Peek() == '=' {
				l.s.Next() // consume it
				return _LTE
			}

		case '>':
			if l.s.Peek() == '>' {
				l.s.Next() // consume it
				return _SHR
			}
			if l.s.Peek() == '=' {
				l.s.Next() // consume it
				return _GTE
			}
		}

		return int(r)
	}
}

func (l *annoLex) Error(s string) {
	l.err = errors.New(s)
}
