//line annotations.y:2
package parser

import __yyfmt__ "fmt"

//line annotations.y:2
import (
	"go/constant"
	"math"
	"text/scanner"
)

//line annotations.y:14
type annotationsSymType struct {
	yys int
	n   nameNode
	l   LiteralNode
	v   ExpressionNode
	id  Identifier
	els []Element
	el  Element
	an  []Annotation
	t   Type
	p   scanner.Position
	err error
}

const _IDENT = 57346
const _STRING_LIT = 57347
const _RAW_STRING_LIT = 57348
const _RUNE_LIT = 57349
const _INT_LIT = 57350
const _FLOAT_LIT = 57351
const _IMAG_LIT = 57352
const _ERROR = 57353
const _NIL = 57354
const _TRUE = 57355
const _FALSE = 57356
const _EOL = 57357
const _REAL = 57358
const _IMAG = 57359
const _COMPLEX = 57360
const _NAN = 57361
const _INF = 57362
const _MAP = 57363
const _STRUCT = 57364
const _INTERFACE = 57365
const _SHL = 57366
const _SHR = 57367
const _AND_NOT = 57368
const _EQ = 57369
const _NEQ = 57370
const _GTE = 57371
const _LTE = 57372
const _AND = 57373
const _OR = 57374
const _MINUS = 57375

var annotationsToknames = [...]string{
	"$end",
	"error",
	"$unk",
	"_IDENT",
	"_STRING_LIT",
	"_RAW_STRING_LIT",
	"_RUNE_LIT",
	"_INT_LIT",
	"_FLOAT_LIT",
	"_IMAG_LIT",
	"_ERROR",
	"_NIL",
	"_TRUE",
	"_FALSE",
	"_EOL",
	"_REAL",
	"_IMAG",
	"_COMPLEX",
	"_NAN",
	"_INF",
	"_MAP",
	"_STRUCT",
	"_INTERFACE",
	"'@'",
	"'+'",
	"'-'",
	"'|'",
	"'^'",
	"'*'",
	"'/'",
	"'%'",
	"_SHL",
	"_SHR",
	"_AND_NOT",
	"'&'",
	"'{'",
	"'['",
	"'('",
	"'>'",
	"'<'",
	"'!'",
	"_EQ",
	"_NEQ",
	"_GTE",
	"_LTE",
	"_AND",
	"_OR",
	"'='",
	"'$'",
	"'#'",
	"'\\\\'",
	"'?'",
	"_MINUS",
	"';'",
	"')'",
	"'}'",
	"'.'",
	"']'",
	"','",
	"':'",
}
var annotationsStatenames = [...]string{}

const annotationsEofCode = 1
const annotationsErrCode = 2
const annotationsInitialStackSize = 16

//line annotations.y:304

//line yacctab:1
var annotationsExca = [...]int{
	-1, 1,
	1, -1,
	-2, 0,
	-1, 51,
	36, 18,
	-2, 39,
}

const annotationsPrivate = 57344

const annotationsLast = 543

var annotationsAct = [...]int{

	19, 20, 47, 121, 124, 123, 8, 48, 43, 87,
	139, 119, 42, 86, 86, 51, 85, 144, 14, 86,
	147, 146, 53, 13, 73, 12, 74, 75, 78, 41,
	77, 79, 80, 76, 54, 55, 63, 61, 56, 57,
	58, 59, 60, 64, 62, 5, 141, 51, 69, 70,
	91, 67, 68, 71, 72, 93, 94, 95, 96, 97,
	98, 99, 100, 101, 102, 103, 104, 105, 106, 107,
	108, 109, 110, 111, 112, 88, 92, 114, 115, 116,
	140, 89, 4, 52, 6, 9, 3, 34, 51, 51,
	33, 51, 128, 122, 128, 4, 118, 133, 132, 117,
	131, 32, 21, 54, 55, 63, 61, 56, 57, 58,
	59, 60, 64, 62, 81, 82, 84, 69, 70, 120,
	67, 68, 71, 72, 65, 66, 83, 9, 46, 128,
	142, 128, 122, 143, 1, 91, 2, 138, 7, 145,
	130, 15, 10, 11, 50, 44, 128, 122, 148, 54,
	55, 63, 61, 56, 57, 58, 59, 60, 64, 62,
	49, 0, 0, 69, 70, 0, 67, 68, 71, 72,
	65, 66, 9, 23, 24, 22, 35, 37, 40, 0,
	0, 0, 129, 0, 27, 28, 29, 38, 39, 0,
	0, 0, 0, 36, 31, 0, 26, 0, 56, 57,
	58, 59, 60, 64, 62, 0, 25, 0, 0, 30,
	54, 55, 63, 61, 56, 57, 58, 59, 60, 64,
	62, 0, 0, 0, 69, 70, 90, 67, 68, 71,
	72, 65, 66, 0, 0, 0, 0, 0, 0, 0,
	149, 54, 55, 63, 61, 56, 57, 58, 59, 60,
	64, 62, 0, 0, 0, 69, 70, 0, 67, 68,
	71, 72, 65, 66, 0, 0, 0, 0, 0, 0,
	0, 137, 54, 55, 63, 61, 56, 57, 58, 59,
	60, 64, 62, 0, 0, 0, 69, 70, 0, 67,
	68, 71, 72, 65, 66, 0, 0, 0, 0, 0,
	0, 0, 136, 54, 55, 63, 61, 56, 57, 58,
	59, 60, 64, 62, 0, 0, 0, 69, 70, 0,
	67, 68, 71, 72, 65, 66, 0, 0, 0, 0,
	0, 0, 0, 135, 54, 55, 63, 61, 56, 57,
	58, 59, 60, 64, 62, 0, 0, 0, 69, 70,
	0, 67, 68, 71, 72, 65, 66, 0, 0, 0,
	0, 0, 0, 0, 113, 54, 55, 63, 61, 56,
	57, 58, 59, 60, 64, 62, 0, 0, 0, 69,
	70, 0, 67, 68, 71, 72, 65, 66, 54, 55,
	63, 61, 56, 57, 58, 59, 60, 64, 62, 0,
	0, 0, 69, 70, 0, 67, 68, 71, 72, 65,
	9, 23, 24, 22, 35, 37, 40, 0, 16, 17,
	18, 0, 27, 28, 29, 38, 39, 50, 0, 0,
	0, 36, 31, 0, 26, 0, 0, 0, 0, 0,
	0, 0, 45, 49, 25, 0, 0, 30, 9, 23,
	24, 22, 35, 37, 40, 0, 16, 17, 18, 0,
	27, 28, 29, 38, 39, 0, 0, 0, 0, 36,
	31, 0, 26, 0, 9, 23, 24, 22, 35, 37,
	40, 0, 25, 0, 0, 30, 27, 28, 29, 38,
	39, 0, 0, 0, 0, 36, 31, 0, 26, 0,
	0, 0, 0, 0, 0, 0, 9, 0, 25, 9,
	0, 30, 54, 55, 63, 61, 56, 57, 58, 59,
	60, 64, 62, 50, 125, 126, 0, 125, 126, 0,
	0, 127, 0, 0, 127, 0, 0, 0, 0, 49,
	0, 0, 134,
}
var annotationsPact = [...]int{

	71, 30, -1000, 71, 81, 58, 58, 30, -13, -39,
	-1000, -1000, 444, 406, 79, -33, -1000, -1000, -1000, 340,
	-14, -1000, -1000, -1000, -1000, 470, 470, -5, -8, -10,
	470, 470, -1000, -1000, -1000, -1000, 106, -1000, -1000, -1000,
	-1000, -40, -1000, -51, -1000, 406, -1000, 45, -1000, 168,
	39, -14, -1000, -1000, 470, 470, 470, 470, 470, 470,
	470, 470, 470, 470, 470, 470, 470, 470, 470, 470,
	470, 470, 470, 470, 309, -1000, 470, 470, 470, -1000,
	-1000, -1000, -1000, -1000, -1000, -1000, 406, 406, -45, 406,
	502, 124, 505, 169, 169, -1000, -1000, -1000, -1000, -1000,
	169, -1000, 169, -1000, 9, 363, 487, 487, 487, 487,
	487, 487, 278, -1000, 247, 216, 78, -1000, -1000, -1000,
	-46, -1000, -1000, -1000, -1000, 44, 10, 123, -1000, 502,
	-41, -1000, -1000, -1000, 470, -1000, -1000, -1000, 470, -1000,
	-35, -36, -1000, -1000, 502, 185, -1000, -1000, -1000, -1000,
}
var annotationsPgo = [...]int{

	0, 134, 136, 29, 12, 145, 2, 7, 5, 3,
	140, 4, 1, 8, 128, 0, 102, 101, 90, 87,
}
var annotationsR1 = [...]int{

	0, 1, 1, 1, 1, 1, 1, 2, 2, 2,
	12, 12, 13, 13, 13, 6, 6, 6, 7, 7,
	9, 9, 9, 10, 10, 10, 8, 8, 11, 14,
	14, 14, 14, 3, 3, 3, 4, 4, 5, 15,
	15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
	15, 15, 15, 15, 15, 15, 15, 15, 15, 15,
	15, 16, 16, 16, 17, 17, 18, 18, 18, 18,
	18, 19, 19,
}
var annotationsR2 = [...]int{

	0, 1, 2, 3, 2, 3, 2, 2, 5, 5,
	1, 3, 3, 1, 4, 1, 3, 5, 1, 4,
	1, 1, 1, 1, 1, 1, 3, 3, 2, 1,
	1, 1, 1, 1, 3, 2, 1, 1, 3, 1,
	4, 1, 1, 1, 1, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 2, 4, 4,
	6, 3, 3, 3, 3, 3, 3, 3, 3, 2,
	2, 1, 1, 1, 1, 2, 1, 2, 1, 1,
	2, 1, 2,
}
var annotationsChk = [...]int{

	-1000, -1, -2, 15, 24, 15, 54, -1, -12, 4,
	-2, -2, 38, 36, 57, -14, 12, 13, 14, -15,
	-12, -16, 7, 5, 6, 38, 28, 16, 17, 18,
	41, 26, -17, -18, -19, 8, 25, 9, 19, 20,
	10, -3, -4, -13, -5, 36, -14, -6, -7, 37,
	21, -12, 4, 55, 25, 26, 29, 30, 31, 32,
	33, 28, 35, 27, 34, 46, 47, 42, 43, 39,
	40, 44, 45, 38, -15, -15, 38, 38, 38, -15,
	-15, 8, 9, 20, 10, 56, 59, 60, -3, 36,
	58, -15, 37, -15, -15, -15, -15, -15, -15, -15,
	-15, -15, -15, -15, -15, -15, -15, -15, -15, -15,
	-15, -15, -15, 55, -15, -15, -15, -4, -13, 56,
	-3, -9, -6, -8, -11, 22, 23, 29, -12, 58,
	-10, -7, -8, -11, 37, 55, 55, 55, 59, 56,
	36, 36, -6, -9, 58, -15, 56, 56, -9, 55,
}
var annotationsDef = [...]int{

	0, -2, 1, 0, 0, 4, 6, 2, 7, 10,
	3, 5, 0, 0, 0, 0, 29, 30, 31, 32,
	39, 41, 42, 43, 44, 0, 0, 0, 0, 0,
	0, 0, 71, 72, 73, 74, 0, 76, 78, 79,
	81, 0, 33, 36, 37, 0, 13, 0, 15, 0,
	0, -2, 11, 8, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 57, 0, 0, 0, 69,
	70, 75, 77, 80, 82, 9, 35, 0, 0, 0,
	0, 0, 0, 46, 47, 48, 49, 50, 51, 52,
	53, 54, 55, 56, 61, 62, 63, 64, 65, 66,
	67, 68, 0, 45, 0, 0, 0, 34, 38, 12,
	0, 16, 20, 21, 22, 0, 0, 0, 18, 0,
	0, 23, 24, 25, 0, 40, 58, 59, 0, 14,
	0, 0, 28, 19, 0, 0, 26, 27, 17, 60,
}
var annotationsTok1 = [...]int{

	1, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 41, 3, 50, 49, 31, 35, 3,
	38, 55, 29, 25, 59, 26, 57, 30, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 60, 54,
	40, 48, 39, 52, 24, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 37, 51, 58, 28, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	3, 3, 3, 36, 27, 56,
}
var annotationsTok2 = [...]int{

	2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
	12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
	22, 23, 32, 33, 34, 42, 43, 44, 45, 46,
	47, 53,
}
var annotationsTok3 = [...]int{
	0,
}

var annotationsErrorMessages = [...]struct {
	state int
	token int
	msg   string
}{}

//line yaccpar:1

/*	parser for yacc output	*/

var (
	annotationsDebug        = 0
	annotationsErrorVerbose = false
)

type annotationsLexer interface {
	Lex(lval *annotationsSymType) int
	Error(s string)
}

type annotationsParser interface {
	Parse(annotationsLexer) int
	Lookahead() int
}

type annotationsParserImpl struct {
	lval  annotationsSymType
	stack [annotationsInitialStackSize]annotationsSymType
	char  int
}

func (p *annotationsParserImpl) Lookahead() int {
	return p.char
}

func annotationsNewParser() annotationsParser {
	return &annotationsParserImpl{}
}

const annotationsFlag = -1000

func annotationsTokname(c int) string {
	if c >= 1 && c-1 < len(annotationsToknames) {
		if annotationsToknames[c-1] != "" {
			return annotationsToknames[c-1]
		}
	}
	return __yyfmt__.Sprintf("tok-%v", c)
}

func annotationsStatname(s int) string {
	if s >= 0 && s < len(annotationsStatenames) {
		if annotationsStatenames[s] != "" {
			return annotationsStatenames[s]
		}
	}
	return __yyfmt__.Sprintf("state-%v", s)
}

func annotationsErrorMessage(state, lookAhead int) string {
	const TOKSTART = 4

	if !annotationsErrorVerbose {
		return "syntax error"
	}

	for _, e := range annotationsErrorMessages {
		if e.state == state && e.token == lookAhead {
			return "syntax error: " + e.msg
		}
	}

	res := "syntax error: unexpected " + annotationsTokname(lookAhead)

	// To match Bison, suggest at most four expected tokens.
	expected := make([]int, 0, 4)

	// Look for shiftable tokens.
	base := annotationsPact[state]
	for tok := TOKSTART; tok-1 < len(annotationsToknames); tok++ {
		if n := base + tok; n >= 0 && n < annotationsLast && annotationsChk[annotationsAct[n]] == tok {
			if len(expected) == cap(expected) {
				return res
			}
			expected = append(expected, tok)
		}
	}

	if annotationsDef[state] == -2 {
		i := 0
		for annotationsExca[i] != -1 || annotationsExca[i+1] != state {
			i += 2
		}

		// Look for tokens that we accept or reduce.
		for i += 2; annotationsExca[i] >= 0; i += 2 {
			tok := annotationsExca[i]
			if tok < TOKSTART || annotationsExca[i+1] == 0 {
				continue
			}
			if len(expected) == cap(expected) {
				return res
			}
			expected = append(expected, tok)
		}

		// If the default action is to accept or reduce, give up.
		if annotationsExca[i+1] != 0 {
			return res
		}
	}

	for i, tok := range expected {
		if i == 0 {
			res += ", expecting "
		} else {
			res += " or "
		}
		res += annotationsTokname(tok)
	}
	return res
}

func annotationslex1(lex annotationsLexer, lval *annotationsSymType) (char, token int) {
	token = 0
	char = lex.Lex(lval)
	if char <= 0 {
		token = annotationsTok1[0]
		goto out
	}
	if char < len(annotationsTok1) {
		token = annotationsTok1[char]
		goto out
	}
	if char >= annotationsPrivate {
		if char < annotationsPrivate+len(annotationsTok2) {
			token = annotationsTok2[char-annotationsPrivate]
			goto out
		}
	}
	for i := 0; i < len(annotationsTok3); i += 2 {
		token = annotationsTok3[i+0]
		if token == char {
			token = annotationsTok3[i+1]
			goto out
		}
	}

out:
	if token == 0 {
		token = annotationsTok2[1] /* unknown char */
	}
	if annotationsDebug >= 3 {
		__yyfmt__.Printf("lex %s(%d)\n", annotationsTokname(token), uint(char))
	}
	return char, token
}

func annotationsParse(annotationslex annotationsLexer) int {
	return annotationsNewParser().Parse(annotationslex)
}

func (annotationsrcvr *annotationsParserImpl) Parse(annotationslex annotationsLexer) int {
	var annotationsn int
	var annotationsVAL annotationsSymType
	var annotationsDollar []annotationsSymType
	_ = annotationsDollar // silence set and not used
	annotationsS := annotationsrcvr.stack[:]

	Nerrs := 0   /* number of errors */
	Errflag := 0 /* error recovery flag */
	annotationsstate := 0
	annotationsrcvr.char = -1
	annotationstoken := -1 // annotationsrcvr.char translated into internal numbering
	defer func() {
		// Make sure we report no lookahead when not parsing.
		annotationsstate = -1
		annotationsrcvr.char = -1
		annotationstoken = -1
	}()
	annotationsp := -1
	goto annotationsstack

ret0:
	return 0

ret1:
	return 1

annotationsstack:
	/* put a state and value onto the stack */
	if annotationsDebug >= 4 {
		__yyfmt__.Printf("char %v in %v\n", annotationsTokname(annotationstoken), annotationsStatname(annotationsstate))
	}

	annotationsp++
	if annotationsp >= len(annotationsS) {
		nyys := make([]annotationsSymType, len(annotationsS)*2)
		copy(nyys, annotationsS)
		annotationsS = nyys
	}
	annotationsS[annotationsp] = annotationsVAL
	annotationsS[annotationsp].yys = annotationsstate

annotationsnewstate:
	annotationsn = annotationsPact[annotationsstate]
	if annotationsn <= annotationsFlag {
		goto annotationsdefault /* simple state */
	}
	if annotationsrcvr.char < 0 {
		annotationsrcvr.char, annotationstoken = annotationslex1(annotationslex, &annotationsrcvr.lval)
	}
	annotationsn += annotationstoken
	if annotationsn < 0 || annotationsn >= annotationsLast {
		goto annotationsdefault
	}
	annotationsn = annotationsAct[annotationsn]
	if annotationsChk[annotationsn] == annotationstoken { /* valid shift */
		annotationsrcvr.char = -1
		annotationstoken = -1
		annotationsVAL = annotationsrcvr.lval
		annotationsstate = annotationsn
		if Errflag > 0 {
			Errflag--
		}
		goto annotationsstack
	}

annotationsdefault:
	/* default state action */
	annotationsn = annotationsDef[annotationsstate]
	if annotationsn == -2 {
		if annotationsrcvr.char < 0 {
			annotationsrcvr.char, annotationstoken = annotationslex1(annotationslex, &annotationsrcvr.lval)
		}

		/* look through exception table */
		xi := 0
		for {
			if annotationsExca[xi+0] == -1 && annotationsExca[xi+1] == annotationsstate {
				break
			}
			xi += 2
		}
		for xi += 2; ; xi += 2 {
			annotationsn = annotationsExca[xi+0]
			if annotationsn < 0 || annotationsn == annotationstoken {
				break
			}
		}
		annotationsn = annotationsExca[xi+1]
		if annotationsn < 0 {
			goto ret0
		}
	}
	if annotationsn == 0 {
		/* error ... attempt to resume parsing */
		switch Errflag {
		case 0: /* brand new error */
			annotationslex.Error(annotationsErrorMessage(annotationsstate, annotationstoken))
			Nerrs++
			if annotationsDebug >= 1 {
				__yyfmt__.Printf("%s", annotationsStatname(annotationsstate))
				__yyfmt__.Printf(" saw %s\n", annotationsTokname(annotationstoken))
			}
			fallthrough

		case 1, 2: /* incompletely recovered error ... try again */
			Errflag = 3

			/* find a state where "error" is a legal shift action */
			for annotationsp >= 0 {
				annotationsn = annotationsPact[annotationsS[annotationsp].yys] + annotationsErrCode
				if annotationsn >= 0 && annotationsn < annotationsLast {
					annotationsstate = annotationsAct[annotationsn] /* simulate a shift of "error" */
					if annotationsChk[annotationsstate] == annotationsErrCode {
						goto annotationsstack
					}
				}

				/* the current p has no shift on "error", pop stack */
				if annotationsDebug >= 2 {
					__yyfmt__.Printf("error recovery pops state %d\n", annotationsS[annotationsp].yys)
				}
				annotationsp--
			}
			/* there is no state on the stack with an error shift ... abort */
			goto ret1

		case 3: /* no shift yet; clobber input char */
			if annotationsDebug >= 2 {
				__yyfmt__.Printf("error recovery discards %s\n", annotationsTokname(annotationstoken))
			}
			if annotationstoken == annotationsEofCode {
				goto ret1
			}
			annotationsrcvr.char = -1
			annotationstoken = -1
			goto annotationsnewstate /* try again in the same state */
		}
	}

	/* reduction by production annotationsn */
	if annotationsDebug >= 2 {
		__yyfmt__.Printf("reduce %v in:\n\t%v\n", annotationsn, annotationsStatname(annotationsstate))
	}

	annotationsnt := annotationsn
	annotationspt := annotationsp
	_ = annotationspt // guard against "declared and not used"

	annotationsp -= annotationsR2[annotationsn]
	// annotationsp is now the index of $0. Perform the default action. Iff the
	// reduced production is ε, $1 is possibly out of range.
	if annotationsp+1 >= len(annotationsS) {
		nyys := make([]annotationsSymType, len(annotationsS)*2)
		copy(nyys, annotationsS)
		annotationsS = nyys
	}
	annotationsVAL = annotationsS[annotationsp+1]

	/* consult goto table to find next state */
	annotationsn = annotationsR1[annotationsn]
	annotationsg := annotationsPgo[annotationsn]
	annotationsj := annotationsg + annotationsS[annotationsp].yys + 1

	if annotationsj >= annotationsLast {
		annotationsstate = annotationsAct[annotationsg]
	} else {
		annotationsstate = annotationsAct[annotationsj]
		if annotationsChk[annotationsstate] != -annotationsn {
			annotationsstate = annotationsAct[annotationsg]
		}
	}
	// dummy call; replaced with literal code
	switch annotationsnt {

	case 1:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:56
		{
			annotationsVAL.an = annotationsDollar[1].an
			annotationslex.(*annoLex).res = annotationsVAL.an
		}
	case 2:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:60
		{
			annotationsVAL.an = annotationsDollar[2].an
			annotationslex.(*annoLex).res = annotationsVAL.an
		}
	case 3:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:64
		{
			annotationsVAL.an = append(annotationsDollar[1].an, annotationsDollar[3].an...)
			annotationslex.(*annoLex).res = annotationsVAL.an
		}
	case 4:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:68
		{
			annotationsVAL.an = annotationsDollar[1].an
			annotationslex.(*annoLex).res = annotationsVAL.an
		}
	case 5:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:72
		{
			annotationsVAL.an = append(annotationsDollar[1].an, annotationsDollar[3].an...)
			annotationslex.(*annoLex).res = annotationsVAL.an
		}
	case 6:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:76
		{
			annotationsVAL.an = annotationsDollar[1].an
			annotationslex.(*annoLex).res = annotationsVAL.an
		}
	case 7:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:81
		{
			annotationsVAL.an = []Annotation{{Type: annotationsDollar[2].id, Pos: annotationsDollar[1].p}}
		}
	case 8:
		annotationsDollar = annotationsS[annotationspt-5 : annotationspt+1]
		//line annotations.y:84
		{
			annotationsVAL.an = []Annotation{{Type: annotationsDollar[2].id, Value: annotationsDollar[4].v, Pos: annotationsDollar[1].p}}
		}
	case 9:
		annotationsDollar = annotationsS[annotationspt-5 : annotationspt+1]
		//line annotations.y:87
		{
			annotationsVAL.an = []Annotation{{Type: annotationsDollar[2].id, Value: AggregateNode{Contents: annotationsDollar[4].els, pos: annotationsDollar[3].p}, Pos: annotationsDollar[1].p}}
		}
	case 10:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:91
		{
			annotationsVAL.id = Identifier{Name: annotationsDollar[1].n.Val, Pos: annotationsDollar[1].n.Pos}
		}
	case 11:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:94
		{
			annotationsVAL.id = Identifier{PackageAlias: annotationsDollar[1].n.Val, Name: annotationsDollar[3].n.Val, Pos: annotationsDollar[1].n.Pos}
		}
	case 12:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:98
		{
			annotationsVAL.v = AggregateNode{Contents: annotationsDollar[2].els, pos: annotationsDollar[1].p}
		}
	case 14:
		annotationsDollar = annotationsS[annotationspt-4 : annotationspt+1]
		//line annotations.y:102
		{
			annotationsVAL.v = TypedExpressionNode{Type: annotationsDollar[1].t, Value: AggregateNode{Contents: annotationsDollar[3].els, pos: annotationsDollar[2].p}}
		}
	case 16:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:107
		{
			annotationsVAL.t = sliceType{elem: annotationsDollar[3].t, pos: annotationsDollar[1].p}
		}
	case 17:
		annotationsDollar = annotationsS[annotationspt-5 : annotationspt+1]
		//line annotations.y:110
		{
			annotationsVAL.t = mapType{key: annotationsDollar[3].t, elem: annotationsDollar[5].t, pos: annotationsDollar[1].p}
		}
	case 18:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:114
		{
			annotationsVAL.t = namedType{name: annotationsDollar[1].id}
		}
	case 19:
		annotationsDollar = annotationsS[annotationspt-4 : annotationspt+1]
		//line annotations.y:117
		{
			annotationsVAL.t = arrayType{len: annotationsDollar[2].v, elem: annotationsDollar[4].t, pos: annotationsDollar[1].p}
		}
	case 26:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:129
		{
			annotationsVAL.t = emptyType{isStruct: true, pos: annotationsDollar[1].p}
		}
	case 27:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:132
		{
			annotationsVAL.t = emptyType{isStruct: false, pos: annotationsDollar[1].p}
		}
	case 28:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:136
		{
			annotationsVAL.t = pointerType{pos: annotationsDollar[1].p, elem: annotationsDollar[2].t}
		}
	case 29:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:140
		{
			annotationsVAL.v = LiteralNode{pos: annotationsDollar[1].p} // leave val nil
		}
	case 30:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:143
		{
			annotationsVAL.v = LiteralNode{Val: constant.MakeBool(true), pos: annotationsDollar[1].p}
		}
	case 31:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:146
		{
			annotationsVAL.v = LiteralNode{Val: constant.MakeBool(false), pos: annotationsDollar[1].p}
		}
	case 33:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:151
		{
			annotationsVAL.els = []Element{annotationsDollar[1].el}
		}
	case 34:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:154
		{
			if annotationsDollar[1].els[0].HasKey != annotationsDollar[3].el.HasKey {
				annotationslex.Error("element list cannot contain a mix of map and array style elements (e.g. with and without key)")
			}
			annotationsVAL.els = append(annotationsDollar[1].els, annotationsDollar[3].el)
		}
	case 35:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:160
		{
			annotationsVAL.els = annotationsDollar[1].els
		}
	case 36:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:164
		{
			annotationsVAL.el = Element{Value: annotationsDollar[1].v}
		}
	case 38:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:169
		{
			annotationsVAL.el = Element{Key: annotationsDollar[1].v, HasKey: true, Value: annotationsDollar[3].v}
		}
	case 39:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:173
		{
			annotationsVAL.v = RefNode{Ident: annotationsDollar[1].id}
		}
	case 40:
		annotationsDollar = annotationsS[annotationspt-4 : annotationspt+1]
		//line annotations.y:176
		{
			annotationsVAL.v = TypedExpressionNode{Type: namedType{name: annotationsDollar[1].id}, Value: annotationsDollar[3].v}
		}
	case 41:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:179
		{
			annotationsVAL.v = annotationsDollar[1].v
		}
	case 42:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:182
		{
			annotationsVAL.v = annotationsDollar[1].l
		}
	case 43:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:185
		{
			annotationsVAL.v = annotationsDollar[1].l
		}
	case 44:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:188
		{
			annotationsVAL.v = annotationsDollar[1].l
		}
	case 45:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:191
		{
			annotationsVAL.v = ParenthesizedExpressionNode{Contents: annotationsDollar[2].v, pos: annotationsDollar[1].p}
		}
	case 46:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:194
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "+", OperatorPos: annotationsDollar[2].p}
		}
	case 47:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:197
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "-", OperatorPos: annotationsDollar[2].p}
		}
	case 48:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:200
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "*", OperatorPos: annotationsDollar[2].p}
		}
	case 49:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:203
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "/", OperatorPos: annotationsDollar[2].p}
		}
	case 50:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:206
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "%", OperatorPos: annotationsDollar[2].p}
		}
	case 51:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:209
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "<<", OperatorPos: annotationsDollar[2].p}
		}
	case 52:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:212
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: ">>", OperatorPos: annotationsDollar[2].p}
		}
	case 53:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:215
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "^", OperatorPos: annotationsDollar[2].p}
		}
	case 54:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:218
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "&", OperatorPos: annotationsDollar[2].p}
		}
	case 55:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:221
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "|", OperatorPos: annotationsDollar[2].p}
		}
	case 56:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:224
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "&^", OperatorPos: annotationsDollar[2].p}
		}
	case 57:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:227
		{
			annotationsVAL.v = PrefixOperatorNode{Value: annotationsDollar[2].v, Operator: "^", pos: annotationsDollar[1].p}
		}
	case 58:
		annotationsDollar = annotationsS[annotationspt-4 : annotationspt+1]
		//line annotations.y:230
		{
			annotationsVAL.v = InvokeRealNode{Argument: annotationsDollar[3].v, pos: annotationsDollar[1].p}
		}
	case 59:
		annotationsDollar = annotationsS[annotationspt-4 : annotationspt+1]
		//line annotations.y:233
		{
			annotationsVAL.v = InvokeImagNode{Argument: annotationsDollar[3].v, pos: annotationsDollar[1].p}
		}
	case 60:
		annotationsDollar = annotationsS[annotationspt-6 : annotationspt+1]
		//line annotations.y:236
		{
			annotationsVAL.v = InvokeComplexNode{RealArg: annotationsDollar[3].v, ImagArg: annotationsDollar[5].v, pos: annotationsDollar[1].p}
		}
	case 61:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:239
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "&&", OperatorPos: annotationsDollar[2].p}
		}
	case 62:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:242
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "||", OperatorPos: annotationsDollar[2].p}
		}
	case 63:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:245
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "==", OperatorPos: annotationsDollar[2].p}
		}
	case 64:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:248
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "!=", OperatorPos: annotationsDollar[2].p}
		}
	case 65:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:251
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: ">", OperatorPos: annotationsDollar[2].p}
		}
	case 66:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:254
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "<", OperatorPos: annotationsDollar[2].p}
		}
	case 67:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:257
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: ">=", OperatorPos: annotationsDollar[2].p}
		}
	case 68:
		annotationsDollar = annotationsS[annotationspt-3 : annotationspt+1]
		//line annotations.y:260
		{
			annotationsVAL.v = BinaryOperatorNode{Left: annotationsDollar[1].v, Right: annotationsDollar[3].v, Operator: "<=", OperatorPos: annotationsDollar[2].p}
		}
	case 69:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:263
		{
			annotationsVAL.v = PrefixOperatorNode{Value: annotationsDollar[2].v, Operator: "!", pos: annotationsDollar[1].p}
		}
	case 70:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:266
		{
			annotationsVAL.v = PrefixOperatorNode{Value: annotationsDollar[2].v, Operator: "-", pos: annotationsDollar[1].p}
		}
	case 74:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:274
		{
			annotationsVAL.v = annotationsDollar[1].l
		}
	case 75:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:277
		{
			annotationsVAL.v = annotationsDollar[2].l
		}
	case 76:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:281
		{
			annotationsVAL.v = annotationsDollar[1].l
		}
	case 77:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:284
		{
			annotationsVAL.v = LiteralNode{Val: annotationsDollar[2].l.Val, pos: annotationsDollar[1].p}
		}
	case 78:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:287
		{
			annotationsVAL.v = LiteralNode{Val: constant.MakeUnknown(), pos: annotationsDollar[1].p}
		}
	case 79:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:290
		{
			annotationsVAL.v = LiteralNode{Val: constant.MakeFloat64(math.Inf(1)), pos: annotationsDollar[1].p}
		}
	case 80:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:293
		{
			annotationsVAL.v = LiteralNode{Val: constant.MakeFloat64(math.Inf(1)), pos: annotationsDollar[1].p}
		}
	case 81:
		annotationsDollar = annotationsS[annotationspt-1 : annotationspt+1]
		//line annotations.y:297
		{
			annotationsVAL.v = annotationsDollar[1].l
		}
	case 82:
		annotationsDollar = annotationsS[annotationspt-2 : annotationspt+1]
		//line annotations.y:300
		{
			annotationsVAL.v = LiteralNode{Val: annotationsDollar[2].l.Val, pos: annotationsDollar[1].p}
		}
	}
	goto annotationsstack /* stack new state and value */
}
