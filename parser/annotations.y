%{
package parser

//lint:file-ignore U1000 generated parser has unused symbols

import (
	"go/constant"
	"math"
	"text/scanner"
)

%}

// fields inside this union end up as the fields in a structure known
// as ${PREFIX}SymType, of which a reference is passed to the lexer.
%union{
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

// any non-terminal which returns a value needs a type, which is
// really a field name in the above union struct
%type <an>  annotations annotation
%type <els> elements
%type <el>  element keyAndVal
%type <t>   type constrainedType emptyType elemType keyType pointerType
%type <id>  identifier
%type <v>   val scalar scalarExpression numericLit intLit floatLit imagLit

// same for terminals
%token <n>   _IDENT
%token <l>   _STRING_LIT _RAW_STRING_LIT _RUNE_LIT _INT_LIT _FLOAT_LIT _IMAG_LIT
%token <err> _ERROR
%token <p>   _NIL _TRUE _FALSE _EOL _REAL _IMAG _COMPLEX _NAN _INF _MAP _STRUCT _INTERFACE
%token <p>   '@' '+'  '-' '|' '^' '*'  '/' '%' _SHL _SHR _AND_NOT '&'
%token <p>   '{' '[' '(' '>'  '<' '!' _EQ _NEQ _GTE _LTE _AND _OR
// these are unused operators, but we want the parser to be able to emit
// better error messages when it sees them instead of reporting "$unk"
%token <p>   '=' '$' '#' '\\' '?'

%left _OR
%left _AND
%left _EQ, _NEQ, _GTE, '>', _LTE, '<'
%left '+'  '-' '|' '^'
%left '*'  '/' '%' _SHL _SHR _AND_NOT '&'
%left '!' _MINUS

%%

annotations : annotation {
		$$ = $1
		annotationslex.(*annoLex).res = $$
	}
	| _EOL annotations {
		$$ = $2
		annotationslex.(*annoLex).res = $$
	}
	| annotations _EOL annotation {
		$$ = append($1, $3...)
		annotationslex.(*annoLex).res = $$
	}
	| annotations _EOL {
		$$ = $1
		annotationslex.(*annoLex).res = $$
	}
	| annotations ';' annotation {
		$$ = append($1, $3...)
		annotationslex.(*annoLex).res = $$
	}
	| annotations ';' {
		$$ = $1
		annotationslex.(*annoLex).res = $$
	}

annotation : '@' identifier {
		$$ = []Annotation{ {Type: $2, Pos: $1} }
	}
	| '@' identifier '(' scalar ')' {
		$$ = []Annotation{ {Type: $2, Value: $4, Pos: $1} }
	}
	| '@' identifier '{' elements '}' {
		$$ = []Annotation{ {Type: $2, Value: AggregateNode{Contents: $4, pos: $3}, Pos: $1} }
	}

identifier : _IDENT {
		$$ = Identifier{Name: $1.Val, Pos: $1.Pos}
	}
	| _IDENT '.' _IDENT {
		$$ = Identifier{PackageAlias: $1.Val, Name: $3.Val, Pos: $1.Pos}
	}

val : '{' elements '}' {
		$$ = AggregateNode{Contents: $2, pos: $1}
	}
	| scalar
	| type '{' elements '}' {
		$$ = TypedExpressionNode{Type: $1, Value: AggregateNode{Contents: $3, pos: $2}}
	}

type : constrainedType
	| '[' ']' elemType {
		$$ = sliceType{elem: $3, pos: $1}
	}
	| _MAP '[' keyType ']' elemType {
		$$ = mapType{key: $3, elem: $5, pos: $1}
	}

constrainedType : identifier {
		$$ = namedType{name: $1}
	}
	| '[' scalarExpression ']' elemType {
		$$ = arrayType{len: $2, elem: $4, pos: $1}
	}

elemType : type
	| emptyType
	| pointerType

keyType : constrainedType
	| emptyType
	| pointerType

emptyType : _STRUCT '{' '}' {
		$$ = emptyType{isStruct: true, pos: $1}
	}
	| _INTERFACE '{' '}' {
		$$ = emptyType{isStruct: false, pos: $1}
	}

pointerType : '*' type {
		$$ = pointerType{pos: $1, elem: $2}
	}

scalar : _NIL {
		$$ = LiteralNode{pos: $1} // leave val nil
	}
	| _TRUE {
		$$ = LiteralNode{Val: constant.MakeBool(true), pos: $1}
	}
	| _FALSE {
		$$ = LiteralNode{Val: constant.MakeBool(false), pos: $1}
	}
	| scalarExpression

elements : element {
		$$ = []Element{$1}
	}
	| elements ',' element {
		if $1[0].HasKey != $3.HasKey {
			annotationslex.Error("element list cannot contain a mix of map and array style elements (e.g. with and without key)")
		}
		$$ = append($1, $3)
	}
	| elements ',' {
		$$ = $1
	}

element : val {
		$$ = Element{Value: $1}
	}
	| keyAndVal

keyAndVal : val ':' val {
		$$ = Element{Key: $1, HasKey: true, Value: $3}
	}

scalarExpression : identifier {
		$$ = RefNode{Ident: $1}
	}
	| identifier '(' scalarExpression ')' {
		$$ = TypedExpressionNode{Type: namedType{name: $1}, Value: $3}
	}
	| numericLit {
		$$ = $1
	}
	| _RUNE_LIT {
		$$ = $1
	}
	| _STRING_LIT {
		$$ = $1
	}
	| _RAW_STRING_LIT {
		$$ = $1
	}
	| '(' scalarExpression ')' {
		$$ = ParenthesizedExpressionNode{Contents: $2, pos: $1}
	}
	| scalarExpression '+' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "+", OperatorPos: $2}
	}
	| scalarExpression '-' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "-", OperatorPos: $2}
	}
	| scalarExpression '*' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "*", OperatorPos: $2}
	}
	| scalarExpression '/' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "/", OperatorPos: $2}
	}
	| scalarExpression '%' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "%", OperatorPos: $2}
	}
	| scalarExpression _SHL scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "<<", OperatorPos: $2}
	}
	| scalarExpression _SHR scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: ">>", OperatorPos: $2}
	}
	| scalarExpression '^' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "^", OperatorPos: $2}
	}
	| scalarExpression '&' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "&", OperatorPos: $2}
	}
	| scalarExpression '|' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "|", OperatorPos: $2}
	}
	| scalarExpression _AND_NOT scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "&^", OperatorPos: $2}
	}
	| '^' scalarExpression   %prec '!' {
		$$ = PrefixOperatorNode{Value: $2, Operator: "^", pos: $1}
	}
	| _REAL '(' scalarExpression ')' {
		$$ = InvokeRealNode{Argument: $3, pos: $1}
	}
	| _IMAG '(' scalarExpression ')' {
		$$ = InvokeImagNode{Argument: $3, pos: $1}
	}
	| _COMPLEX '(' scalarExpression ',' scalarExpression ')' {
		$$ = InvokeComplexNode{RealArg: $3, ImagArg: $5, pos: $1}
	}
	| scalarExpression _AND scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "&&", OperatorPos: $2}
	}
	| scalarExpression _OR scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "||", OperatorPos: $2}
	}
	| scalarExpression _EQ scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "==", OperatorPos: $2}
	}
	| scalarExpression _NEQ scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "!=", OperatorPos: $2}
	}
	| scalarExpression '>' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: ">", OperatorPos: $2}
	}
	| scalarExpression '<' scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "<", OperatorPos: $2}
	}
	| scalarExpression _GTE scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: ">=", OperatorPos: $2}
	}
	| scalarExpression _LTE scalarExpression {
		$$ = BinaryOperatorNode{Left: $1, Right: $3, Operator: "<=", OperatorPos: $2}
	}
	| '!' scalarExpression {
		$$ = PrefixOperatorNode{Value: $2, Operator: "!", pos: $1}
	}
	| '-' scalarExpression   %prec _MINUS  {
		$$ = PrefixOperatorNode{Value: $2, Operator: "-", pos: $1}
	}

numericLit : intLit
	| floatLit
	| imagLit

intLit : _INT_LIT {
		$$ = $1
	}
	| '+' _INT_LIT {
		$$ = $2
	}

floatLit : _FLOAT_LIT {
		$$ = $1
	}
	| '+' _FLOAT_LIT {
		$$ = LiteralNode{Val: $2.Val, pos: $1}
	}
	| _NAN {
		$$ = LiteralNode{Val: constant.MakeUnknown(), pos: $1}
	}
	| _INF {
		$$ = LiteralNode{Val: constant.MakeFloat64(math.Inf(1)), pos: $1}
	}
	| '+' _INF {
		$$ = LiteralNode{Val: constant.MakeFloat64(math.Inf(1)), pos: $1}
	}

imagLit : _IMAG_LIT {
		$$ = $1
	}
	| '+' _IMAG_LIT {
		$$ = LiteralNode{Val: $2.Val, pos: $1}
	}

%%