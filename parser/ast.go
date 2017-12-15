package parser

import (
	"fmt"
	"go/constant"
	"text/scanner"
)

// ExpressionNode is a node in the AST for constant expressions (including
// arithmetic operations) and aggregate values (e.g. maps, slices, arrays, and
// structs).
type ExpressionNode interface {
	Pos() scanner.Position
}

// nameNode is an AST node that represents a simple name/identifier.
type nameNode struct {
	Val string
	Pos scanner.Position
}

// LiteralNode is an expression node that represents a literal value, such as a
// number, boolean, or string.
type LiteralNode struct {
	Val constant.Value // nil if literal nil
	pos scanner.Position
}

func (n LiteralNode) Pos() scanner.Position {
	return n.pos
}

// RefNode is an expression node that is a reference to an identifier, which is
// expected to resolve to a constant or function name.
type RefNode struct {
	Ident Identifier
}

func (n RefNode) Pos() scanner.Position {
	return n.Ident.Pos
}

// BinaryOperatorNode is an expression node that represents an binary operator
// and its two arguments. These include arithmetic operations (including
// bitwise ones), logical and comparison operations, and string concatenation.
type BinaryOperatorNode struct {
	Left, Right ExpressionNode
	Operator    string
	OperatorPos scanner.Position
}

func (n BinaryOperatorNode) Pos() scanner.Position {
	return n.Left.Pos()
}

// ParenthesizedExpressionNode is an expression node that represents an
// expression surrounded by parentheses.
type ParenthesizedExpressionNode struct {
	Contents ExpressionNode
	pos      scanner.Position
}

func (n ParenthesizedExpressionNode) Pos() scanner.Position {
	return n.pos
}

// TypedExpressionNode is an expression node that represents a type conversion.
// It consists of the target type and an expression that is the value.
type TypedExpressionNode struct {
	Type  Type
	Value ExpressionNode
}

func (n TypedExpressionNode) Pos() scanner.Position {
	return n.Type.Pos()
}

// PrefixOperatorNode is an expression node that represents a prefix operator.
// This includes logical not (!), bitwise not (^), and unary minus (-).
type PrefixOperatorNode struct {
	Operator string
	Value    ExpressionNode
	pos      scanner.Position
}

func (n PrefixOperatorNode) Pos() scanner.Position {
	return n.pos
}

// InvokeRealNode is an expression node that represents the invocation of the
// built-in function real, which takes a complex number and returns the real
// portion.
type InvokeRealNode struct {
	Argument ExpressionNode
	pos      scanner.Position
}

func (n InvokeRealNode) Pos() scanner.Position {
	return n.pos
}

// InvokeImagNode is an expression node that represents the invocation of the
// built-in function imag, which takes a complex number and returns the
// imaginary portion.
type InvokeImagNode struct {
	Argument ExpressionNode
	pos      scanner.Position
}

func (n InvokeImagNode) Pos() scanner.Position {
	return n.pos
}

// InvokeComplexNode is an expression node that represents the invocation of the
// built-in function complex, which takes two values and assembles a complex
// number from them. The expression complex(X, Y) is the same as X + Yi if X and
// Y are numeric literals.
type InvokeComplexNode struct {
	RealArg, ImagArg ExpressionNode
	pos              scanner.Position
}

func (n InvokeComplexNode) Pos() scanner.Position {
	return n.pos
}

// AggregateNode is an expression node that represents an aggregate value, which
// could be a slice/array, a map, or a struct value.
type AggregateNode struct {
	Contents []Element
	pos      scanner.Position
}

func (n AggregateNode) Pos() scanner.Position {
	return n.pos
}

// Identifier is an AST node that refers to an identifier, possibly qualified
// with a package name/alias.
type Identifier struct {
	PackageAlias string
	Name         string
	Pos          scanner.Position
}

func (id Identifier) String() string {
	if id.PackageAlias == "" {
		return id.Name
	} else {
		return fmt.Sprintf("%s.%s", id.PackageAlias, id.Name)
	}
}

// Element is an AST node for a component of an aggregate value. Aggregates that
// represent arrays or slices will not have keys. Aggregates that represent
// structs may or may not have keys. Aggregates that represent maps must have
// keys.
type Element struct {
	Key    ExpressionNode
	HasKey bool
	Value  ExpressionNode
}

func (e Element) Pos() scanner.Position {
	if e.HasKey {
		return e.Key.Pos()
	} else {
		return e.Value.Pos()
	}
}

// Annotation is a fully parsed annotation. It identifies the annotation type
// and has an optional value. If the value is not present, it is assumed to be
// "true" for annotations whose underlying type is bool or an empty struct. If
// the named annotation type is neither a bool nor a struct, a value must be
// supplied.
type Annotation struct {
	Type  Identifier
	Value ExpressionNode
	Pos   scanner.Position
}

// Type is an AST node that represents a type reference. Type references in
// annotations are limited. Unlike full Go syntax, annotations cannot reference
// channel types or anonymous structs or interfaces other than the empty struct
// and empty interface.
type Type interface {
	Name() Identifier
	Elem() Type
	Key() Type
	Len() ExpressionNode
	IsSlice() bool
	IsMap() bool
	IsArray() bool
	IsNamed() bool
	IsPointer() bool
	IsEmptyStruct() bool
	IsEmptyInterface() bool
	Pos() scanner.Position
}

type namedType struct {
	name Identifier
}

func (t namedType) Name() Identifier       { return t.name }
func (t namedType) Elem() Type             { return nil }
func (t namedType) Key() Type              { return nil }
func (t namedType) Len() ExpressionNode    { return nil }
func (t namedType) IsSlice() bool          { return false }
func (t namedType) IsMap() bool            { return false }
func (t namedType) IsArray() bool          { return false }
func (t namedType) IsNamed() bool          { return true }
func (t namedType) IsPointer() bool        { return false }
func (t namedType) IsEmptyStruct() bool    { return false }
func (t namedType) IsEmptyInterface() bool { return false }
func (t namedType) Pos() scanner.Position  { return t.name.Pos }

type arrayType struct {
	len  ExpressionNode
	elem Type
	pos  scanner.Position
}

func (t arrayType) Name() Identifier       { return Identifier{} }
func (t arrayType) Elem() Type             { return t.elem }
func (t arrayType) Key() Type              { return nil }
func (t arrayType) Len() ExpressionNode    { return t.len }
func (t arrayType) IsSlice() bool          { return false }
func (t arrayType) IsMap() bool            { return false }
func (t arrayType) IsArray() bool          { return true }
func (t arrayType) IsNamed() bool          { return false }
func (t arrayType) IsPointer() bool        { return false }
func (t arrayType) IsEmptyStruct() bool    { return false }
func (t arrayType) IsEmptyInterface() bool { return false }
func (t arrayType) Pos() scanner.Position  { return t.pos }

type sliceType struct {
	elem Type
	pos  scanner.Position
}

func (t sliceType) Name() Identifier       { return Identifier{} }
func (t sliceType) Elem() Type             { return t.elem }
func (t sliceType) Key() Type              { return nil }
func (t sliceType) Len() ExpressionNode    { return nil }
func (t sliceType) IsSlice() bool          { return true }
func (t sliceType) IsMap() bool            { return false }
func (t sliceType) IsArray() bool          { return false }
func (t sliceType) IsNamed() bool          { return false }
func (t sliceType) IsPointer() bool        { return false }
func (t sliceType) IsEmptyStruct() bool    { return false }
func (t sliceType) IsEmptyInterface() bool { return false }
func (t sliceType) Pos() scanner.Position  { return t.pos }

type mapType struct {
	key  Type
	elem Type
	pos  scanner.Position
}

func (t mapType) Name() Identifier       { return Identifier{} }
func (t mapType) Elem() Type             { return t.elem }
func (t mapType) Key() Type              { return t.key }
func (t mapType) Len() ExpressionNode    { return nil }
func (t mapType) IsSlice() bool          { return false }
func (t mapType) IsMap() bool            { return true }
func (t mapType) IsArray() bool          { return false }
func (t mapType) IsNamed() bool          { return false }
func (t mapType) IsPointer() bool        { return false }
func (t mapType) IsEmptyStruct() bool    { return false }
func (t mapType) IsEmptyInterface() bool { return false }
func (t mapType) Pos() scanner.Position  { return t.pos }

type emptyType struct {
	isStruct bool
	pos      scanner.Position
}

func (t emptyType) Name() Identifier       { return Identifier{} }
func (t emptyType) Elem() Type             { return nil }
func (t emptyType) Key() Type              { return nil }
func (t emptyType) Len() ExpressionNode    { return nil }
func (t emptyType) IsSlice() bool          { return false }
func (t emptyType) IsMap() bool            { return false }
func (t emptyType) IsArray() bool          { return false }
func (t emptyType) IsNamed() bool          { return false }
func (t emptyType) IsPointer() bool        { return false }
func (t emptyType) IsEmptyStruct() bool    { return t.isStruct }
func (t emptyType) IsEmptyInterface() bool { return !t.isStruct }
func (t emptyType) Pos() scanner.Position  { return t.pos }

type pointerType struct {
	elem Type
	pos  scanner.Position
}

func (t pointerType) Name() Identifier       { return Identifier{} }
func (t pointerType) Elem() Type             { return t.elem }
func (t pointerType) Key() Type              { return nil }
func (t pointerType) Len() ExpressionNode    { return nil }
func (t pointerType) IsSlice() bool          { return false }
func (t pointerType) IsMap() bool            { return false }
func (t pointerType) IsArray() bool          { return false }
func (t pointerType) IsNamed() bool          { return false }
func (t pointerType) IsPointer() bool        { return true }
func (t pointerType) IsEmptyStruct() bool    { return false }
func (t pointerType) IsEmptyInterface() bool { return false }
func (t pointerType) Pos() scanner.Position  { return t.pos }
