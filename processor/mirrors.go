package processor

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"sort"

	"github.com/jhump/annogo"
)

// AnnotationMirror is a view of an annotation instance that appears in source.
type AnnotationMirror struct {
	// Metadata for the type of the annotation.
	Metadata *AnnotationMetadata
	// The location in source where this annotation is defined.
	Pos token.Position
	// The actual value of the annotation.
	Value AnnotationValue
}

func (m AnnotationMirror) annoType() annoType {
	return annoType{packagePath: m.Metadata.Type.Pkg().Path(), name: m.Metadata.Type.Name()}
}

// AnnotationMetadata is metadata that describes an annotation type. This
// contains some of the same attributes as found on annotations.Annotation, but
// it describes an annotation that exists in source, not as a runtime type. It
// includes all metadata that an annotation processor might find relevant.
type AnnotationMetadata struct {
	// The annotation type.
	Type *types.TypeName
	// The type used to describe the annotation. This can differ from the actual
	// annotation type if it defines a FactoryFunc, which transforms the
	// representation (data in an instance of the annotation) into the right
	// type.
	Representation types.Type

	// Since annotation types are themselves annotated (they have, at a minimum,
	// @annogo.Annotation), this field is a view of the type as an
	// annotated element.
	Element *AnnotatedElement

	// A function used to transform a value, as it appears in source, into the
	// right type. Corresponds to the value of an @annogo.FactoryFunc
	// annotation.
	FactoryFunc types.Object
	// If true, the annotation can be extracted at runtime using annotation
	// registration APIs. Corresponds to the field of the same name on the
	// @annogo.Annotation.
	RuntimeVisible bool
	// The kinds of elements on which this annotation is allowed to appear.
	// Corresponds to the field of the same name on the @annogo.Annotation.
	AllowedElements []annogo.ElementType
	// If true, this annotation can appear more than once on a single annotated
	// element. Corresponds to the field of the same name on the
	// @annogo.Annotation.
	AllowRepeated bool

	// The set of fields that are required. These fields must be present in all
	// instances of the annotation in source. Fields that are not required will
	// take a default value (if specified) or the type's zero value if they are
	// not specified in source. These are all fields that had an
	// @annogo.Required annotation.
	RequiredFields map[string]bool
	// The default values, for fields that have defaults other than their type's
	// zero value. These are all fields that had an @annogo.DefaultValue
	// annotation.
	DefaultFieldValues map[string]AnnotationValue
}

// ValueKind indicates the type of underlying value for an annotation.
type ValueKind int

const (
	// KindInvalid should not be used and indicates an incorrectly uninitialized
	// kind.
	KindInvalid ValueKind = iota
	// KindInt is for values whose type is a signed integer.
	KindInt
	// KindUint is for values whose type is an unsigned integer.
	KindUint
	// KindFloat is for values whose type is a floating point number.
	KindFloat
	// KindComplex is for values whose type is a complex number.
	KindComplex
	// KindString is for values whose type is a string.
	KindString
	// KindBool is for values whose type is a boolean.
	KindBool
	// KindNil is for values whose type is nil.
	KindNil
	// KindFunc is for values whose type is the name of a function or method.
	KindFunc
	// KindSlice is for values whose type is a slice or array.
	KindSlice
	// KindMap is for values whose type is a map.
	KindMap
	// KindStruct is for values whose type is a struct.
	KindStruct
)

var kindNames = map[ValueKind]string{
	KindInt:     "int",
	KindUint:    "uint",
	KindFloat:   "float",
	KindComplex: "complex",
	KindString:  "string",
	KindBool:    "bool",
	KindNil:     "nil",
	KindFunc:    "func",
	KindSlice:   "slice",
	KindMap:     "map",
	KindStruct:  "struct",
}

func (k ValueKind) String() string {
	if s, ok := kindNames[k]; ok {
		return s
	}
	return "<invalid>"
}

// AnnotationStructEntry represents a field in an annotation value whose type
// is a struct.
type AnnotationStructEntry struct {
	// The field in the struct.
	Field *types.Var
	// The position in source where this field value is defined.
	Pos token.Position
	// The value of the field.
	Value AnnotationValue
}

// AnnotationMapEntry represents an entry, key-value pair, in an annotation
// whose type is a map.
type AnnotationMapEntry struct {
	// The key for this map entry.
	Key AnnotationValue
	// The value for this map entry.
	Value AnnotationValue
}

// AnnotationValue represents the value of an annotation. All values are
// constant expressions, known at compile-time.
type AnnotationValue struct {
	// The type of the value.
	Type types.Type
	// The kind of the value.
	Kind ValueKind
	// The actual value. It will be an int64, uint64, float64, complex128, bool,
	// or string for scalar values. For function values, it will be a
	// *types.Func that refers to a top-level function or method. For aggregate
	// values, it will be a []AnnotationValue, []AnnotationMapEntry, or
	// []AnnotationStructEntry.
	Value interface{}
	// If the value is a reference to a constant, this is the constant that
	// was referenced. Expressions that include references to constants do not
	// count. E.g. the expression "someConst + 1" would result in a nil Ref, but
	// "someConst" would result in a non-nil Ref to someConst.
	Ref *types.Const

	// The position in source where this annotation value is defined.
	Pos token.Position
}

// AsInt is a convenience function that type asserts the value as an int64.
func (v AnnotationValue) AsInt() int64 {
	return v.Value.(int64)
}

// AsUint is a convenience function that type asserts the value as a uint64.
func (v AnnotationValue) AsUint() uint64 {
	return v.Value.(uint64)
}

// AsFloat is a convenience function that type asserts the value as a float64.
func (v AnnotationValue) AsFloat() float64 {
	return v.Value.(float64)
}

// AsComplex is a convenience function that type asserts the value as a
// complex128.
func (v AnnotationValue) AsComplex() complex128 {
	return v.Value.(complex128)
}

// AsString is a convenience function that type asserts the value as a string.
func (v AnnotationValue) AsString() string {
	return v.Value.(string)
}

// AsBool is a convenience function that type asserts the value as a bool.
func (v AnnotationValue) AsBool() bool {
	return v.Value.(bool)
}

// AsFunc is a convenience function that type asserts the value as a
// *types.Func.
func (v AnnotationValue) AsFunc() *types.Func {
	return v.Value.(*types.Func)
}

// AsSlice is a convenience function that type asserts the value as a
// []AnnotationValue.
func (v AnnotationValue) AsSlice() []AnnotationValue {
	return v.Value.([]AnnotationValue)
}

// AsMap is a convenience function that type asserts the value as a
// []AnnotationMapEntry.
func (v AnnotationValue) AsMap() []AnnotationMapEntry {
	return v.Value.([]AnnotationMapEntry)
}

// AsStruct is a convenience function that type asserts the value as a
// []AnnotationStructEntry.
func (v AnnotationValue) AsStruct() []AnnotationStructEntry {
	return v.Value.([]AnnotationStructEntry)
}

var nilValue AnnotationValue

func newValue(t types.Type, k ValueKind, v interface{}, pos token.Position) AnnotationValue {
	switch k {
	case KindInt:
		if _, ok := v.(int64); !ok {
			panic(fmt.Sprintf("value of kind int has non-int64 val: %T", v))
		}
	case KindUint:
		if _, ok := v.(uint64); !ok {
			panic(fmt.Sprintf("value of kind uint has non-uint64 val: %T", v))
		}
	case KindFloat:
		if _, ok := v.(float64); !ok {
			panic(fmt.Sprintf("value of kind float has non-float64 val: %T", v))
		}
	case KindComplex:
		if _, ok := v.(complex128); !ok {
			panic(fmt.Sprintf("value of kind complex has non-complex128 val: %T", v))
		}
	case KindBool:
		if _, ok := v.(bool); !ok {
			panic(fmt.Sprintf("value of kind bool has non-bool val: %T", v))
		}
	case KindString:
		if _, ok := v.(string); !ok {
			panic(fmt.Sprintf("value of kind string has non-string val: %T", v))
		}
	case KindFunc:
		if _, ok := v.(*types.Func); !ok {
			panic(fmt.Sprintf("value of kind func has non-func val: %T", v))
		}
	case KindStruct:
		if _, ok := v.([]AnnotationStructEntry); !ok {
			panic(fmt.Sprintf("value of kind struct has non-struct val: %T", v))
		}
	case KindMap:
		if _, ok := v.([]AnnotationMapEntry); !ok {
			panic(fmt.Sprintf("value of kind map has non-map val: %T", v))
		}
	case KindSlice:
		if _, ok := v.([]AnnotationValue); !ok {
			panic(fmt.Sprintf("value of kind slice has non-slice val: %T", v))
		}
	case KindNil:
		if v != nil {
			panic(fmt.Sprintf("value of kind nil has non-nil val: %T", v))
		}
	default:
		panic("attempt to create annotation value with invalid kind")
	}
	return AnnotationValue{Type: t, Kind: k, Value: v, Pos: pos}
}

// AnnotatedElement is a view of an element in source that has annotations. It
// can represent any Go source element on which annotations are allowed: a type,
// a struct field, an interface method or embed, a function or method, a
// variable, or a constant.
type AnnotatedElement struct {
	// The actual source element, as a types.Object.
	Obj types.Object
	// The element's name/identifier in the source AST.
	Ident *ast.Ident
	// The AST for the file in which this element is defined.
	File *ast.File

	// Child elements. The children of structs are fields. The children of
	// interfaces are methods and embeds. Other elements do not have children.
	Children []*AnnotatedElement
	// The element's parent. The parent of a field will be the enclosing struct.
	// The parent of an interface method or embed will be the enclosing
	// interface.
	Parent *AnnotatedElement

	// The processor context for the package in which this element is defined.
	Context *Context

	// The element types that apply to this element.
	ApplicableTypes []annogo.ElementType
	// The annotations defined on this element.
	Annotations []AnnotationMirror
}

// IsElementType returns true if this element is the given element type. It is
// considered to be the given type if the given type appears in the element's
// set of applicable types.
func (e *AnnotatedElement) IsElementType(et annogo.ElementType) bool {
	for _, e := range e.ApplicableTypes {
		if e == et {
			return true
		}
	}
	return false
}

// GetDeclaringFilename gets the name of the file that declared this element.
func (e *AnnotatedElement) GetDeclaringFilename() string {
	p := e.Context.Program.Fset.Position(e.File.Package)
	return p.Filename
}

// FindAnnotations returns annotation mirrors whose annotation type is the given
// type. The given type is described by its package path and name.
func (e *AnnotatedElement) FindAnnotations(packagePath, name string) []AnnotationMirror {
	return findAnnotations(e.Annotations, packagePath, name)
}

func findAnnotations(annos []AnnotationMirror, packagePath, name string) []AnnotationMirror {
	i := sort.Search(len(annos), func(n int) bool {
		t := annos[n].Metadata.Type
		if t.Pkg().Path() == packagePath {
			return t.Name() >= name
		}
		return t.Pkg().Path() > packagePath
	})
	at := annoType{packagePath: packagePath, name: name}
	var matches []AnnotationMirror
	for i < len(annos) {
		m := annos[i]
		if m.annoType() != at {
			break
		}
		matches = append(matches, m)
		i++
	}
	return matches
}

type annoType struct {
	packagePath, name string
}

// TODO: Helper method to populate an actual annotation value from a mirror
// (for processors where annotations of interest are known and linked in)
