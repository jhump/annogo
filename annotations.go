package annogo

import "fmt"

//go:generate aptgo github.com/jhump/annogo

// Annotation is a meta-annotation. Other types that will be used as annotations
// should be annotated with it. For example:
//
//    // @annogo.Annotation
//    type MyNewAnnotation struct {
//        Foo int
//        Bar []string
//    }
//
// Running the annotation processor on the package containing this type will
// generate code for accessing the runtime-visible annotations. Only annotations
// on top-level elements (and fields and methods of top-level types) are
// accessible. Annotations on constants are not accessible unless they are typed
// constants (then accessible via type and constant name). Annotations on
// variables are accessible using the variable's address unless the variable's
// storage size is zero. (This applies to variables whose type is an empty
// struct or an array of empty structs.) Otherwise, the variables are accessible
// using the variable's type and name.
//
// Annotations on constructs defined inside of function and method bodies
// (types and fields thereof) are not allowed.
//
// @Annotation{AllowedElements: Types}
type Annotation struct {
	// RuntimeVisible indicates that the annotation is queryable at runtime
	// using APIs in this package. This field is effectively ignored if
	// AllowedElements disallows all element types.
	//
	// @DefaultValue(true)
	RuntimeVisible bool

	// AllowedElements indicates the kinds of elements that can be annotated. If
	// it is empty, the annotation can be used on any kind of element.
	AllowedElements []ElementType

	// AllowRepeated indicates whether an element can have more than one
	// annotation of this type on it.
	AllowRepeated bool
}

// FactoryFunc names a function that produces values for the annotated type or
// field. The function should take a single argument (which represents the
// structure of the annotation's value) and return a single value, which must
// have the same type as the annotated type or field.
//
// Factories are necessary for types that need to maintain extra invariants or
// provide stronger encapsulation. For example, an immutable value must use a
// factory function since, to provide immutability, its fields/contents must be
// unexported.
//
// @Annotation{AllowedElements: {Types, Fields}}
type FactoryFunc func(AnyType) SelfType

// DefaultValue is an annotation that indicates a default value for an
// annotation field. It is not valid to use on types or methods. It is also not
// valid to use with fields in structs that are not themselves annotations.
//
// @Annotation{AllowedElements: AnnotationFields}
type DefaultValue struct {
	// @Required
	Value SelfType
}

// Required is an annotation that indicates an annotation field that must be
// defined in an annotation. When this annotation is not present, fields that
// are not defined in a given annotation will use the zero value for the field's
// type or a DefaultValue if that annotation is present. But when this
// annotation is present, they may not assume a default value; the annotation
// must explicitly define a value.
//
// @Annotation{AllowedElements: AnnotationFields}
type Required bool

// ElementType is an enumeration of the kinds of elements that can be annotated.
type ElementType int

const (
	// AnnotationTypes are type elements that are themselves annotations (e.g.
	// annotated with @annogo.Annotation).
	//
	// Only top-level, named types can be annotated. Types defined inside of
	// functions and methods cannot be annotated.
	AnnotationTypes ElementType = iota

	// AnnotationFields are fields of annotation types that are structs. Only
	// fields of top-level, named types can be annotated.
	AnnotationFields

	// Types are type elements. This is a superset of AnnotationTypes and is
	// also the union of ConcreteTypes and Interfaces.
	//
	// Only top-level, named types can be annotated. Types defined inside of
	// functions and methods cannot be annotated.
	Types

	// ConcreteTypes are type elements that are *not* interfaces. This is a
	// subset of Types. This is the complement of Interfaces. The union of
	// ConcreteTypes and Interfaces is the same as Types.
	//
	// Only top-level, named types can be annotated.
	ConcreteTypes

	// Interfaces are type elements that are defined to be interfaces. This is a
	// subset of Types. This is the complement of ConcreteTypes. The union of
	// ConcreteTypes and Interfaces is the same as Types.
	//
	// Only top-level, named interfaces can be annotated.
	Interfaces

	// Fields are fields of struct type elements. Only fields of top-level,
	// named types can be annotated. Annotations that allow fields will also
	// allow annotation fields (e.g. fields of annotation types).
	Fields

	// Methods are method elements. Only methods of top-level, named types
	// can be annotated.
	Methods

	// InterfaceMethods are the methods that comprise an interface. Only methods
	// of top-level, named interfaces can be annotated.
	InterfaceMethods

	// InterfaceEmbeds are the interface types embedded in another interface.
	// Only the embeds of top-level, named interfaces can be annotated.
	InterfaceEmbeds

	// Functions are top-level, named functions. Methods for named types are
	// functions, too. So an annotation that allows function elements also allows
	// such method elements. However, this only applies to methods that have
	// bodies: interface methods are not allowed unless InterfaceMethods is also
	// used.
	Functions

	// Variables are top-level (e.g. package-level) variables. Variables inside
	// function and method bodies cannot be annotated.
	Variables

	// Constants are top-level (e.g. package-level) constants. Constants that
	// are defined inside of function and method bodies cannot be annotated.
	Constants
)

func (et ElementType) String() string {
	switch et {
	case AnnotationTypes:
		return "annotation types"
	case AnnotationFields:
		return "annotation fields"
	case Types:
		return "types"
	case ConcreteTypes:
		return "concrete types"
	case Interfaces:
		return "interfaces"
	case Fields:
		return "fields"
	case Methods:
		return "methods"
	case InterfaceMethods:
		return "interface methods"
	case InterfaceEmbeds:
		return "interface embeds"
	case Functions:
		return "functions"
	case Variables:
		return "variables"
	case Constants:
		return "constants"
	default:
		return fmt.Sprintf("?%d?", int(et))
	}
}

// AnyType is a special marker type that is used in annotation fields where
// one might use interface{} in an ordinary Go struct. The annotation parser
// allows any kind of value.
//
// The kinds of values that the parser may assign to an AnyType field follow:
//   1. Numeric types: int64, uint64, float64, or complex128
//   2. Character types: string, rune
//   3. Other scalars: bool
//   4. Structs. This is the type used for values that appear to be structs. A
//      value appears to be a struct if it has the shape of a map but all of its
//      keys are unqualified identifiers, in which case the identifiers are
//      taken to be field names. The concrete type of an AnyType with a struct
//      value will be an unnamed struct type.
//   5. Arrays of any supported type. The array's element type will be AnyType
//      if the contents appear heterogenous. Note that struct elements may
//      appear to be heterogenous since types are strictly inferred from the
//      constants in the annotation data. So if one element has a field named
//      "Foo" and an unsigned integer value, but another has a field of the same
//      name with signed (e.g. negative) integer value, they will be interpreted
//      as heterogenous elements. Similarly, arrays of arrays appear
//      heterogenous if the elements have different lengths. (AnyType values
//      do not use slices, only arrays.)
//   6. Maps whose keys and values are of any supported type. Like any other map
//      in the Go language, maps cannot be used as keys in other maps. The map's
//      key and/or value types will be AnyType if they appear heterogenous. See
//      above remarks about heterogenous array elements for why struct values
//      can appear heterogenous even when that is not the intent.
//
// Note that this marker type can also be used inside function types and has a
// similar special meaning in that context. For example, consider the following
// annotation type:
//
//    // @annogo.Annotation
//    type Foo struct {
//        Make     func(string) annogo.AnyType
//        Registry func(map[string]annogo.AnyType) error
//    }
//
// In this case, an annotation value can refer to any function that accepts a
// string and returns one value, regardless of the actual return type, for the
// field named "Make". Similarly, the value can refer to any function that
// accepts a map with string keys and returns error, regardless of the map's
// value type, for the field named "Registry". Note that channels of, pointers
// to, and structs with AnyTypes are not supported this way; only array, slice,
// and map types are.
//
// When used this way, the actual underlying function and its signature type can
// be queried using GetFunctionDetails. The actual annotation value will be a
// synthetic function that delegates to the correct one. For functions that
// accept or return an array, slice, or map type that references AnyType, the
// synthetic function will have to copy the arguments/return values to translate
// between the declared and actual signatures.
type AnyType interface{}

// SelfType is a special marker type. When an annotation is a struct that
// contains a field of this type, the value of that field will be the same
// type as the annotated type. This can only be used to annotate types and
// fields (in which case it refers to the field's type). It cannot be used
// in annotations on methods.
//
// Note that the kinds of values that can actually be described in an
// annotation are limited. For example, function types can only refer to named
// functions. Channels aren't supported at all.
//
// Like AnyType, this marker type can also be used inside function types and has
// a similar special meaning in that context.
type SelfType interface{}
