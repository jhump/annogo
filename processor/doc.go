// Package processor contains the runtime library used by code that processes
// annotations.
//
// This package defines an interface, Processor, which is implemented by things
// that can process annotations.
//
//    func(ctx *Context, output processor.OutputFactory) error
//
// Processing is generally expected to validate annotation values and,
// optionally, generate code that is derived from the annotation values.
//
// If a processor returns an error, processing has failed and the error should
// indicate why -- which can indicate that an annotation value is invalid for
// some reason. Validation errors should be constructed with
// processor.NewErrorWithPosition so that they can report locations in the
// source code, to aid users in resolving the error.
//
// The OutputFactory passed to the processor may be used to generate code. When
// generating source code, a process should use the factory to create an output
// whose path includes both the Go import path and source file name. The factory
// can be used with the WriteGoFiles function in the github.com/jhump/gopoet
// package, making it easy to author Go source code from an annotation
// processor.
//
// The remaining APIs and types in this package can be broken into three main
// categories: Processor Registration, Processor Invocation, and Mirrors.
//
// Processor Registration
//
// Processor implementations can be registered with this package using the
// RegisterProcessor method. All registered processors can later be queried with
// the AllRegisteredProcessors function. These can be used to create
// command-line tools that will run custom processors. The aptgo program
// (included in this repo) can load Go plugins, which can register custom
// processor implementations in their package init functions. This allows the
// aptgo to run custom processors, in addition to its default processing (which
// makes runtime-visible annotations accessible via the functions in the
// annogo package).
//
// Processor Invocation
//
// The package includes functions and types used to invoke processors. Key among
// them is processor.Config. This struct defines the packages that will be
// processed, the processors that will be invoked, and the output factory (which
// controls where generated output files are actually written).
//
// After a processor.Config is constructed, its Execute method is used to
// actually invoke the configured processors. This process involves parsing the
// source code for all packages to process, performing full type analysis on
// the sources, and then extracting annotations. Once annotations are extracted,
// they are passed to each configured processor, via the processor.Context, one
// package at a time.
//
// There are also some "shortcut" methods in this package: Process and
// ProcessAll. These functions create a processor.Config using the arguments
// given and using "typical" values for other settings and then call the
// resulting config's Execute method. The ProcessAll method will invoke all
// processors that have been registered with this package (via the
// RegisterProcessor function).
//
// Mirrors
//
// The "mirrors" API consists of several key types:
//
// AnnotationMirror: The mirror is a representation of the annotation that
// processors can query. The mirror refers to an AnnotationMetadata instance,
// which describes the annotation type, and to an AnnotationValue instance,
// which describes the actual value.
//
// AnnotationMetadata: The metadata describes the type of an annotation. It
// augments the "go/types" representation of the type by also providing resolved
// values for attributes interesting to an annotation processor, such as the
// values of the types' @annogo.Annotation annotations.
//
// AnnotationValue: Since it is possible that the annotation types are not
// "known" to a processor (e.g. not compiled and linked into the processor
// implementation), these values must expose the data in a way that is similar
// to reflection, where the consumer need not know the actual type(s) ahead of
// time. Unlike reflect.Value, an AnnotationValue can only represent a valid
// annotation value (not all values and types in Go are valid as annotation
// values). An AnnotationValue also includes information about the position in
// source code where the values were defined (to assist with good error
// reporting). Finally, an AnnotationValue that refers to other program elements
// (such as a struct field, constant, or function) does so via values that
// implement types.Object (in particular, *types.Var for fields, *types.Const
// for constants, and *types.Func for functions).
//
// AnnotatedElement: An annotated element is an element in Go source that has
// annotations. This struct provides access to the Go program element via the
// corresponding types.Object as well as references to the element in the
// program AST. It also provides access to AnnotationMirror instances for every
// annotation present on the element.
//
// Processors can inspect the annotated elements and corresponding annotation
// mirrors, to inspect and validate values and/or to generate code derived from
// them. The entry point for this inspection is the processor.Context (provided
// to the processor when it is invoked), which provides several ways to query
// for annotated elements in a package.
package processor
