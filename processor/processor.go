package processor

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/constant"
	goparser "go/parser"
	"go/token"
	"go/types"
	"math"
	"reflect"
	"sort"
	"strings"
	"text/scanner"
	"unicode"
	"unicode/utf8"

	"golang.org/x/tools/go/loader"

	"github.com/jhump/annogo"
	"github.com/jhump/annogo/parser"
)

var (
	emptyInterface = types.NewInterface(nil, nil)
	emptyStruct    = types.NewStruct(nil, nil)

	anyTypePkg, anyTypeName, selfTypePkg, selfTypeName string
	requiredPkg, requiredName, defaultPkg, defaultName string

	annoTypeForAnnotation, annoTypeForRequired, annoTypeForDefault annoType
)

func init() {
	f := func(rt reflect.Type) (string, string) {
		return rt.PkgPath(), rt.Name()
	}

	annotationPkg, annotationName := f(reflect.TypeOf(annogo.Annotation{}))
	annoTypeForAnnotation = annoType{packagePath: annotationPkg, name: annotationName}

	requiredPkg, requiredName = f(reflect.TypeOf(annogo.Required(false)))
	defaultPkg, defaultName = f(reflect.TypeOf(annogo.DefaultValue{}))
	annoTypeForRequired = annoType{packagePath: requiredPkg, name: requiredName}
	annoTypeForDefault = annoType{packagePath: defaultPkg, name: defaultName}

	anyTypePkg, anyTypeName = f(reflect.TypeOf((*annogo.AnyType)(nil)).Elem())
	selfTypePkg, selfTypeName = f(reflect.TypeOf((*annogo.SelfType)(nil)).Elem())
}

// ErrorWithPosition is an error that has source position information associated
// with it. The position indicates the location in a source file where the error
// was encountered.
type ErrorWithPosition struct {
	err error
	pos token.Position
}

func (e *ErrorWithPosition) Error() string {
	return fmt.Sprintf("%s:%d:%d: %s", e.pos.Filename, e.pos.Line, e.pos.Column, e.err.Error())
}

func (e *ErrorWithPosition) Underlying() error {
	return e.err
}

func (e *ErrorWithPosition) Pos() token.Position {
	return e.pos
}

func posError(pos token.Position, err error) error {
	return &ErrorWithPosition{err: err, pos: pos}
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

// Processor is a function that acts on annotations and is invoked from the
// annotation processor tool. Typical processor implementations generate code
// based on the annotations present in source.
type Processor func(ctx *Context, outputDir string) error

// ProcessAll invokes all registered Processor instances to process the given
// package.
func ProcessAll(pkgPath string, includeTest bool, outputDir string) error {
	return Process(pkgPath, includeTest, outputDir, AllRegisteredProcessors()...)
}

func Process(pkgPath string, includeTest bool, outputDir string, procs ...Processor) error {
	conf := loader.Config{
		ParserMode:          goparser.ParseComments,
		TypeCheckFuncBodies: func(string) bool { return false },
		ImportPkgs:          map[string]bool{pkgPath: includeTest},
	}
	prg, err := conf.Load()
	if err != nil {
		return err
	}
	allContexts := map[*types.Package]*Context{}
	for _, pkgInfo := range prg.InitialPackages() {
		ctx := newContext(outputDir, pkgInfo, prg, allContexts, includeTest)
		if err := ctx.computeAllAnnotations(); err != nil {
			return err
		}
		for _, proc := range procs {
			if err := proc(ctx, outputDir); err != nil {
				return err
			}
		}
	}
	return nil
}

type annoType struct {
	packagePath, name string
}

// Context represents the environment for an annotation processor. It represents
// a single package (for which the processors were invoked). It provides access
// to all annotations and annotated elements encountered in the package.
type Context struct {
	Package     *loader.PackageInfo
	Program     *loader.Program
	includeTest bool
	outputDir   string

	allContexts   map[*types.Package]*Context
	metadata      map[*types.TypeName]*AnnotationMetadata
	fieldMetadata map[types.Object][]AnnotationMirror

	AllElements        map[types.Object]*AnnotatedElement
	AllAnnotationTypes map[string][]string
	byType             map[annogo.ElementType][]*AnnotatedElement
	byAnnotation       map[annoType][]*AnnotatedElement
	processed          map[*ast.CommentGroup]struct{}
}

func newContext(outputDir string, pkg *loader.PackageInfo, prg *loader.Program, contextPool map[*types.Package]*Context, includeTest bool) *Context {
	ctx := &Context{
		Package:            pkg,
		Program:            prg,
		includeTest:        includeTest,
		outputDir:          outputDir,
		AllElements:        map[types.Object]*AnnotatedElement{},
		AllAnnotationTypes: map[string][]string{},
		byType:             map[annogo.ElementType][]*AnnotatedElement{},
		byAnnotation:       map[annoType][]*AnnotatedElement{},
		allContexts:        contextPool,
		metadata:           map[*types.TypeName]*AnnotationMetadata{},
		fieldMetadata:      map[types.Object][]AnnotationMirror{},
		processed:          map[*ast.CommentGroup]struct{}{},
	}
	contextPool[pkg.Pkg] = ctx
	return ctx
}

func (c *Context) GetMetadata(packagePath, name string) (*AnnotationMetadata, error) {
	pkgInfo := c.Program.Package(packagePath)
	if pkgInfo == nil {
		// If we don't know about this package, load it
		conf := loader.Config{
			ParserMode:          goparser.ParseComments,
			TypeCheckFuncBodies: func(string) bool { return false },
			ImportPkgs:          map[string]bool{packagePath: c.includeTest},
		}
		prg, err := conf.Load()
		if err != nil {
			return nil, err
		}
		pkgInfo = prg.Package(packagePath)
		ctx := newContext(c.outputDir, pkgInfo, prg, c.allContexts, c.includeTest)
		// share memoized metadata and pool of all contexts
		ctx.metadata = c.metadata
	}

	obj := pkgInfo.Pkg.Scope().Lookup(name)
	if obj == nil {
		return nil, fmt.Errorf("no such symbol %s in package %s", name, packagePath)
	}
	if _, ok := obj.(*types.TypeName); !ok {
		return nil, fmt.Errorf("%s.%s is not a type", packagePath, name)
	}

	return c.getMetadata(obj.(*types.TypeName), pkgInfo, parser.Identifier{PackageAlias: packagePath, Name: name}, nil)
}

func (c *Context) GetMetadataForTypeName(t *types.TypeName) (*AnnotationMetadata, error) {
	pkg := c.Program.AllPackages[t.Pkg()]
	return c.getMetadata(t, pkg, parser.Identifier{PackageAlias: t.Pkg().Path(), Name: t.Name()}, nil)
}

func (c *Context) ElementsOfType(t annogo.ElementType) []*AnnotatedElement {
	return c.byType[t]
}

func (c *Context) ElementsAnnotatedWith(packagePath, typeName string) []*AnnotatedElement {
	return c.byAnnotation[annoType{packagePath: packagePath, name: typeName}]
}

func (c *Context) computeAllAnnotations() error {
	// TODO: accumulate multiple errors (up to some limit... 20?) instead of failing after first
	for _, file := range c.Package.Files {
		if err := c.computeAnnotationsFromFile(file); err != nil {
			return err
		}
	}

	// Now that we've processed everything, create the map that conveys all
	// annotation types. First, de-dup using a map of sets.
	annoTypes := map[string]map[string]struct{}{}
	for a := range c.byAnnotation {
		names := annoTypes[a.packagePath]
		if names == nil {
			names = map[string]struct{}{}
			annoTypes[a.packagePath] = names
		}
		names[a.name] = struct{}{}
	}

	// Then convert that into map of slices.
	for pkg, names := range annoTypes {
		nameSlice := make([]string, len(names))
		i := 0
		for n := range names {
			nameSlice[i] = n
			i++
		}
		sort.Strings(nameSlice)
		c.AllAnnotationTypes[pkg] = nameSlice
	}

	return nil
}

func (c *Context) computeAnnotationsFromFile(file *ast.File) error {
	for _, decl := range file.Decls {
		switch decl := decl.(type) {
		case *ast.GenDecl:
			for _, s := range decl.Specs {
				if decl.Tok == token.CONST || decl.Tok == token.VAR {
					spec := s.(*ast.ValueSpec)
					for _, id := range spec.Names {
						doc := spec.Doc
						if doc == nil || len(doc.List) == 0 {
							doc = decl.Doc
						}
						var et annogo.ElementType
						if decl.Tok == token.CONST {
							et = annogo.Constants
						} else {
							et = annogo.Variables
						}
						if err := c.computeAnnotationsFromElement(file, []annogo.ElementType{et}, id, doc, nil); err != nil {
							return err
						}
					}
				} else if decl.Tok == token.TYPE {
					spec := s.(*ast.TypeSpec)
					doc := spec.Doc
					if doc == nil || len(doc.List) == 0 {
						doc = decl.Doc
					}
					if err := c.computeAnnotationsFromType(file, spec, doc); err != nil {
						return err
					}
				}
			}
		case *ast.FuncDecl:
			var ets []annogo.ElementType
			ets = append(ets, annogo.Functions)
			if decl.Recv != nil {
				ets = append(ets, annogo.Methods)
			}
			if err := c.computeAnnotationsFromElement(file, ets, decl.Name, decl.Doc, nil); err != nil {
				return err
			}
		}
	}

	var err error
	ast.Inspect(file, func(node ast.Node) bool {
		if err != nil {
			return false
		}
		var doc *ast.CommentGroup
		switch node := node.(type) {
		case *ast.ImportSpec:
			doc = node.Doc
		case *ast.TypeSpec:
			doc = node.Doc
		case *ast.ValueSpec:
			doc = node.Doc
		case *ast.GenDecl:
			doc = node.Doc
		case *ast.FuncDecl:
			doc = node.Doc
		case *ast.Field:
			doc = node.Doc
		case *ast.File:
			doc = node.Doc
		}
		if pos, found := hasAnnotations(doc); found {
			p := c.Program.Fset.Position(pos)
			p.String()
			err = fmt.Errorf("%v: annotations are only allowed on top-level types, functions, variables, and constants or fields and methods of top-level types", p)
			return false
		}
		return true
	})
	return err
}

func (c *Context) computeAnnotationsFromElement(file *ast.File, ets []annogo.ElementType, id *ast.Ident, doc *ast.CommentGroup, parent *AnnotatedElement) error {
	obj := c.Package.ObjectOf(id)
	if _, ok := c.AllElements[obj]; ok {
		// already processed this one
		return nil
	}
	annos, err := c.parseAnnotations(file, obj.Type(), doc, modeAll)
	if err != nil {
		return err
	}

	if err := checkAnnotations(annos, ets, false); err != nil {
		return err
	}

	c.newElement(file, id, obj, annos, ets, parent)
	return nil
}

func (c *Context) computeAnnotationsFromType(file *ast.File, spec *ast.TypeSpec, doc *ast.CommentGroup) error {
	id := spec.Name
	obj := c.Package.ObjectOf(id).(*types.TypeName)
	if _, ok := c.AllElements[obj]; ok {
		// already processed this one
		return nil
	}
	annos, err := c.parseAnnotations(file, obj.Type(), doc, modeAll)
	if err != nil {
		return err
	}

	var ets []annogo.ElementType
	ets = append(ets, annogo.Types)
	objType := obj.Type().Underlying()
	isConcrete := false
	if _, ok := objType.(*types.Interface); ok {
		ets = append(ets, annogo.Interfaces)
	} else {
		isConcrete = true
		ets = append(ets, annogo.ConcreteTypes)
	}

	for _, a := range annos {
		// see if this type is actually an annotation type
		if a.annoType() == annoTypeForAnnotation {
			// make sure we have annotation metadata for this one
			if _, err := c.putMetadata(obj, a); err != nil {
				return err
			}
			ets = append(ets, annogo.AnnotationTypes)
			break
		}
	}

	if err := checkAnnotations(annos, ets, isConcrete); err != nil {
		return err
	}

	ae := c.newElement(file, id, obj, annos, ets, nil)
	if iface, ok := spec.Type.(*ast.InterfaceType); ok {
		if iface.Methods != nil {
			for _, method := range iface.Methods.List {
				names := method.Names
				elType := annogo.InterfaceMethods
				if names == nil {
					names = []*ast.Ident{method.Type.(*ast.Ident)}
					elType = annogo.InterfaceEmbeds
				}
				for _, n := range method.Names {
					if err := c.computeAnnotationsFromElement(file, []annogo.ElementType{elType}, n, method.Doc, ae); err != nil {
						return err
					}
				}
			}
		}
	} else if strct, ok := spec.Type.(*ast.StructType); ok {
		if strct.Fields != nil {
			for _, fld := range strct.Fields.List {
				var fieldTypes []annogo.ElementType
				fieldTypes = append(fieldTypes, annogo.Fields)
				for _, et := range ets {
					if et == annogo.AnnotationTypes {
						fieldTypes = append(fieldTypes, annogo.AnnotationFields)
						break
					}
				}

				names := fld.Names
				if names == nil {
					// anonymous/embedded field
					if star, ok := fld.Type.(*ast.StarExpr); ok {
						names = []*ast.Ident{star.X.(*ast.Ident)}
					} else {
						names = []*ast.Ident{fld.Type.(*ast.Ident)}
					}
				}
				for _, n := range names {
					if err := c.computeAnnotationsFromElement(file, fieldTypes, n, fld.Doc, ae); err != nil {
						return err
					}
				}
			}
		}
	}
	return nil
}

func (c *Context) newElement(file *ast.File, id *ast.Ident, obj types.Object, annos []AnnotationMirror, ets []annogo.ElementType, p *AnnotatedElement) *AnnotatedElement {
	ae := &AnnotatedElement{
		Context:         c,
		Ident:           id,
		File:            file,
		Obj:             obj,
		Parent:          p,
		ApplicableTypes: ets,
		Annotations:     annos,
	}
	c.AllElements[obj] = ae
	for _, et := range ets {
		c.byType[et] = append(c.byType[et], ae)
	}
	var prevType annoType
	for _, anno := range annos {
		at := anno.annoType()
		if at != prevType {
			c.byAnnotation[at] = append(c.byAnnotation[at], ae)
			prevType = at
		}
	}
	if p != nil {
		p.Children = append(p.Children, ae)
	}
	return ae
}

type parseMode int

const (
	modeAll parseMode = iota
	modeTypeMetadata
	modeFieldMetadata
)

func (c *Context) parseAnnotations(file *ast.File, selfType types.Type, doc *ast.CommentGroup, mode parseMode) ([]AnnotationMirror, error) {
	c.processed[doc] = struct{}{}
	buf, adjuster := c.extractAnnotations(doc)
	if buf == nil {
		return nil, nil
	}

	var annos []parser.Annotation
	if a, err := parser.ParseAnnotations("", buf); err != nil {
		pos := adjuster.adjustPosition(err.Pos())
		return nil, posError(pos, err.Underlying())
	} else {
		annos = a
	}

	if mode != modeAll {
		var mirrors []AnnotationMirror
		for _, anno := range annos {
			mirror, err := c.convertAnnotation(file, selfType, anno, adjuster, mode)
			if err != nil {
				return nil, err
			}
			if mirror.Metadata != nil {
				if mode == modeTypeMetadata {
					// there is only one kind of annotation we look for in this
					// mode; so if we found one, we're done
					return []AnnotationMirror{mirror}, nil
				} else if mirrors == nil {
					mirrors = make([]AnnotationMirror, 0, 2)
				}
				mirrors = append(mirrors, mirror)
			}
		}
		return mirrors, nil
	}

	mirrors := make([]AnnotationMirror, len(annos))
	for i, anno := range annos {
		var err error
		mirrors[i], err = c.convertAnnotation(file, selfType, anno, adjuster, modeAll)
		if err != nil {
			return nil, err
		}
	}

	// for repeated annotations, group them together; we use stable sort so that
	// elements are re-grouped but otherwise remain in original order
	sort.SliceStable(mirrors, func(i, j int) bool {
		ati := mirrors[i].annoType()
		atj := mirrors[j].annoType()
		if ati.packagePath == atj.packagePath {
			return ati.name < atj.name
		} else {
			return ati.packagePath < atj.packagePath
		}
	})
	return mirrors, nil
}

func (c *Context) convertAnnotation(file *ast.File, selfType types.Type, a parser.Annotation, adjuster posAdjuster, mode parseMode) (AnnotationMirror, error) {
	var mirror AnnotationMirror
	annoPkg, anno, err := c.resolveSymbol(file, a.Type, adjuster)
	if err != nil {
		return mirror, err
	}
	if _, ok := anno.(*types.TypeName); !ok {
		pos := adjuster.adjustPosition(a.Type.Pos)
		return mirror, posError(pos, fmt.Errorf("%v is not a type", a.Type))
	}

	if mode == modeTypeMetadata {
		at := annoType{packagePath: annoPkg.Pkg.Path(), name: a.Type.Name}
		if at != annoTypeForAnnotation {
			// no metadata in this one
			return mirror, nil
		}
	} else if mode == modeFieldMetadata {
		at := annoType{packagePath: annoPkg.Pkg.Path(), name: a.Type.Name}
		if at != annoTypeForRequired && at != annoTypeForDefault {
			// no metadata in this one
			return mirror, nil
		}
	}

	meta, err := c.getMetadata(anno.(*types.TypeName), annoPkg, a.Type, nil)
	if err != nil {
		return mirror, err
	}
	if meta == nil {
		// we've already queried metadata for the type; it's not an annotation
		pos := adjuster.adjustPosition(a.Type.Pos)
		return mirror, posError(pos, fmt.Errorf("%v is not an annotation type", a.Type))
	}

	p := annoPkg.Pkg
	if n, ok := meta.Representation.(*types.Named); ok {
		p = n.Obj().Pkg()
	}
	var av AnnotationValue
	if a.Value != nil {
		av, err = c.convertExpression(file, a.Value, selfType, meta.Representation, p, adjuster)
	} else {
		var tv typeAndVal
		tv.pos = adjuster.adjustPosition(a.Type.Pos)
		_, u := GetUnderlyingType(meta.Representation)
		switch u.(type) {
		case *types.Basic:
			tv.v = true
		case *types.Struct:
			tv.v = ([]parser.Element)(nil)
		default:
			return mirror, posError(tv.pos, fmt.Errorf("annotation %v requires a value since its type is not bool or struct", a.Type))
		}
		av, err = c.convertValue(file, tv, selfType, meta.Representation, p, adjuster)
	}
	if err != nil {
		return mirror, err
	}
	mirror.Value = av
	mirror.Metadata = meta

	return mirror, nil
}

func (c *Context) resolveSymbol(file *ast.File, id parser.Identifier, adjuster posAdjuster) (*loader.PackageInfo, types.Object, error) {
	var pkg *loader.PackageInfo
	var obj types.Object
	if id.PackageAlias == "" {
		obj = c.Package.Pkg.Scope().Lookup(id.Name)
		if obj == nil {
			// try again below by searching in "." imports
			id.PackageAlias = "."
		} else {
			pkg = c.Package
		}
	}
	if obj == nil {
		found := false
		for _, imp := range file.Imports {
			if imp.Name != nil && imp.Name.Name == id.PackageAlias {
				for _, impPkg := range c.Package.Pkg.Imports() {
					if impPkg.Path() == imp.Path.Value {
						found = true
						obj = impPkg.Scope().Lookup(id.Name)
						pkg = c.Program.AllPackages[impPkg]
						break
					}
				}
			}
			if found {
				break
			}
		}
		// see if we can find it in "_" imports, using the package's
		// name as its qualifier
		if obj == nil {
			pos := adjuster.adjustPosition(id.Pos)
			for _, imp := range file.Imports {
				if imp.Name != nil && imp.Name.Name == "_" {
					for _, impPkg := range c.Package.Pkg.Imports() {
						if impPkg.Path() == imp.Path.Value && impPkg.Name() == id.PackageAlias {
							if o := impPkg.Scope().Lookup(id.Name); o != nil {
								if obj != nil {
									return nil, nil, posError(pos, fmt.Errorf("package name %s is ambiguous; could be %q or %q", id.PackageAlias, obj.Pkg().Path(), o.Pkg().Path()))
								}
								obj = o
								pkg = c.Program.AllPackages[impPkg]
							}
							break
						}
					}
				}
			}
			if obj == nil || !obj.Exported() {
				return nil, nil, posError(pos, fmt.Errorf("symbol %v does not exist", id))
			}
		}
	}

	return pkg, obj, nil
}

func (c *Context) getMetadata(anno *types.TypeName, annoPkg *loader.PackageInfo, a parser.Identifier, seen []types.Object) (meta *AnnotationMetadata, err error) {
	var ok bool
	meta, ok = c.metadata[anno]
	if ok {
		return meta, nil
	}

	// find the file that contains the indicated annotation type
	var annoSpec *ast.TypeSpec
	var annoFile *ast.File
	var annoDoc *ast.CommentGroup
	for _, f := range annoPkg.Files {
		astObj := f.Scope.Lookup(a.Name)
		if astObj != nil {
			if astObj.Kind != ast.Typ {
				return nil, fmt.Errorf("%v is not a type so cannot be used as an annotation", a)
			}
			for _, d := range f.Decls {
				if decl, ok := d.(*ast.GenDecl); ok && decl.Tok == token.TYPE {
					for _, s := range decl.Specs {
						spec := s.(*ast.TypeSpec)
						if spec.Name.Name == a.Name {
							annoSpec = spec
							annoFile = f
							if spec.Doc == nil {
								annoDoc = decl.Doc
							} else {
								annoDoc = spec.Doc
							}
							break
						}
					}
				}
				if annoSpec != nil {
					break
				}
			}
			break
		}
		if annoSpec != nil {
			break
		}
	}
	ctx := c
	if annoPkg != c.Package {
		ctx := c.allContexts[annoPkg.Pkg]
		if ctx == nil {
			ctx = newContext(c.outputDir, annoPkg, c.Program, c.allContexts, c.includeTest)
			// share memoized metadata and pool of all contexts
			ctx.metadata = c.metadata
			ctx.fieldMetadata = c.fieldMetadata
		}
	}
	at := annoType{packagePath: annoPkg.Pkg.Path(), name: a.Name}
	if at == annoTypeForAnnotation {
		// We have a boot-strapping cycle trying to get metadata for
		// @annotation.Annotation. So we must seed the metadata with a
		// provisional entry so the next step doesn't cause infinite
		// recursion.
		ctx.metadata[anno] = &AnnotationMetadata{Type: anno, Representation: anno.Type()}
	}
	annoObj := ctx.Package.ObjectOf(annoSpec.Name)
	mirrors, err := ctx.parseAnnotations(annoFile, annoObj.Type(), annoDoc, modeTypeMetadata)
	if err != nil {
		return nil, err
	}
	if len(mirrors) == 0 {
		// the type does not have @annogo.Annotation, so is not an annotation type
		return nil, nil
	}
	return ctx.putMetadata(anno, mirrors[0])
}

func (c *Context) putMetadata(anno *types.TypeName, am AnnotationMirror) (*AnnotationMetadata, error) {
	meta := &AnnotationMetadata{Type: anno}
	for _, fieldEntry := range am.Value.AsStruct() {
		name := fieldEntry.Field.Name()
		val := fieldEntry.Value
		switch name {
		case "FactoryFunc":
			if val.Kind != KindNil {
				meta.FactoryFunc = val.AsFunc()
			}
		case "RuntimeVisible":
			meta.RuntimeVisible = val.AsBool()
		case "AllowedElements":
			if val.Kind != KindNil {
				sl := val.AsSlice()
				meta.AllowedElements = make([]annogo.ElementType, len(sl))
				for i, v := range sl {
					et := annogo.ElementType(v.AsInt())
					meta.AllowedElements[i] = et
				}
			}
		case "AllowRepeated":
			meta.AllowRepeated = val.AsBool()
		default:
			panic(fmt.Sprintf("Unknown field of @Annotation: %s", name))
		}
	}
	if err := c.resolveRepresentation(meta, []types.Object{anno}); err != nil {
		return nil, err
	}
	c.metadata[anno] = meta
	return meta, nil
}

func (c *Context) resolveRepresentation(meta *AnnotationMetadata, seen []types.Object) error {
	if meta.FactoryFunc != nil {
		// Factory functions take one argument and have one result. We need to
		// recursively query the input argument and its associated factory
		// function until we come to a type that has no alternate
		// representation.
		sig := meta.FactoryFunc.Type().(*types.Signature)
		input := sig.Params().At(0).Type()
		if nt, ok := input.(*types.Named); ok {
			anno := nt.Obj()
			if seenType(anno, seen) {
				var msg bytes.Buffer
				fmt.Fprintf(&msg, "%s.%s", anno.Pkg().Name(), anno.Name())
				for i := len(seen) - 1; i >= 0; i-- {
					a := seen[i]
					fmt.Fprintf(&msg, " -> %s.%s", a.Pkg().Name(), a.Name())
				}
				return fmt.Errorf("cycle in factory functions: %s", msg.String())
			}
			seen = append(seen, anno)
			annoPkg := c.Program.AllPackages[anno.Pkg()]
			aident := parser.Identifier{PackageAlias: anno.Pkg().Name(), Name: anno.Name()}
			m, err := c.getMetadata(anno, annoPkg, aident, seen)
			if err != nil {
				return err
			}
			if m != nil {
				meta.Representation = m.Representation
			}
		}
	}
	if meta.Representation == nil {
		meta.Representation = meta.Type.Type()
	}
	return nil
}

func (c *Context) getFieldMetadata(strct *types.Named, field *types.Var) (meta []AnnotationMirror, err error) {
	var ok bool
	meta, ok = c.fieldMetadata[field]
	if ok {
		return meta, nil
	}

	// find the file that contains the indicated field
	var fieldId *ast.Ident
	var fieldFile *ast.File
	var fieldDoc *ast.CommentGroup
	fieldPkg := c.Program.AllPackages[field.Pkg()]
	for _, f := range fieldPkg.Files {
		astObj := f.Scope.Lookup(strct.Obj().Name())
		if astObj != nil {
			typeSpec := astObj.Decl.(*ast.TypeSpec)
			flds := typeSpec.Type.(*ast.StructType).Fields
			if flds != nil {
				for _, fld := range flds.List {
					for _, nm := range fld.Names {
						if nm.Name == field.Name() {
							fieldId = nm
							fieldFile = f
							fieldDoc = fld.Doc
							break
						}
					}
					if fieldId != nil {
						break
					}
				}
			}
			break
		}
		if fieldId != nil {
			break
		}
	}
	ctx := c
	if fieldPkg != c.Package {
		ctx := c.allContexts[fieldPkg.Pkg]
		if ctx == nil {
			ctx = newContext(c.outputDir, fieldPkg, c.Program, c.allContexts, c.includeTest)
			// share memoized metadata and pool of all contexts
			ctx.metadata = c.metadata
			ctx.fieldMetadata = c.fieldMetadata
		}
	}
	at := annoType{packagePath: fieldPkg.Pkg.Path(), name: strct.Obj().Name()}
	if at == annoTypeForRequired || at == annoTypeForDefault || at == annoTypeForAnnotation {
		// We have a boot-strapping cycle trying to get field metadata for
		// fields of @annotation.Annotation, @annotation.Required, or
		// @annotation.Default. So we must seed the metadata with a provisional
		// entry so the next step doesn't cause infinite recursion.
		ctx.fieldMetadata[field] = nil
	}
	annoObj := ctx.Package.ObjectOf(fieldId)
	mirrors, err := ctx.parseAnnotations(fieldFile, annoObj.Type(), fieldDoc, modeFieldMetadata)
	if err != nil {
		return nil, err
	}
	ctx.fieldMetadata[field] = mirrors
	return mirrors, nil
}

func seenType(t types.Object, seen []types.Object) bool {
	for _, o := range seen {
		if t == o {
			return true
		}
	}
	return false
}

func (c *Context) extractAnnotations(doc *ast.CommentGroup) (*bytes.Buffer, posAdjuster) {
	if doc == nil {
		return nil, nil
	}
	var buf bytes.Buffer
	var adjuster posAdjuster
	found := false
	prevSingleLine := false
	var pos token.Position
	for _, l := range doc.List {
		txt := l.Text
		singleLine := false
		if strings.HasPrefix(txt, "/*") {
			txt = txt[2:]
			if strings.HasSuffix(txt, "*/") {
				txt = txt[:len(txt)-2]
			}
		} else if strings.HasPrefix(txt, "//") {
			singleLine = true
			txt = txt[2:]
		}

		if singleLine != prevSingleLine {
			found = false
			buf.Reset()
			prevSingleLine = singleLine
			adjuster = nil
		}

		pos = c.Program.Fset.Position(l.Slash)
		// skip past opening "//" or "/*"
		pos.Offset += 2
		pos.Column += 2

		for _, line := range strings.Split(txt, "\n") {
			trimmed := strings.TrimSpace(line)
			if !found && trimmed != "" && trimmed[0] == '@' {
				found = true
			}
			if found {
				adjuster = append(adjuster, posAdj{outOffset: buf.Len(), inPos: pos})
				buf.WriteString(line)
				buf.WriteByte('\n')
			}
			pos.Offset += len(line) + 1
			pos.Line++
			pos.Column = 1
		}

		// set this so we can record end of input as the last entry in adjuster
		pos = c.Program.Fset.Position(l.End())
	}
	if !found {
		return nil, nil
	}
	adjuster = append(adjuster, posAdj{outOffset: buf.Len(), inPos: pos})
	return &buf, adjuster
}

type posAdj struct {
	outOffset int
	inPos     token.Position
}

type posAdjuster []posAdj

func (a posAdjuster) adjustPosition(pos scanner.Position) token.Position {
	var t token.Position
	t.Filename = pos.Filename
	el := a[pos.Line-1]
	var tok token.Position
	tok.Filename = el.inPos.Filename
	tok.Line = el.inPos.Line
	tok.Column = el.inPos.Column + pos.Column - 1
	tok.Offset = el.inPos.Offset + (pos.Offset - el.outOffset)
	return tok
}

func hasAnnotations(doc *ast.CommentGroup) (token.Pos, bool) {
	if doc == nil {
		return 0, false
	}
	for _, l := range doc.List {
		if strings.TrimSpace(l.Text)[0] == '@' {
			return l.Slash, true
		}
	}
	return 0, false
}

func checkAnnotations(annos []AnnotationMirror, ets []annogo.ElementType, isConcrete bool) error {
	contains := func(et annogo.ElementType) bool {
		for _, e := range ets {
			if e == et {
				return true
			}
		}
		return false
	}
	var prevType annoType
	for _, a := range annos {
		at := a.annoType()
		if a.Metadata.AllowedElements != nil {
			found := false
			for _, aet := range a.Metadata.AllowedElements {
				if contains(aet) {
					found = true
					break
				}
			}
			if !found {
				return fmt.Errorf("annotation type %s.%s cannot be used on %s", at.packagePath, at.name, strings.ToLower(ets[len(ets)-1].String()))
			}
		}
		if at == prevType && !a.Metadata.AllowRepeated {
			return fmt.Errorf("annotation type %s.%s appears more than once but cannot be repeated", at.packagePath, at.name)
		}
	}
	return nil
}

func (c *Context) convertExpression(file *ast.File, exp parser.ExpressionNode, selfType, targetType types.Type, p *types.Package, adjuster posAdjuster) (av AnnotationValue, err error) {
	var tv typeAndVal
	tv.pos = adjuster.adjustPosition(exp.Pos())
	tv.t, tv.v, err = c.getExpressionValue(file, exp, adjuster)
	if err != nil {
		return AnnotationValue{}, err
	}
	return c.convertValue(file, tv, selfType, targetType, p, adjuster)
}

type typeAndVal struct {
	t   types.Type
	v   interface{}
	pos token.Position
}

func (c *Context) convertValue(file *ast.File, tv typeAndVal, selfType, targetType types.Type, p *types.Package, adjuster posAdjuster) (av AnnotationValue, err error) {
	av, err = c.tryConvertValue(file, tv, selfType, targetType, p, adjuster)
	if wte, ok := err.(wrongTypeError); ok {
		ntyp, typ := GetUnderlyingType(targetType)
		var elemType types.Type
		var fld *types.Var
		switch t := typ.(type) {
		case *types.Array:
			if t.Len() != 1 {
				return nilValue, posError(tv.pos, fmt.Errorf("array must have length %d but given literal value has 1 element", t.Len()))
			}
			elemType = t.Elem()
		case *types.Slice:
			elemType = t.Elem()
		case *types.Struct:
			if t.NumFields() == 1 && t.Field(0).Name() == "Value" {
				fld = t.Field(0)
				elemType = fld.Type()
			}
		}
		if elemType == nil {
			// unwrap the marker error type
			return nilValue, wte.err
		}
		// try to convert the value to an element of the array/slice/struct
		av, err = c.tryConvertValue(file, tv, selfType, elemType, p, adjuster)
		if wte, ok := err.(wrongTypeError); ok {
			// unwrap the marker error type
			return nilValue, wte.err
		} else if err != nil {
			return nilValue, err
		}
		// that worked! so we need to wrap the value as a single element slice
		if fld != nil {
			return newValue(ntyp, KindStruct, []AnnotationStructEntry{{Field: fld, Pos: tv.pos, Value: av}}, tv.pos), nil
		} else {
			return newValue(ntyp, KindSlice, []AnnotationValue{av}, tv.pos), nil
		}
	}
	return av, err
}

func (c *Context) tryConvertValue(file *ast.File, tv typeAndVal, selfType, targetType types.Type, p *types.Package, adjuster posAdjuster) (av AnnotationValue, err error) {
	ntyp, typ := GetUnderlyingType(targetType)

	var sourceType string

	if tv.t != nil {
		sourceType = tv.t.String()
		if assignable(tv.t, ntyp, selfType, false) {
			if n, ok := tv.t.(*types.Named); ok {
				p = n.Obj().Pkg()
			}
			ttv := typeAndVal{t: nil, v: tv.v, pos: tv.pos}
			av, err := c.convertValue(file, ttv, selfType, targetType, p, adjuster)
			if err == nil {
				av.Type = typ
			}
			return av, err
		}
	} else {
		switch v := tv.v.(type) {
		case int64:
			sourceType = "int"
			if av, ok := convertInt(v, typ, tv.pos); ok {
				return av, nil
			}

		case uint64:
			sourceType = "uint"
			if av, ok := convertUint(v, typ, tv.pos); ok {
				return av, nil
			}

		case float64:
			sourceType = "float"
			if av, ok := convertFloat(v, typ, tv.pos); ok {
				return av, nil
			}

		case complex128:
			sourceType = "complex"
			if av, ok := convertComplex(v, typ, tv.pos); ok {
				return av, nil
			}

		case bool:
			sourceType = "bool"
			if types.AssignableTo(typeBool, typ) {
				return annotationValue(typeBool, typ, v, KindBool, tv.pos), nil
			}

		case string:
			sourceType = "string"
			if types.AssignableTo(typeString, typ) {
				return annotationValue(typeString, typ, v, KindString, tv.pos), nil
			}

		case []parser.Element:
			sourceType = "composite"
			if len(v) > 0 {
				if v[0].HasKey {
					sourceType = "composite"
				} else {
					sourceType = "composite"
				}
			}

			switch t := typ.(type) {
			case *types.Struct:
				return c.convertStructValue(file, v, tv.pos, selfType, t, ntyp, p, adjuster)
			case *types.Map:
				return c.convertMapValue(file, v, tv.pos, selfType, t, ntyp, adjuster)
			case *types.Array:
				if t.Len() != int64(len(v)) {
					return nilValue, posError(tv.pos, fmt.Errorf("array must have length %d but given literal value has %d elements", t.Len(), len(v)))
				}
				return c.convertSliceValue(file, v, tv.pos, selfType, t.Elem(), ntyp, adjuster)
			case *types.Slice:
				return c.convertSliceValue(file, v, tv.pos, selfType, t.Elem(), ntyp, adjuster)
			case *types.Interface:
				if t.NumMethods() == 0 {
					// Type is empty interface, so we can assign any value to it. So
					// assume map or struct if the value has keys. Otherwise, assume
					// slice. If it has keys and they are all unqualified names then
					// assume it is a struct; otherwise it is a map.
					return c.convertCompositeValue(file, v, tv.pos, selfType, adjuster)
				}
			}

		case *types.Func:
			sourceType = v.Type().String()
			if t := canAssignIndirect(v.Type(), targetType, selfType); t != nil {
				return annotationValue(v.Type(), t, v, KindFunc, tv.pos), nil
			}

		case nil:
			sourceType = "nil"
			switch targetType.(type) {
			case *types.Pointer, *types.Slice, *types.Signature, *types.Interface, *types.Map, *types.Chan:
				return newValue(targetType, KindNil, nil, tv.pos), nil
			}

		default:
			panic(fmt.Sprintf("%s:%d:%d: unsupported kind of value: %T", tv.pos.Filename, tv.pos.Line, tv.pos.Column, v))
		}
	}

	rewritten := rewriteSelfType(targetType, selfType)
	if types.Identical(targetType, rewritten) {
		return nilValue, wrongTypeError{err: posError(tv.pos, fmt.Errorf("annotation value of type %s cannot be assigned to %v", sourceType, targetType))}
	} else {
		return nilValue, wrongTypeError{err: posError(tv.pos, fmt.Errorf("annotation value of type %s cannot be assigned to %v (%v)", sourceType, targetType, rewritten))}
	}
}

type wrongTypeError struct {
	err error
}

func (e wrongTypeError) Error() string {
	return e.err.Error()
}

var (
	intTypes = map[types.Type]reflect.Type{
		typeInt64: reflect.TypeOf(int64(0)),
		typeInt32: reflect.TypeOf(int32(0)),
		typeInt:   reflect.TypeOf(int(0)),
		typeInt16: reflect.TypeOf(int16(0)),
		typeInt8:  reflect.TypeOf(int8(0)),
	}
	uintTypes = map[types.Type]reflect.Type{
		typeUint64:  reflect.TypeOf(uint64(0)),
		typeUint32:  reflect.TypeOf(uint32(0)),
		typeUint:    reflect.TypeOf(uint(0)),
		typeUintptr: reflect.TypeOf(uintptr(0)),
		typeUint16:  reflect.TypeOf(uint16(0)),
		typeUint8:   reflect.TypeOf(uint8(0)),
	}
	floatTypes = map[types.Type]reflect.Type{
		typeFloat64: reflect.TypeOf(float64(0)),
		typeFloat32: reflect.TypeOf(float32(0)),
	}
	complexTypes = map[types.Type]reflect.Type{
		typeComplex128: reflect.TypeOf(complex128(0)),
		typeComplex64:  reflect.TypeOf(complex64(0)),
	}
)

func convertInt(i int64, t types.Type, pos token.Position) (AnnotationValue, bool) {
	for k, rt := range intTypes {
		if types.AssignableTo(k, t) {
			// round trip to see if the value overflows
			v := reflect.ValueOf(i).Convert(rt).Int()
			if v == i {
				return annotationValue(k, t, i, KindInt, pos), true
			}
		}
	}
	if i > 0 {
		// because int and uint are mutually auto-convertible, we can't
		// just call convertUint, because it could call convertInt and
		// result in infinite cycle
		u := uint64(i)
		for k, rt := range uintTypes {
			if types.AssignableTo(k, t) {
				// round trip to see if the value overflows
				v := reflect.ValueOf(u).Convert(rt).Uint()
				if v == u {
					return annotationValue(k, t, u, KindUint, pos), true
				}
			}
		}
	}
	// try to promote to float
	return convertFloat(float64(i), t, pos)
}

func convertUint(u uint64, t types.Type, pos token.Position) (AnnotationValue, bool) {
	for k, rt := range uintTypes {
		if types.AssignableTo(k, t) {
			// round trip to see if the value overflows
			v := reflect.ValueOf(u).Convert(rt).Uint()
			if v == u {
				return annotationValue(k, t, u, KindUint, pos), true
			}
		}
	}
	if u <= math.MaxInt64 {
		// because int and uint are mutually auto-convertible, we can't
		// just call convertInt, because it could call convertUint and
		// result in infinite cycle
		i := int64(u)
		for k, rt := range intTypes {
			if types.AssignableTo(k, t) {
				// round trip to see if the value overflows
				v := reflect.ValueOf(i).Convert(rt).Int()
				if v == i {
					return annotationValue(k, t, i, KindInt, pos), true
				}
			}
		}
	}
	// try to promote to float
	return convertFloat(float64(u), t, pos)
}

func convertFloat(f float64, t types.Type, pos token.Position) (AnnotationValue, bool) {
	for k, rt := range floatTypes {
		if types.AssignableTo(k, t) {
			// round trip to see if the value overflows
			v := reflect.ValueOf(f).Convert(rt).Float()
			if v == f {
				return annotationValue(k, t, f, KindFloat, pos), true
			}
		}
	}
	// try to promote to complex
	return convertComplex(complex(f, 0), t, pos)
}

func convertComplex(c complex128, t types.Type, pos token.Position) (AnnotationValue, bool) {
	for k, rt := range complexTypes {
		if types.AssignableTo(k, t) {
			// round trip to see if the value overflows
			v := reflect.ValueOf(c).Convert(rt).Complex()
			if v == c {
				return annotationValue(k, t, c, KindComplex, pos), true
			}
		}
	}
	return nilValue, false
}

func annotationValue(sourceType, targetType types.Type, value interface{}, kind ValueKind, pos token.Position) AnnotationValue {
	// prefer concrete types, so if target type is interface and source type is not, use the source type
	if _, ok := targetType.Underlying().(*types.Interface); ok {
		if _, ok := sourceType.Underlying().(*types.Interface); !ok {
			if b, ok := sourceType.Underlying().(*types.Basic); ok {
				switch b.Kind() {
				// upgrade from untyped kinds to typed kinds
				case types.UntypedBool:
					sourceType = typeBool
				case types.UntypedString:
					sourceType = typeString
				case types.UntypedComplex:
					sourceType = typeComplex128
				case types.UntypedFloat:
					sourceType = typeFloat64
				case types.UntypedRune:
					sourceType = typeInt32
				case types.UntypedNil:
					// instead of using a nil type, just use the target interface type
					sourceType = targetType
					// we have to leave UntypedInt alone since we don't know at this
					// point whether it needs to be int64 (e.g. if it's negative) or
					// uint64 (e.g. too big to fit in int64).
				}
			}
			return newValue(sourceType, kind, value, pos)
		}
	}
	return newValue(targetType, kind, value, pos)
}

func assignable(from, to, selfType types.Type, insideOfFunction bool) bool {
	if IsAnyType(to) {
		return true
	}
	if selfType != nil && IsSelfType(to) {
		to = selfType
	}
	if types.AssignableTo(from, to) {
		return true
	} else if !containsSpecialType(to, selfType != nil) {
		return false
	}
	return assignableToSpecial(from, to, selfType, insideOfFunction)
}

func assignableToSpecial(from, to, selfType types.Type, insideOfFunction bool) bool {
	// recursive calls may swap out selfType for nil since SelfType is only
	// supported in scalar and function types
	switch from := from.(type) {
	case *types.Named:
		return assignableToSpecial(from.Underlying(), to, selfType, insideOfFunction)
	case *types.Basic:
	case *types.Pointer:
		if to, ok := to.(*types.Pointer); ok {
			return assignable(from.Elem(), to.Elem(), selfType, insideOfFunction)
		}
	case *types.Array:
		if to, ok := to.(*types.Array); ok {
			if from.Len() != to.Len() {
				return false
			}
			if !insideOfFunction {
				selfType = nil
			}
			return assignable(from.Elem(), to.Elem(), selfType, insideOfFunction)
		}
	case *types.Slice:
		if to, ok := to.(*types.Slice); ok {
			if !insideOfFunction {
				selfType = nil
			}
			return assignable(from.Elem(), to.Elem(), selfType, insideOfFunction)
		}
	case *types.Map:
		if to, ok := to.(*types.Map); ok {
			if !insideOfFunction {
				selfType = nil
			}
			return assignable(from.Key(), to.Key(), selfType, insideOfFunction) &&
				assignable(from.Elem(), to.Elem(), selfType, insideOfFunction)
		}
	case *types.Chan:
		if to, ok := to.(*types.Chan); ok {
			if !insideOfFunction {
				selfType = nil
			}
			if from.Dir() != to.Dir() {
				return false
			}
			return assignable(from.Elem(), to.Elem(), selfType, insideOfFunction)
		}
	case *types.Struct:
	case *types.Interface:
	case *types.Tuple:
	case *types.Signature:
		if to, ok := to.(*types.Signature); ok {
			if from.Params().Len() != to.Params().Len() || from.Results().Len() != to.Results().Len() {
				return false
			}
			for i := 0; i < from.Params().Len(); i++ {
				if !assignable(from.Params().At(i).Type(), to.Params().At(i).Type(), selfType, true) {
					return false
				}
			}
			for i := 0; i < from.Results().Len(); i++ {
				if !assignable(from.Results().At(i).Type(), to.Results().At(i).Type(), selfType, true) {
					return false
				}
			}
			return true
		}
	}
	return false
}

func IsAnyType(t types.Type) bool {
	return isNamedType(t, anyTypePkg, anyTypeName)
}

func IsSelfType(t types.Type) bool {
	return isNamedType(t, selfTypePkg, selfTypeName)
}

func isNamedType(t types.Type, pkgPath, name string) bool {
	if nt, ok := t.(*types.Named); ok {
		return nt.Obj().Pkg().Path() == pkgPath && nt.Obj().Name() == name
	}
	return false
}

func containsSpecialType(t types.Type, includeSelfType bool) bool {
	switch t := t.(type) {
	case *types.Named:
		if t.Obj().Pkg().Path() == anyTypePkg && t.Obj().Name() == anyTypeName {
			return true
		}
		if includeSelfType && t.Obj().Pkg().Path() == selfTypePkg && t.Obj().Name() == selfTypeName {
			return true
		}
	case *types.Pointer:
		return containsSpecialType(t.Elem(), includeSelfType)
	case *types.Array:
		return containsSpecialType(t.Elem(), includeSelfType)
	case *types.Slice:
		return containsSpecialType(t.Elem(), includeSelfType)
	case *types.Map:
		return containsSpecialType(t.Key(), includeSelfType) ||
			containsSpecialType(t.Elem(), includeSelfType)
	case *types.Chan:
		return containsSpecialType(t.Elem(), includeSelfType)
	case *types.Signature:
		for i := 0; i < t.Params().Len(); i++ {
			if containsSpecialType(t.Params().At(i).Type(), includeSelfType) {
				return true
			}
		}
		for i := 0; i < t.Results().Len(); i++ {
			if containsSpecialType(t.Results().At(i).Type(), includeSelfType) {
				return true
			}
		}
	}
	return false
}

func rewriteSelfType(t types.Type, self types.Type) types.Type {
	if self == nil {
		return t
	}
	switch t := t.(type) {
	case *types.Named:
		if t.Obj().Pkg().Path() == selfTypePkg && t.Obj().Name() == selfTypeName {
			return self
		}
		return t

	case *types.Pointer:
		e := t.Elem()
		re := rewriteSelfType(e, self)
		if re == e {
			return t
		}
		return types.NewPointer(re)

	case *types.Array:
		e := t.Elem()
		re := rewriteSelfType(e, self)
		if re == e {
			return t
		}
		return types.NewArray(re, t.Len())

	case *types.Slice:
		e := t.Elem()
		re := rewriteSelfType(e, self)
		if re == e {
			return t
		}
		return types.NewSlice(re)

	case *types.Map:
		k := t.Key()
		rk := rewriteSelfType(k, self)
		e := t.Elem()
		re := rewriteSelfType(e, self)
		if rk == k && re == e {
			return t
		}
		return types.NewMap(rk, re)

	case *types.Chan:
		e := t.Elem()
		re := rewriteSelfType(e, self)
		if re == e {
			return t
		}
		return types.NewChan(t.Dir(), re)

	case *types.Tuple:
		var rc []*types.Var
		for i := 0; i < t.Len(); i++ {
			v := t.At(i)
			e := v.Type()
			re := rewriteSelfType(e, self)
			if re != e {
				if rc == nil {
					rc = make([]*types.Var, t.Len())
					for j := 0; j < i; j++ {
						rc[j] = t.At(j)
					}
				}
				rv := types.NewParam(v.Pos(), v.Pkg(), v.Name(), re)
				rc[i] = rv
			} else if rc != nil {
				rc[i] = v
			}
		}
		if rc == nil {
			return t
		}
		return types.NewTuple(rc...)

	case *types.Signature:
		p := t.Params()
		rp := rewriteSelfType(p, self).(*types.Tuple)
		r := t.Results()
		rr := rewriteSelfType(r, self).(*types.Tuple)
		if rp == p && rr == r {
			return t
		}
		return types.NewSignature(t.Recv(), rp, rr, t.Variadic())

	default:
		return t
	}
}

func (c *Context) convertStructValue(file *ast.File, v []parser.Element, pos token.Position, selfType types.Type, structType *types.Struct, nt types.Type, p *types.Package, adjuster posAdjuster) (AnnotationValue, error) {
	isLocal := p == c.Package.Pkg
	if nt, ok := nt.(*types.Named); ok {
		// NB: This is a bit subtle, but this call will compute annotations for
		// the type if they have not already been computed. We don't actually
		// need the metadata, but the act of retrieving it is what ensures that
		// the type's annotations (and its fields, if it is a struct) have been
		// processed. That way, we can look for field annotations below, as part
		// of validating the assigned value.
		var err error
		_, err = c.GetMetadataForTypeName(nt.Obj())
		if err != nil {
			return nilValue, err
		}
	}

	strct := make([]AnnotationStructEntry, len(v))
	if len(v) > 0 && !v[0].HasKey {
		if len(v) != structType.NumFields() {
			return nilValue, posError(pos, fmt.Errorf("too few values for struct type %s; expecting %d, got %d", nt.String(), structType.NumFields(), len(v)))
		}
		for i, fldVal := range v {
			fld := structType.Field(i)
			if !fld.Exported() && !isLocal {
				pos := adjuster.adjustPosition(fldVal.Pos())
				return nilValue, posError(pos, fmt.Errorf("cannot set non-exported field %s of type %s", fld.Name(), nt.String()))
			}
			av, err := c.convertExpression(file, fldVal.Value, selfType, fld.Type(), fld.Pkg(), adjuster)
			if err != nil {
				return nilValue, err
			}
			pos := adjuster.adjustPosition(fldVal.Pos())
			strct[i] = AnnotationStructEntry{Field: fld, Pos: pos, Value: av}
		}
	} else {
		fields := map[string]*types.Var{}
		for i := 0; i < structType.NumFields(); i++ {
			fields[structType.Field(i).Name()] = structType.Field(i)
		}

		fieldNames := map[string]struct{}{}
		for i, fldVal := range v {
			var fldName string
			if ref, ok := fldVal.Key.(parser.RefNode); ok {
				id := ref.Ident
				if id.PackageAlias == "" {
					fldName = id.Name
				}
			}
			if fldName == "" {
				return nilValue, posError(pos, fmt.Errorf("cannot assign map value to type %s", nt.String()))
			}
			if _, ok := fieldNames[fldName]; ok {
				pos := adjuster.adjustPosition(fldVal.Key.Pos())
				return nilValue, posError(pos, fmt.Errorf("struct value has duplicate entries: field %q", fldName))
			}
			fld := fields[fldName]
			if fld == nil {
				pos := adjuster.adjustPosition(fldVal.Pos())
				return nilValue, posError(pos, fmt.Errorf("struct type %s has no field named %s", nt.String(), fldName))
			}
			if !fld.Exported() && !isLocal {
				pos := adjuster.adjustPosition(fldVal.Pos())
				return nilValue, posError(pos, fmt.Errorf("cannot set non-exported field %s of type %s", fld.Name(), nt.String()))
			}
			delete(fields, fldName)
			av, err := c.convertExpression(file, fldVal.Value, selfType, fld.Type(), fld.Pkg(), adjuster)
			if err != nil {
				return nilValue, err
			}
			fieldNames[fldName] = struct{}{}
			pos := adjuster.adjustPosition(fldVal.Pos())
			strct[i] = AnnotationStructEntry{Field: fld, Pos: pos, Value: av}
		}

		for _, fld := range fields {
			var fieldAnnos []AnnotationMirror
			if ae := c.AllElements[fld]; ae != nil {
				fieldAnnos = ae.Annotations
			} else if named, ok := nt.(*types.Named); ok {
				// named type has either not been processed yet or is in a
				// package that is not being processed, so we need to get its
				// metadata out-of-band
				var err error
				if fieldAnnos, err = c.getFieldMetadata(named, fld); err != nil {
					return nilValue, err
				}
			}
			a := findAnnotations(fieldAnnos, requiredPkg, requiredName)
			if len(a) > 0 && a[0].Value.AsBool() {
				return nilValue, posError(pos, fmt.Errorf("field %s is not specified but is required", fld.Name()))
			}
			a = findAnnotations(fieldAnnos, defaultPkg, defaultName)
			if len(a) > 0 {
				def := a[0].Value.Value.([]AnnotationStructEntry)[0].Value
				strct = append(strct, AnnotationStructEntry{Field: fld, Pos: a[0].Pos, Value: def})
			}
		}
	}
	return newValue(nt, KindStruct, strct, pos), nil
}

func (c *Context) convertMapValue(file *ast.File, v []parser.Element, pos token.Position, selfType types.Type, mapType *types.Map, nt types.Type, adjuster posAdjuster) (AnnotationValue, error) {
	mp := make([]AnnotationMapEntry, 0, len(v))
	keys := map[interface{}]struct{}{}
	for i, e := range v {
		if !e.HasKey {
			return nilValue, fmt.Errorf("map values must have keys")
		}
		avk, err := c.convertExpression(file, e.Key, selfType, mapType.Key(), c.Package.Pkg, adjuster)
		if err != nil {
			return nilValue, err
		}
		k := c.GetValue(avk, true)
		if _, ok := keys[k]; ok {
			return nilValue, fmt.Errorf("map value has duplicate entries: key = %v", k)
		}
		avv, err := c.convertExpression(file, e.Value, selfType, mapType.Elem(), c.Package.Pkg, adjuster)
		if err != nil {
			return nilValue, err
		}
		keys[k] = struct{}{}
		mp[i] = AnnotationMapEntry{Key: avk, Value: avv}
	}

	return newValue(nt, KindMap, mp, pos), nil
}

func (c *Context) convertSliceValue(file *ast.File, v []parser.Element, pos token.Position, selfType types.Type, elemType, nt types.Type, adjuster posAdjuster) (AnnotationValue, error) {
	if len(v) > 0 && v[0].HasKey {
		pos := adjuster.adjustPosition(v[0].Key.Pos())
		return nilValue, posError(pos, fmt.Errorf("slice/array values should not have keys"))
	}

	sl := make([]AnnotationValue, len(v))
	for i, e := range v {
		av, err := c.convertExpression(file, e.Value, selfType, elemType, c.Package.Pkg, adjuster)
		if err != nil {
			return nilValue, err
		}
		sl[i] = av
	}

	return newValue(nt, KindSlice, sl, pos), nil
}

func (c *Context) convertCompositeValue(file *ast.File, v []parser.Element, pos token.Position, selfType types.Type, adjuster posAdjuster) (AnnotationValue, error) {
	isStruct := true
	hasKeys := false
	for _, e := range v {
		if e.HasKey {
			hasKeys = true
			if ref, ok := e.Key.(parser.RefNode); !ok || ref.Ident.PackageAlias != "" {
				isStruct = false
			}
		} else {
			isStruct = false
		}
	}

	if isStruct {
		strct := []AnnotationStructEntry{}
		fieldNames := map[string]struct{}{}
		fields := make([]*types.Var, len(v))
		for i, e := range v {
			av, err := c.convertExpression(file, e.Value, selfType, emptyInterface, c.Package.Pkg, adjuster)
			if err != nil {
				return nilValue, err
			}
			fld := types.NewField(token.NoPos, c.Package.Pkg, e.Key.(parser.RefNode).Ident.Name, av.Type, false)
			fields[i] = fld
			pos := adjuster.adjustPosition(e.Key.Pos())
			if _, ok := fieldNames[fld.Name()]; ok {
				return nilValue, posError(pos, fmt.Errorf("struct value has duplicate entries: field %q", fld.Name()))
			}
			fieldNames[fld.Name()] = struct{}{}
			strct[i] = AnnotationStructEntry{Field: fld, Pos: pos, Value: av}
		}
		strctType := types.NewStruct(fields, nil)
		return newValue(strctType, KindStruct, strct, pos), nil

	} else if hasKeys {
		mp := make([]AnnotationMapEntry, 0, len(v))
		keys := map[interface{}]struct{}{}
		var elType, keyType types.Type
		for i, e := range v {
			avk, err := c.convertExpression(file, e.Key, selfType, emptyInterface, c.Package.Pkg, adjuster)
			if err != nil {
				return nilValue, err
			}
			k := c.GetValue(avk, true)
			if _, ok := keys[k]; ok {
				pos := adjuster.adjustPosition(e.Key.Pos())
				return nilValue, posError(pos, fmt.Errorf("map value has duplicate entries: key = %v", k))
			}
			if keyType == nil {
				keyType = avk.Type
			} else if !types.AssignableTo(avk.Type, keyType) {
				keyType = emptyInterface
			}
			avv, err := c.convertExpression(file, e.Value, selfType, emptyInterface, c.Package.Pkg, adjuster)
			if err != nil {
				return nilValue, err
			}
			if elType == nil {
				elType = avv.Type
			} else if !types.AssignableTo(avv.Type, elType) {
				elType = emptyInterface
			}
			keys[k] = struct{}{}
			mp[i] = AnnotationMapEntry{Key: avk, Value: avv}
		}
		if keyType == nil {
			keyType = emptyInterface
		}
		if elType == nil {
			elType = emptyInterface
		}
		mpType := types.NewMap(keyType, elType)
		return newValue(mpType, KindMap, mp, pos), nil

	} else {
		sl := make([]AnnotationValue, len(v))
		var elType types.Type
		for i, e := range v {
			av, err := c.convertExpression(file, e.Value, selfType, emptyInterface, c.Package.Pkg, adjuster)
			if err != nil {
				return nilValue, err
			}
			if elType == nil {
				elType = av.Type
			} else if !types.AssignableTo(av.Type, elType) {
				elType = emptyInterface
			}
			sl[i] = av
		}
		if elType == nil {
			elType = emptyInterface
		}
		arType := types.NewArray(elType, int64(len(sl)))
		return newValue(arType, KindSlice, sl, pos), nil
	}
}

func (c *Context) GetValue(av AnnotationValue, forceArray bool) interface{} {
	switch av.Kind {
	case KindMap:
		mp := map[interface{}]interface{}{}
		for _, entry := range av.AsMap() {
			k := c.GetValue(entry.Key, true)
			mp[k] = c.GetValue(entry.Value, false)
		}
		return mp
	case KindSlice:
		s := av.AsSlice()
		sl := make([]interface{}, len(s))
		for i, v := range s {
			sl[i] = c.GetValue(v, true)
		}
		if forceArray {
			return asArray(reflect.ValueOf(sl)).Interface()
		}
		return sl
	case KindStruct:
		s := av.AsStruct()
		fields := make([]reflect.StructField, 0, len(s))
		vals := map[string]interface{}{}
		for _, entry := range s {
			v := c.GetValue(entry.Value, forceArray)
			name := entry.Field.Name()
			vals[name] = v
			fld := reflect.StructField{
				Name: name,
				Type: reflect.TypeOf(v),
			}
			if !isExported(name) {
				fld.PkgPath = c.Package.Pkg.Path()
			}
			fields = append(fields, fld)
		}
		strct := reflect.Zero(reflect.StructOf(fields))
		for name, val := range vals {
			strct.FieldByName(name).Set(reflect.ValueOf(val))
		}
		return strct.Interface()

	default:
		return av.Value
	}
}

func (c *Context) findConstantValue(con *types.Const) (interface{}, bool) {
	if b, ok := con.Type().(*types.Basic); ok && b.Kind() == types.UntypedNil {
		return nil, true
	}
	val := con.Val()
	if val != nil {
		switch val.Kind() {
		case constant.Int:
			if constant.Sign(val) < 0 {
				if i, ok := constant.Int64Val(val); ok {
					return i, true
				}
			} else {
				if u, ok := constant.Uint64Val(val); ok {
					return u, true
				}
			}
		case constant.Float:
			if f, ok := constant.Float64Val(val); ok {
				return f, true
			}
		case constant.Complex:
			if r, ok := constant.Float64Val(constant.Real(val)); ok {
				if i, ok := constant.Float64Val(constant.Imag(val)); ok {
					return complex(r, i), true
				}
			}
		case constant.String:
			return constant.StringVal(val), true
		case constant.Bool:
			return constant.BoolVal(val), true
		}
	}
	// invalid or missing value
	return nil, false
}

func isExported(name string) bool {
	r, _ := utf8.DecodeRuneInString(name)
	if r == utf8.RuneError {
		r = rune(name[0])
	}
	return unicode.IsUpper(r)
}

func canAssignIndirect(sourceType, targetType, selfType types.Type) types.Type {
	for {
		if assignable(sourceType, targetType, selfType, false) {
			return targetType
		}
		if p, ok := targetType.Underlying().(*types.Pointer); ok {
			targetType = p.Elem()
		} else {
			return nil
		}
	}
}

func (c *Context) convertType(file *ast.File, t parser.Type, adjuster posAdjuster) (types.Type, error) {
	switch {
	case t.IsMap():
		k, err := c.convertType(file, t.Key(), adjuster)
		if err != nil {
			return nil, err
		}
		v, err := c.convertType(file, t.Elem(), adjuster)
		if err != nil {
			return nil, err
		}
		return types.NewMap(k, v), nil

	case t.IsArray():
		exptyp, v, err := c.getExpressionValue(file, t.Len(), adjuster)
		if err != nil {
			return nil, err
		}
		pos := adjuster.adjustPosition(t.Len().Pos())
		var l int64
		if i, ok := v.(int64); ok {
			if i < 0 {
				return nil, posError(pos, fmt.Errorf("array bound is negative: %v", v))
			}
			l = i
		} else if u, ok := v.(uint64); ok {
			if u > math.MaxInt64 {
				return nil, posError(pos, fmt.Errorf("array bound overflows int64: %v", v))
			}
			l = int64(u)
		} else {
			return nil, posError(pos, fmt.Errorf("array bound must be integer type; got %v", exptyp))
		}

		e, err := c.convertType(file, t.Elem(), adjuster)
		if err != nil {
			return nil, err
		}
		return types.NewArray(e, l), nil

	case t.IsSlice():
		e, err := c.convertType(file, t.Elem(), adjuster)
		if err != nil {
			return nil, err
		}
		return types.NewSlice(e), nil

	case t.IsPointer():
		e, err := c.convertType(file, t.Elem(), adjuster)
		if err != nil {
			return nil, err
		}
		return types.NewPointer(e), nil

	case t.IsEmptyInterface():
		return emptyInterface, nil

	case t.IsEmptyStruct():
		return emptyStruct, nil

	default:
		if t.Name().PackageAlias == "" {
			switch t.Name().Name {
			case "string":
				return typeString, nil
			case "bool":
				return typeBool, nil
			case "byte":
				return typeUint8, nil
			case "rune":
				return typeInt32, nil
			case "int":
				return typeInt, nil
			case "int8":
				return typeInt8, nil
			case "int16":
				return typeInt16, nil
			case "int32":
				return typeInt32, nil
			case "int64":
				return typeInt64, nil
			case "uint":
				return typeUint, nil
			case "uint8":
				return typeUint8, nil
			case "uint16":
				return typeUint16, nil
			case "uint32":
				return typeUint32, nil
			case "uint64":
				return typeUint64, nil
			case "float32":
				return typeFloat32, nil
			case "float64":
				return typeFloat64, nil
			case "complex64":
				return typeComplex64, nil
			case "complex128":
				return typeComplex128, nil
			case "uintptr":
				return typeUintptr, nil
			}
		}
		_, obj, err := c.resolveSymbol(file, t.Name(), adjuster)
		if err != nil {
			return nil, err
		}
		return obj.Type(), nil
	}
}

func GetUnderlyingType(t types.Type) (named types.Type, underlying types.Type) {
	nt := t
	for {
		t = nt.Underlying()
		if p, ok := t.(*types.Pointer); ok {
			nt = p.Elem()
		} else {
			break
		}
	}
	return nt, t
}

func asArray(slice reflect.Value) reflect.Value {
	if slice.Kind() != reflect.Slice {
		panic(fmt.Sprintf("argument must be a slice, instead was %v", slice.Type()))
	}
	array := reflect.Zero(reflect.ArrayOf(slice.Len(), slice.Type().Elem()))
	for i := 0; i < slice.Len(); i++ {
		e := slice.Index(i)
		if e.Kind() == reflect.Slice {
			e = asArray(e)
		}
		array.Index(i).Set(e)
	}
	return array
}
