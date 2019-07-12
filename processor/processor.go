package processor

import (
	"bytes"
	"fmt"
	"go/ast"
	goparser "go/parser"
	"go/token"
	"go/types"
	"io"
	"math"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
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
	//lint:ignore SA1019 we still support Go 1.10 which does not have non-deprecated NewInterfaceType
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

// Error implements the error interface. It includes position information in the
// returned message.
func (e *ErrorWithPosition) Error() string {
	return fmt.Sprintf("%s:%d:%d: %s", e.pos.Filename, e.pos.Line, e.pos.Column, e.err.Error())
}

// Underlying returns the underlying error.
func (e *ErrorWithPosition) Underlying() error {
	return e.err
}

// Pos returns the location in source where the underlying error was
// encountered.
func (e *ErrorWithPosition) Pos() token.Position {
	return e.pos
}

// NewErrorWithPosition returns the given error, but associates it with the
// given source code location.
func NewErrorWithPosition(pos token.Position, err error) *ErrorWithPosition {
	return &ErrorWithPosition{err: err, pos: pos}
}

// OutputFactory is a function that creates a writer to an output for the
// given location. Output factories typically use os.OpenFile to create files
// but this function allows the behavior to be customized.
type OutputFactory func(path string) (io.WriteCloser, error)

// Processor is a function that acts on annotations and is invoked from the
// annotation processor tool. Typical processor implementations generate code
// based on the annotations present in source.
type Processor func(ctx *Context, output OutputFactory) error

// ProcessAll invokes all registered Processor instances to process the given
// packages. If the given outputDir is blank, the output will be the GOPATH
// directory that contains the sources for a particular package.
func ProcessAll(pkgPaths []string, includeTest bool, outputDir string) error {
	return Process(pkgPaths, includeTest, outputDir, AllRegisteredProcessors()...)
}

// Process invokes the given processors to process the given packages.
func Process(pkgPaths []string, includeTest bool, outputDir string, procs ...Processor) error {
	importPkgs := map[string]bool{}
	for _, pkgPath := range pkgPaths {
		importPkgs[pkgPath] = includeTest
	}
	cfg := Config{
		ImportPkgs:    importPkgs,
		Processors:    procs,
		OutputFactory: DefaultOutputFactory(outputDir),
	}
	return cfg.Execute()
}

// DefaultOutputFactory returns the default OutputFactory used by Process and
// ProcessAll. If the given rootDir is blank, a GOPATH directory will be chosen,
// based on the location of a package's input sources. The actual full path will
// be <rootDir>/src/<path> (note the implicit "src" path element, just like when
// looking for sources in GOPATH).
//
// After computing the destination path, os.OpenFile is used to open the file
// for writing (creating the file if necessary, truncating it if it already
// exists).
func DefaultOutputFactory(rootDir string) OutputFactory {
	return func(path string) (io.WriteCloser, error) {
		dest, err := determineOutputDir(rootDir, filepath.Dir(path))
		if err != nil {
			return nil, err
		}
		dest = filepath.Join(dest, filepath.Base(path))
		return os.OpenFile(dest, os.O_WRONLY|os.O_TRUNC|os.O_CREATE, 0666)
	}
}

func determineOutputDir(root, pkgPath string) (string, error) {
	if root != "" {
		out := filepath.Join(root, "src", pkgPath)
		if err := os.MkdirAll(out, os.ModePerm); err != nil {
			return "", fmt.Errorf("could not create output directory %s: %s", out, err.Error())
		}
		return out, nil
	}
	gopaths := os.Getenv("GOPATH")
	if gopaths != "" {
		for _, gopath := range strings.Split(gopaths, string(filepath.ListSeparator)) {
			out := filepath.Join(gopath, "src", pkgPath)
			_, err := os.Stat(out)
			if err == nil {
				return out, nil
			}
		}
	}
	out := filepath.Join(runtime.GOROOT(), "src", pkgPath)
	_, err := os.Stat(out)
	if err == nil {
		return "", fmt.Errorf("cannot generate output for package %q because it is in GOROOT", pkgPath)
	}
	return "", fmt.Errorf("could not determine output directory for package %q", pkgPath)
}

// Config represents the configuration for running one or more Processors.
// Callers should configure all of the exported fields and then call the
// Execute method to actually invoke the processors.
type Config struct {
	ImportPkgs    map[string]bool
	CreatePkgs    []loader.PkgSpec
	Processors    []Processor
	OutputFactory func(path string) (io.WriteCloser, error)
}

// Execute invokes the configured processors for the configured packages,
// writing outputs using the configured OutputFactory.
func (cfg *Config) Execute() error {
	conf := loader.Config{
		ParserMode:          goparser.ParseComments,
		TypeCheckFuncBodies: func(string) bool { return false },
		ImportPkgs:          cfg.ImportPkgs,
		CreatePkgs:          cfg.CreatePkgs,
	}
	prg, err := conf.Load()
	if err != nil {
		return err
	}
	allContexts := map[*types.Package]*Context{}
	for _, pkgInfo := range prg.InitialPackages() {
		ctx := allContexts[pkgInfo.Pkg]
		if ctx == nil {
			ctx = newContext(pkgInfo, prg, allContexts)
		}
		if err := ctx.computeAllAnnotations(); err != nil {
			return err
		}
		for _, proc := range cfg.Processors {
			if err := proc(ctx, cfg.OutputFactory); err != nil {
				return err
			}
		}
	}
	return nil
}

// Context represents the environment for an annotation processor. It represents
// a single package (for which the processors were invoked). It provides access
// to all annotations and annotated elements encountered in the package.
type Context struct {
	// Package holds all information about the package being processed. It
	// provides access to the ASTs of files in the package as well as the
	// results of type analysis, to allow for introspection of package elements.
	Package *loader.PackageInfo

	// Program holds information about an entire program being processed, which
	// includes any packages that are being processed as well as their
	// dependencies (including indirect dependencies, e.g. the full transitive
	// closure). This also provides access to the token.FileSet, which can be
	// used to resolve details for source code locations.
	Program *loader.Program

	allContexts   map[*types.Package]*Context
	metadata      map[*types.TypeName]*AnnotationMetadata
	fieldMetadata map[types.Object][]AnnotationMirror

	allElements []*AnnotatedElement
	// AllElementsByObject is map of all elements in the package (represented by
	// types.Object instances) that have annotations to a corresponding
	// AnnotatedElement structure.
	//
	// Also see methods Context.NumElements, Context.GetElement, and
	// Context.ElementsOfType.
	AllElementsByObject map[types.Object]*AnnotatedElement
	// AllAnnotationTypes indicates the names of all annotation types found in
	// the package's sources. The map is keyed by package import path, with the
	// values being slices of unqualified names of annotation types in that
	// package.
	AllAnnotationTypes map[string][]string
	byType             map[annogo.ElementType][]*AnnotatedElement
	byAnnotation       map[annoType][]*AnnotatedElement
	processed          map[*ast.CommentGroup]struct{}
}

func newContext(pkg *loader.PackageInfo, prg *loader.Program, contextPool map[*types.Package]*Context) *Context {
	ctx := &Context{
		Package:             pkg,
		Program:             prg,
		AllElementsByObject: map[types.Object]*AnnotatedElement{},
		AllAnnotationTypes:  map[string][]string{},
		byType:              map[annogo.ElementType][]*AnnotatedElement{},
		byAnnotation:        map[annoType][]*AnnotatedElement{},
		allContexts:         contextPool,
		metadata:            map[*types.TypeName]*AnnotationMetadata{},
		fieldMetadata:       map[types.Object][]AnnotationMirror{},
		processed:           map[*ast.CommentGroup]struct{}{},
	}
	contextPool[pkg.Pkg] = ctx
	return ctx
}

// GetMetadata returns annotation metadata for the given annotation type. If the
// given type's package has not yet been parsed (possible if the requested
// annotation type is not in the transitive dependencies of the context's
// package), it is parsed and processed.
func (c *Context) GetMetadata(packagePath, name string) (*AnnotationMetadata, error) {
	pkgInfo := c.Program.Package(packagePath)
	if pkgInfo == nil {
		// TODO: ideally, the loader package would have a way to add packages to
		// the current program, instead of having to create a new one (creating a
		// new one means that we may end up re-parsing and re-processing packages
		// that are referenced both by the existing program and by the package we
		// are loading).

		// If we don't know about this package, load it
		conf := loader.Config{
			ParserMode:          goparser.ParseComments,
			TypeCheckFuncBodies: func(string) bool { return false },
			ImportPkgs:          map[string]bool{packagePath: false},
		}
		prg, err := conf.Load()
		if err != nil {
			return nil, err
		}
		pkgInfo = prg.Package(packagePath)
		ctx := newContext(pkgInfo, prg, c.allContexts)
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

// GetMetadataForTypeName returns annotation metadata for the given annotation
// type. If the given type's package has not yet been parsed (possible if the
// requested annotation type is not in the transitive dependencies of the
// context's package), it is parsed and processed.
func (c *Context) GetMetadataForTypeName(t *types.TypeName) (*AnnotationMetadata, error) {
	pkg := c.Program.AllPackages[t.Pkg()]
	return c.getMetadata(t, pkg, parser.Identifier{PackageAlias: t.Pkg().Path(), Name: t.Name()}, nil)
}

// NumElements returns the number of annotated elements for the context's
// package.
//
// Note that only packages configured to be processed will have a non-zero
// number of elements. Other packages (such as dependencies of those being
// processed), may actually have annotations therein, but since they will not
// have been processed, the context will not include them.
func (c *Context) NumElements() int {
	return len(c.allElements)
}

// GetElement returns the annotation element at the given index. The given index
// must be greater than or equal to zero and less than c.NumElements().
//
// Note that only packages configured to be processed will have a non-zero
// number of elements. Other packages (such as dependencies of those being
// processed), may actually have annotations therein, but since they will not
// have been processed, the context will not include them.
func (c *Context) GetElement(index int) *AnnotatedElement {
	return c.allElements[index]
}

// ElementsOfType returns a slice of annotated elements of the given type.
//
// Note that only packages configured to be processed will have a non-zero
// number of elements. Other packages (such as dependencies of those being
// processed), may actually have annotations therein, but since they will not
// have been processed, the context will not include them.
func (c *Context) ElementsOfType(t annogo.ElementType) []*AnnotatedElement {
	return c.byType[t]
}

// ElementsAnnotatedWith returns a slice of elements that have been annotated
// with the given annotation type.
//
// Note that only packages configured to be processed will have a non-zero
// number of elements. Other packages (such as dependencies of those being
// processed), may actually have annotations therein, but since they will not
// have been processed, the context will not include them.
func (c *Context) ElementsAnnotatedWith(packagePath, typeName string) []*AnnotatedElement {
	return c.byAnnotation[annoType{packagePath: packagePath, name: typeName}]
}

func (c *Context) computeAllAnnotations() error {
	// TODO: support package annotations
	// TODO: provide mechanism to "import" packages used by annotations but unused by
	// actual application code (perhaps special '// @import' comments between the
	// package declaration and the first element declaration, among actual imports)

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
			err = fmt.Errorf("%v: annotations are only allowed on top-level types, functions, variables, and constants or fields and methods of top-level types", p)
			return false
		}
		return true
	})
	return err
}

func (c *Context) computeAnnotationsFromElement(file *ast.File, ets []annogo.ElementType, id *ast.Ident, doc *ast.CommentGroup, parent *AnnotatedElement) error {
	obj := c.Package.ObjectOf(id)
	if _, ok := c.AllElementsByObject[obj]; ok {
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
	if _, ok := c.AllElementsByObject[obj]; ok {
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
				for _, n := range names {
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
	c.AllElementsByObject[obj] = ae
	c.allElements = append(c.allElements, ae)
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
		perr := err.(*parser.ParseError)
		pos := adjuster.adjustPosition(perr.Pos())
		return nil, NewErrorWithPosition(pos, perr.Underlying())
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
		return mirror, NewErrorWithPosition(pos, fmt.Errorf("%v is not a type", a.Type))
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
		return mirror, NewErrorWithPosition(pos, fmt.Errorf("%v is not an annotation type", a.Type))
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
		_, u := getUnderlyingType(meta.Representation)
		switch u.(type) {
		case *types.Basic:
			tv.v = true
		case *types.Struct:
			tv.v = ([]parser.Element)(nil)
		default:
			return mirror, NewErrorWithPosition(tv.pos, fmt.Errorf("annotation %v requires a value since its type is not bool or struct", a.Type))
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
									return nil, nil, NewErrorWithPosition(pos, fmt.Errorf("package name %s is ambiguous; could be %q or %q", id.PackageAlias, obj.Pkg().Path(), o.Pkg().Path()))
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
				return nil, nil, NewErrorWithPosition(pos, fmt.Errorf("symbol %v does not exist", id))
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
			ctx = newContext(annoPkg, c.Program, c.allContexts)
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
				if anno.Exported() && !val.AsFunc().Exported() {
					return nil, NewErrorWithPosition(val.Pos,
						fmt.Errorf("annotation %s.%s is exported, so its factory function must also be exported",
							anno.Pkg().Path(), anno.Name()))
				}
				if !val.AsFunc().Exported() && val.AsFunc().Pkg().Path() != anno.Pkg().Path() {
					// TODO: remove this check? type analysis should have already caught this
					// error and caused processing to fail before we get to this point
					return nil, NewErrorWithPosition(val.Pos,
						fmt.Errorf("annotation %s.%s cannot use unexpected factory function from another package",
							anno.Pkg().Path(), anno.Name()))
				}
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
			ctx = newContext(fieldPkg, c.Program, c.allContexts)
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
	tv.t, tv.v, tv.ref, err = c.getExpressionValue(file, exp, adjuster)
	if err != nil {
		return AnnotationValue{}, err
	}
	return c.convertValue(file, tv, selfType, targetType, p, adjuster)
}

type typeAndVal struct {
	t   types.Type
	v   interface{}
	ref *types.Const
	pos token.Position
}

func (c *Context) convertValue(file *ast.File, tv typeAndVal, selfType, targetType types.Type, p *types.Package, adjuster posAdjuster) (av AnnotationValue, err error) {
	av, err = c.tryConvertValue(file, tv, selfType, targetType, p, adjuster)
	if wte, ok := err.(wrongTypeError); ok {
		ntyp, typ := getUnderlyingType(targetType)
		var elemType types.Type
		var fld *types.Var
		switch t := typ.(type) {
		case *types.Array:
			if t.Len() != 1 {
				return nilValue, NewErrorWithPosition(tv.pos, fmt.Errorf("array must have length %d but given literal value has 1 element", t.Len()))
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
	defer func() {
		if err == nil {
			av.Ref = tv.ref
		}
	}()

	ntyp, typ := getUnderlyingType(targetType)

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
					return nilValue, NewErrorWithPosition(tv.pos, fmt.Errorf("array must have length %d but given literal value has %d elements", t.Len(), len(v)))
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
		return nilValue, wrongTypeError{err: NewErrorWithPosition(tv.pos, fmt.Errorf("annotation value of type %s cannot be assigned to %v", sourceType, targetType))}
	} else {
		return nilValue, wrongTypeError{err: NewErrorWithPosition(tv.pos, fmt.Errorf("annotation value of type %s cannot be assigned to %v (%v)", sourceType, targetType, rewritten))}
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
				case types.UntypedInt:
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

// IsAnyType returns true if the given type is annogo.AnyType.
func IsAnyType(t types.Type) bool {
	return isNamedType(t, anyTypePkg, anyTypeName)
}

// IsSelfType returns true if the given type is annogo.SelfType.
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
			return nilValue, NewErrorWithPosition(pos, fmt.Errorf("too few values for struct type %s; expecting %d, got %d", nt.String(), structType.NumFields(), len(v)))
		}
		for i, fldVal := range v {
			fld := structType.Field(i)
			if !fld.Exported() && !isLocal {
				pos := adjuster.adjustPosition(fldVal.Pos())
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("cannot set non-exported field %s of type %s", fld.Name(), nt.String()))
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
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("cannot assign map value to type %s", nt.String()))
			}
			if _, ok := fieldNames[fldName]; ok {
				pos := adjuster.adjustPosition(fldVal.Key.Pos())
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("struct value has duplicate entries: field %q", fldName))
			}
			fld := fields[fldName]
			if fld == nil {
				pos := adjuster.adjustPosition(fldVal.Pos())
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("struct type %s has no field named %s", nt.String(), fldName))
			}
			if !fld.Exported() && !isLocal {
				pos := adjuster.adjustPosition(fldVal.Pos())
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("cannot set non-exported field %s of type %s", fld.Name(), nt.String()))
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
			if ae := c.AllElementsByObject[fld]; ae != nil {
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
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("field %s is not specified but is required", fld.Name()))
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
		return nilValue, NewErrorWithPosition(pos, fmt.Errorf("slice/array values should not have keys"))
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
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("struct value has duplicate entries: field %q", fld.Name()))
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
				return nilValue, NewErrorWithPosition(pos, fmt.Errorf("map value has duplicate entries: key = %v", k))
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
		exptyp, v, _, err := c.getExpressionValue(file, t.Len(), adjuster)
		if err != nil {
			return nil, err
		}
		pos := adjuster.adjustPosition(t.Len().Pos())
		var l int64
		if i, ok := v.(int64); ok {
			if i < 0 {
				return nil, NewErrorWithPosition(pos, fmt.Errorf("array bound is negative: %v", v))
			}
			l = i
		} else if u, ok := v.(uint64); ok {
			if u > math.MaxInt64 {
				return nil, NewErrorWithPosition(pos, fmt.Errorf("array bound overflows int64: %v", v))
			}
			l = int64(u)
		} else {
			return nil, NewErrorWithPosition(pos, fmt.Errorf("array bound must be integer type; got %v", exptyp))
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

func getUnderlyingType(t types.Type) (named types.Type, underlying types.Type) {
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

var typeOfEmptyInterface = reflect.TypeOf((*interface{})(nil)).Elem()

func asArray(slice reflect.Value) reflect.Value {
	if slice.Kind() != reflect.Slice {
		panic(fmt.Sprintf("argument must be a slice, instead was %v", slice.Type()))
	}

	elemType := slice.Type().Elem()
	if elemType.Kind() == reflect.Slice {
		// Since purpose of converting to array is to use as map key, we must
		// recursively convert contents when elements are also slices. But then
		// we cannot necessarily compute a single element type, since the
		// lengths of nested slices may have heterogenous lengths (and arrays
		// with different lengths are necessarily different types, regardless of
		// whether they have the same element type or not). So for this case, we
		// use interface{} as the element type of the result
		elemType = typeOfEmptyInterface
	}

	array := reflect.Zero(reflect.ArrayOf(slice.Len(), elemType))
	for i := 0; i < slice.Len(); i++ {
		e := slice.Index(i)
		if e.Kind() == reflect.Slice {
			e = asArray(e)
		}
		array.Index(i).Set(e)
	}
	return array
}
