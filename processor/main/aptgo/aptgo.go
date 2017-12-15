package main

import (
	"flag"
	"fmt"
	"go/types"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strings"

	"github.com/jhump/annogo"
	"github.com/jhump/annogo/processor"
)

func main() {
	test := flag.Bool("include_tests", false, "Indicates whether to process test files.")
	outputDir := flag.String("output_dir", "", "Indicates the root directory where generated files are written."+
		" This is a root, like GOPATH. So files are created under its 'src' sub-directory, organized by package path.")
	flag.Parse()
	if flag.NArg() == 0 {
		fmt.Fprintln(os.Stderr, "Must supply at least one package name")
		os.Exit(1)
	}

	if *outputDir != "" {
		_, err := os.Stat(*outputDir)
		if os.IsNotExist(err) {
			fmt.Fprintf(os.Stderr, "Specified directory, %s, does not exist!", *outputDir)
			os.Exit(1)
		} else if err != nil {
			fmt.Fprintf(os.Stderr, "Failed to check specified directory, %s: %s!", *outputDir, err.Error())
			os.Exit(1)
		}
	}

	outputs := map[string]string{}
	for _, pkg := range flag.Args() {
		out, err := determineOutputDir(*outputDir, pkg)
		if err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			os.Exit(1)
		}
		outputs[pkg] = out
	}

	for _, pkg := range flag.Args() {
		if err := processor.ProcessAll(pkg, *test, outputs[pkg]); err != nil {
			fmt.Fprintln(os.Stderr, err.Error())
			os.Exit(1)
		}
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

func init() {
	processor.RegisterProcessor(baseProcessor)
}

var annotationsPkg = reflect.TypeOf((*annogo.Annotation)(nil)).Elem().PkgPath()

func baseProcessor(context *processor.Context, outputDir string) error {
	outputName := filepath.Join(outputDir, fmt.Sprintf("%s.annos.go", context.Package.Pkg.Name()))

	out, err := os.OpenFile(outputName, os.O_CREATE|os.O_WRONLY, 0666)
	if err != nil {
		return err
	}

	imports := importSpecs{
		packagesByAlias:  map[string]string{},
		aliasesByPackage: map[string]string{},
		isAlias:          map[string]bool{},
	}

	// we always reference this package, to call the Register* functions
	imports.addPackage(context.Program.Package(annotationsPkg).Pkg)

	// one pass through to accumulate all imports
	for _, ae := range context.AllElements {
		for _, am := range ae.Annotations {
			// We have to reference the annotation type
			imports.addPackage(am.Metadata.Type.Pkg())
			// And we may have to reference types in the value
			imports.addAllTypes(am.Value)
		}
	}

	fmt.Fprintf(out, "package %s\n", context.Package.Pkg.Name())
	fmt.Fprintln(out)
	imports.printImports(out)
	fmt.Fprintln(out)
	fmt.Fprintln(out, "func init() {")

	for _, ae := range context.AllElements {
		switch {
		case ae.IsElementType(annogo.Types):
		case ae.IsElementType(annogo.Functions):
		case ae.IsElementType(annogo.InterfaceMethods):
		case ae.IsElementType(annogo.InterfaceEmbeds):
		case ae.IsElementType(annogo.Variables):
		case ae.IsElementType(annogo.Constants):
		}
		fmt.Fprintf(out, "	annotations.Register")
	}

	fmt.Fprintln(out, "}")

	fmt.Printf("OUTPUT = %s\n", outputName)
	objs := map[types.Object]*processor.AnnotatedElement{}
	for pkg, names := range context.AllAnnotationTypes {
		for _, nm := range names {
			for _, el := range context.ElementsAnnotatedWith(pkg, nm) {
				objs[el.Obj] = el
			}
		}
	}

	for obj, el := range objs {
		fmt.Printf("%v (%T):\n", obj, obj)
		fmt.Printf("  Element Types: %v\n", el.ApplicableTypes)
		fmt.Printf("  Filename: %v\n", el.GetDeclaringFilename())
		if el != context.AllElements[obj] {
			fmt.Println("  !! Object mismatch !!")
		}
		var prevMeta *processor.AnnotationMetadata
		for _, m := range el.Annotations {
			if m.Metadata != prevMeta {
				prevMeta = m.Metadata
				fmt.Printf("  Annotation @%s.%s:\n", prevMeta.Type.Pkg().Path(), prevMeta.Type.Name())
				fmt.Printf("  { Representation: %v, FactoryFunc: %v, AllowRepeated: %v, AllowedElements %v }\n", prevMeta.Representation, prevMeta.FactoryFunc, prevMeta.AllowRepeated, prevMeta.AllowedElements)
				prevMeta = m.Metadata
			}
			fmt.Printf("      Type: %v\n", m.Value.Type)
			fmt.Printf("      Value: %#v\n", m.Value.Value)
		}
	}
	return nil
}

type importSpecs struct {
	packagesByAlias  map[string]string
	aliasesByPackage map[string]string
	isAlias          map[string]bool
}

func (s importSpecs) addAllTypes(av processor.AnnotationValue) {
	// since untyped constants can be freely assigned to appropriate
	// types, we will only need to emit code that declares a type for
	// the following kinds of values
	switch av.Kind {
	case processor.KindSlice:
		s.addType(av.Type)
		for _, v := range av.AsSlice() {
			s.addAllTypes(v)
		}
	case processor.KindStruct:
		s.addType(av.Type)
		for _, v := range av.AsStruct() {
			s.addAllTypes(v.Value)
		}
	case processor.KindMap:
		s.addType(av.Type)
		for _, v := range av.AsMap() {
			s.addAllTypes(v.Key)
			s.addAllTypes(v.Value)
		}
	}
}

func (s importSpecs) addType(t types.Type) {
	switch t := t.(type) {
	case *types.Named:
		s.addPackage(t.Obj().Pkg())
	case *types.Slice:
		s.addType(t.Elem())
	case *types.Array:
		s.addType(t.Elem())
	case *types.Map:
		s.addType(t.Key())
		s.addType(t.Elem())
	case *types.Signature:
		for i := 0; i < t.Params().Len(); i++ {
			s.addType(t.Params().At(i).Type())
		}
		for i := 0; i < t.Results().Len(); i++ {
			s.addType(t.Results().At(i).Type())
		}
	case *types.Struct:
		for i := 0; i < t.NumFields(); i++ {
			s.addType(t.Field(i).Type())
		}
	case *types.Interface:
		for i := 0; i < t.NumEmbeddeds(); i++ {
			s.addPackage(t.Embedded(i).Obj().Pkg())
		}
		for i := 0; i < t.NumExplicitMethods(); i++ {
			s.addType(t.Method(i).Type())
		}
	}
}

func (s importSpecs) addPackage(p *types.Package) {
	pkgPath := p.Path()
	name := p.Name()
	if _, ok := s.aliasesByPackage[pkgPath]; ok {
		// already added
		return
	}
	alias := name
	i := 0
	for {
		if _, ok := s.packagesByAlias[alias]; !ok {
			// name already in use
			break
		}
		i++
		alias = fmt.Sprintf("%s%d", name, i)
	}
	s.packagesByAlias[alias] = pkgPath
	s.aliasesByPackage[pkgPath] = alias
	s.isAlias[alias] = i > 0
}

func (s importSpecs) printImports(w io.Writer) {
	pkgs := make([]string, len(s.aliasesByPackage))
	i := 0
	for pkg := range s.aliasesByPackage {
		pkgs[i] = pkg
		i++
	}
	sort.Strings(pkgs)
	fmt.Fprintln(w, "import (")
	for _, pkg := range pkgs {
		alias := s.aliasesByPackage[pkg]
		if s.isAlias[alias] {
			fmt.Fprintf(w, "\t%s %q\n", alias, pkg)
		} else {
			fmt.Fprintf(w, "\t%q\n", pkg)
		}
	}
	fmt.Fprintln(w, ")")
}
