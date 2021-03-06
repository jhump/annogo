// Command aptgo is the annotation processing tool for Go. It processes all
// annotations found in the given packages, generates code to register the
// runtime-visible annotations (so they can be queried at runtime), and then
// also invokes any processors registered by given Go plugins, for running
// custom annotation processors.
//
// The logic that generates init code (for registering runtime-visible
// annotation values) is itself an annotation processor.
package main

import (
	"flag"
	"fmt"
	"go/types"
	"os"
	"plugin"
	"reflect"
	"sort"
	"strings"

	"github.com/jhump/gopoet"

	"github.com/jhump/annogo"
	"github.com/jhump/annogo/processor"
)

func main() {
	test := flag.Bool("include_tests", false, "Indicates whether to process test files.")
	outputDir := flag.String("output_dir", "", "Indicates the root directory where generated files are written."+
		" This is a root, like GOPATH. So files are created under its 'src' sub-directory, organized by package path.")
	var plugins multiString
	flag.Var(&plugins, "plugins", "Indicates the path to a Go plugin to load. The plugin is expected to register processors"+
		" in the init function of its main package. This argument may be specified multiple times to have aptgo load multiple plugins.")
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

	all := processor.AllRegisteredProcessors()
	for _, pl := range plugins {
		before := len(all)
		if _, err := plugin.Open(pl); err != nil {
			fmt.Fprintf(os.Stderr, "Failed to load plugin %s: %v\n", pl, err)
			os.Exit(1)
		}
		all = processor.AllRegisteredProcessors()
		if len(all) <= before {
			fmt.Fprintf(os.Stderr, "Plugin %s is not a valid aptgo plugin: it did not register any processors\n", pl)
			os.Exit(1)
		}
	}

	if err := processor.Process(flag.Args(), *test, *outputDir, all...); err != nil {
		fmt.Fprintln(os.Stderr, err.Error())
		os.Exit(1)
	}
}

func init() {
	processor.RegisterProcessor(baseProcessor)
}

type multiString []string

func (m multiString) String() string {
	return strings.Join(m, ",")
}

func (m *multiString) Set(s string) error {
	*m = append(*m, s)
	return nil
}

func (m multiString) Get() interface{} {
	return []string(m)
}

var typeOfAnnotation = reflect.TypeOf((*annogo.Annotation)(nil)).Elem()
var annotationsPkgPath = typeOfAnnotation.PkgPath()

type annoToProcess struct {
	el  *processor.AnnotatedElement
	val *processor.AnnotationMirror
}

func isSameType(t1 reflect.Type, t2 *types.TypeName) bool {
	return t1.PkgPath() == t2.Pkg().Path() && t1.Name() == t2.Name()
}

func baseProcessor(context *processor.Context, output processor.OutputFactory) error {
	if context.NumElements() == 0 {
		// nothing to do!
		return nil
	}

	var allAnnotations []annoToProcess
	for n := 0; n < context.NumElements(); n++ {
		ae := context.GetElement(n)
		for annoIndex := range ae.Annotations {
			allAnnotations = append(allAnnotations, annoToProcess{el: ae, val: &ae.Annotations[annoIndex]})
		}
	}
	sort.SliceStable(allAnnotations, func(i, j int) bool {
		// We must register @Annotation instances before anything else tries
		// to register usages of those annotated types.
		iIsAnno := isSameType(typeOfAnnotation, allAnnotations[i].val.Metadata.Type)
		jIsAnno := isSameType(typeOfAnnotation, allAnnotations[j].val.Metadata.Type)
		if iIsAnno && !jIsAnno {
			return true
		}
		return false
	})

	outputPkg := context.Package.Pkg
	// TODO: need to generate separate "<pkg>.annos_test.go" and
	// "<pkg>_test.annos_test.go" files if the given annotations include
	// test code (which can be determined by inspecting the package and file
	// details of the annotated elements).
	file := gopoet.NewGoFile(fmt.Sprintf("%s.annos.go", outputPkg.Name()), outputPkg.Path(), outputPkg.Name())

	// we always reference this package, to call the Register* functions
	annosPkg := gopoet.PackageForGoType(context.Program.Package(annotationsPkgPath).Pkg)

	initFunc := gopoet.NewFunc("init")
	initFunc.Comment = "DO NOT EDIT!\n" +
		"This file was generated by aptgo. It registers all runtime-visible annotations\n" +
		"found in this package."
	for i, ap := range allAnnotations {
		ae := ap.el
		anno := ap.val

		if i != 0 {
			initFunc.Println("")
		}

		varName := fmt.Sprintf("v%d", i)
		v := anno.Value
		v.Type = anno.Metadata.Type.Type()
		generateAnnotationValueDecl(&initFunc.CodeBlock, varName, v)

		switch {
		case ae.IsElementType(annogo.Types):
			initFunc.Printf("%s(", annosPkg.Symbol("RegisterTypeAnnotation"))
			generateReflectType(&initFunc.CodeBlock, ae.Obj.Type())
			initFunc.Println(",")
		case ae.IsElementType(annogo.Fields):
			initFunc.Printf("%s(", annosPkg.Symbol("RegisterFieldAnnotation"))
			generateReflectType(&initFunc.CodeBlock, ae.Parent.Obj.Type())
			initFunc.Printlnf(", %q,", ae.Obj.Name())
		case ae.IsElementType(annogo.Functions):
			initFunc.Printlnf("%s(%s,", annosPkg.Symbol("RegisterFunctionAnnotation"), ae.Obj)
		case ae.IsElementType(annogo.InterfaceMethods):
			initFunc.Printf("%s(", annosPkg.Symbol("RegisterInterfaceMethodAnnotation"))
			generateReflectType(&initFunc.CodeBlock, ae.Parent.Obj.Type())
			initFunc.Printlnf(", %q,", ae.Obj.Name())
		case ae.IsElementType(annogo.InterfaceEmbeds):
			initFunc.Printf("%s(", annosPkg.Symbol("RegisterInterfaceEmbedAnnotation"))
			generateReflectType(&initFunc.CodeBlock, ae.Parent.Obj.Type())
			initFunc.Print(", ")
			generateReflectType(&initFunc.CodeBlock, ae.Obj.Type())
			initFunc.Println(",")
		case ae.IsElementType(annogo.Variables):
			initFunc.Printlnf("%s(%s, %q, %q,", annosPkg.Symbol("RegisterVarAnnotation"),
				ae.Obj, ae.Context.Package.Pkg.Path(), ae.Obj.Name())
		case ae.IsElementType(annogo.Constants):
			initFunc.Printf("%s(", annosPkg.Symbol("RegisterConstAnnotation"))
			generateReflectType(&initFunc.CodeBlock, ae.Obj.Type())
			initFunc.Printlnf(", %q %q,", ae.Obj.Pkg().Path(), ae.Obj.Name())
		}
		generateReflectType(&initFunc.CodeBlock, anno.Metadata.Type.Type())
		initFunc.Printlnf(", %s)", varName)
	}
	file.AddElement(initFunc)

	return gopoet.WriteGoFiles(output, file)
}

func generateAnnotationValueDecl(out *gopoet.CodeBlock, name string, val processor.AnnotationValue) {
	//TODO: handle factory funcs (e.g. declare input and then declare result via function invocation)
	var cb gopoet.CodeBlock
	cb.Printf("%s := ", name)
	generateAnnotationValue(out, &cb, name, true, true, val)
	cb.Println("")

	// once we're done, we can add the code for this variable to the output
	out.AddCode(&cb)
}

func generateAdapterFunctionDecl(out *gopoet.CodeBlock, name string, fromType, toType types.Type) {
	var cb gopoet.CodeBlock
	cb.Printf("%s := ", name)
	// TODO: use type conversion for named function types
	// TODO: create adapter function; convert maps, slices, arrays, pointers, and funcs
	// TODO: panic if need to convert a chan
	// TODO: register the adapter

	// once we're done, we can add the code for this variable to the output
	out.AddCode(&cb)
}

func generateAnnotationValue(out, curr *gopoet.CodeBlock, base string, requireCompoundName, requireScalarName bool, val processor.AnnotationValue) {
	if val.Kind == processor.KindNil {
		if requireScalarName {
			curr.Printf("%s(nil)", val.Type)
		} else {
			curr.Print("nil")
		}
		return
	}

	if val.Ref != nil {
		requireScalarName = !types.Identical(val.Ref.Type(), val.Type)
	}

	switch t := val.Type.Underlying().(type) {
	case *types.Pointer:
		v := val
		v.Type = t.Elem()
		isAddressable := true
		switch v.Type.Underlying().(type) {
		case *types.Pointer, *types.Basic:
			isAddressable = false
		}
		if !isAddressable {
			varName := fmt.Sprintf("%s_p", base)
			generateAnnotationValueDecl(out, varName, v)
			curr.Print(varName)
		} else {
			if requireCompoundName {
				curr.Print("&")
			}
			generateAnnotationValue(out, curr, base, requireCompoundName, false, v)
		}

	case *types.Basic:
		canSkipName := true
		if requireScalarName {
			// we can still skip scalar name if the actual type is
			// "bool", "string", "int", "float64", or "complex128".
			canSkipName = t == val.Type &&
				(t.Kind() == types.Bool || t.Kind() == types.String ||
					t.Kind() == types.Int || t.Kind() == types.Float64 ||
					t.Kind() == types.Complex128 || t.Kind() == types.Rune)
			if !canSkipName {
				curr.Printf("%s(", val.Type)
			}
		}
		if val.Ref != nil {
			curr.Printf("%s", val.Ref)
		} else {
			switch val.Kind {
			case processor.KindBool:
				curr.Printf("%v", val.AsBool())
			case processor.KindInt:
				if t.Kind() == types.Rune {
					curr.Printf("%q", val.AsInt())
				} else {
					curr.Printf("%d", val.AsInt())
				}
			case processor.KindUint:
				curr.Printf("%d", val.AsUint())
			case processor.KindFloat:
				curr.Printf("%f", val.AsFloat())
			case processor.KindComplex:
				c := val.AsComplex()
				if real(c) != 0 {
					curr.Printf("%f", real(c))
					if imag(c) > 0 {
						curr.Print("+")
					}
				}
				if imag(c) != 0 {
					curr.Printf("%fi", imag(c))
				}
			case processor.KindString:
				curr.Printf("%q", val.AsString())
			default:
				panic(fmt.Sprintf("unexpected kind of annotation value for basic type %s: %s", val.Type.Underlying(), val.Kind))
			}
		}
		if requireScalarName && !canSkipName {
			curr.Print(")")
		}

	case *types.Signature:
		// TODO: due to presence of annogo.AnyType or annogo.SelfType, it is
		// possible for the named function to have mismatching type, in which
		// case we need to generate an anonymous function here that adapts the
		// signature. If any array, slice, or map values need to be adapted,
		// they will need to be copied (possibly recursively) to convert the
		// value to the correct type. The generated code should register any
		// adapters via annogo.RegisterAdaptedFunction.
		fn := val.AsFunc()
		if !types.AssignableTo(fn.Type(), t) {
			varName := fmt.Sprintf("%s_p", base)
			generateAdapterFunctionDecl(out, varName, fn.Type(), t)
			curr.Print(varName)
		} else {
			curr.Printf("%s", val.AsFunc())
		}

	case *types.Array, *types.Slice:
		if requireCompoundName {
			curr.Printf("%s", val.Type)
		}
		curr.Println("{")
		slice := val.AsSlice()
		for i := range slice {
			generateAnnotationValue(out, curr, fmt.Sprintf("%s_%d", base, i), false, false, slice[i])
			curr.Println(",")
		}
		curr.Print("}")

	case *types.Map:
		if requireCompoundName {
			curr.Printf("%s", val.Type)
		}
		curr.Println("{")
		mp := val.AsMap()
		for i := range mp {
			generateAnnotationValue(out, curr, fmt.Sprintf("%s_%dk", base, i), false, false, mp[i].Key)
			curr.Print(": ")
			generateAnnotationValue(out, curr, fmt.Sprintf("%s_%dv", base, i), false, false, mp[i].Value)
			curr.Println(",")
		}
		curr.Print("}")

	case *types.Struct:
		if requireCompoundName {
			curr.Printf("%s", val.Type)
		}
		curr.Println("{")
		str := val.AsStruct()
		for i := range str {
			curr.Printf("%s: ", str[i].Field.Name())
			generateAnnotationValue(out, curr, fmt.Sprintf("%s_%d", base, i), true, false, str[i].Value)
			curr.Println(",")
		}
		curr.Print("}")

	default:
		panic(fmt.Sprintf("unexpected type of annotation value: %s", val.Type.Underlying()))
	}
}

var reflectTypeOf = gopoet.NewPackage("reflect").Symbol("TypeOf")

func generateReflectType(out *gopoet.CodeBlock, t types.Type) {
	out.Printf("%s((*%s)(nil)).Elem()", reflectTypeOf, t)
}
