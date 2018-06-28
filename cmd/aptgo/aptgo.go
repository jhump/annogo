package main

import (
	"flag"
	"fmt"
	"go/types"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
	"sort"
	"strings"

	"github.com/jhump/annogo"
	"github.com/jhump/annogo/processor"
	"github.com/jhump/gopoet"
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

var typeOfAnnotation = reflect.TypeOf((*annogo.Annotation)(nil)).Elem()
var annotationsPkgPath = typeOfAnnotation.PkgPath()

type annoToProcess struct {
	el  *processor.AnnotatedElement
	val *processor.AnnotationMirror
}

func isSameType(t1 reflect.Type, t2 *types.TypeName) bool {
	return t1.PkgPath() == t2.Pkg().Path() && t1.Name() == t2.Name()
}

func baseProcessor(context *processor.Context, outputDir string) error {
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
	file := gopoet.NewGoFile(fmt.Sprintf("%s.annos.go", outputPkg.Name()), outputPkg.Path(), outputPkg.Name())

	// we always reference this package, to call the Register* functions
	annosPkg := gopoet.PackageForGoType(context.Program.Package(annotationsPkgPath).Pkg)

	initFunc := gopoet.NewFunc("init")
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

	out, err := os.OpenFile(filepath.Join(outputDir, file.Name), os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		return err
	}
	return gopoet.WriteGoFile(out, file)
}

func generateAnnotationValueDecl(out *gopoet.CodeBlock, name string, val processor.AnnotationValue) {
	var cb gopoet.CodeBlock
	cb.Printf("%s := ", name)
	generateAnnotationValue(out, &cb, name, true, true, val)
	cb.Println("")

	// once we're done, we can add the code for this variable to the output
	out.AddCode(&cb)
}

func generateAnnotationValue(out, curr *gopoet.CodeBlock, base string, requireCompoundName, requireScalarName bool, val processor.AnnotationValue) {
	if val.Kind == processor.KindNil {
		if !requireScalarName {
			curr.Printf("%s(nil)", val.Type)
		} else {
			curr.Print("nil")
		}
		return
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
					t.Kind() == types.Complex128)
			if !canSkipName {
				curr.Printf("%s(", val.Type)
			}
		}
		switch val.Kind {
		case processor.KindBool:
			curr.Printf("%v", val.AsBool())
		case processor.KindInt:
			curr.Printf("%d", val.AsInt())
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
		if requireScalarName && !canSkipName {
			curr.Print(")")
		}

	case *types.Signature:
		curr.Printf("%s", val.AsFunc())

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