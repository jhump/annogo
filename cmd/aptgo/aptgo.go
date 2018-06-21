package main

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"reflect"
	"runtime"
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

var annotationsPkg = reflect.TypeOf((*annogo.Annotation)(nil)).Elem().PkgPath()

func baseProcessor(context *processor.Context, outputDir string) error {
	if len(context.AllElements) == 0 {
		// nothing to do!
		return nil
	}

	outputPkg := context.Package.Pkg
	file := gopoet.NewGoFile(fmt.Sprintf("%s.annos.go", outputPkg.Name()), outputPkg.Path(), outputPkg.Name())

	// we always reference this package, to call the Register* functions
	annosPkg := context.Program.Package(annotationsPkg).Pkg

	initFunc := gopoet.NewFunc("init")
	for _, ae := range context.AllElements {
		switch {
		case ae.IsElementType(annogo.Types):
		case ae.IsElementType(annogo.Fields):
		case ae.IsElementType(annogo.Functions):
		case ae.IsElementType(annogo.InterfaceMethods):
		case ae.IsElementType(annogo.InterfaceEmbeds):
		case ae.IsElementType(annogo.Variables):
		case ae.IsElementType(annogo.Constants):
		}
		for _, anno := range ae.Annotations {
			initFunc.Printlnf("// %s.Register(%s, %s)", annosPkg, ae.Obj, anno.Metadata.Type)
		}
	}
	file.AddElement(initFunc)

	out, err := os.OpenFile(filepath.Join(outputDir, file.Name), os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0666)
	if err != nil {
		return err
	}
	return gopoet.WriteGoFile(out, file)
}
