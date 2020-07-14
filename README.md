# Anno-Go
[![Build Status](https://travis-ci.com/jhump/annogo.svg?branch=master)](https://travis-ci.com/github/jhump/annogo/branches)
[![Go Report Card](https://goreportcard.com/badge/github.com/jhump/annogo)](https://goreportcard.com/report/github.com/jhump/annogo)
[![GoDoc](https://godoc.org/github.com/jhump/annogo?status.svg)](https://godoc.org/github.com/jhump/annogo)

Annotations and annotation processing for Go!

### NOTE: Still Under Construction!

## Annotations

Annotations take the shape of specially-formatted content in Go doc comments.
They can appear on types declarations, interface elements (embedded interfaces
and methods), struct fields, and other top-level program elements like consts,
vars, and funcs (including methods).

Annotation values are strongly-typed and structured using syntax familiar to
Go programmers:

Any type that has the `@annogo.Annotation` "meta-annotation" can be used as an
annotation type.
```go
// @annogo.Annotation
type MyNewAnnotation struct {
    Foo int
    Bar []string
}
```

And it can then be referenced in annotations on other program elements:
```go
// @MyNewAnnotation{Foo: 123, Bar: {"a", "b", "c"}}
var AnnotatedThing = 1
```

## Annotation Processing

This repo includes a program, `aptgo`, that can be used in a `go:generate`
directive to perform annotation processing.

Its default behavior is to generate `*.annos.go` source code that contains
`init` functions that register all runtime-visible annotation values. This
allows programs to introspect, at runtime, on the annotations present on a
program element.

It also allows for invoking other processors that may perform custom logic
to validate annotations and/or perform extra code generation based on the
annotations.

To that end, this repo includes the `processor` sub-package, which provides
APIs used by annotation processors for querying annotations from source.
This is similar to Java's annotation processing functionality, and this
also includes an `AnnotationMirror` type, which allows processors to inspect
and interact with annotation values that are defined in source, even for
values whose types are not known to the processor.
