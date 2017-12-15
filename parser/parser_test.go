package parser

import (
	"strings"
	"testing"
)

func TestParseAnnotations(t *testing.T) {
	var input = `
@NoValue
@SimpleValue(123)
@StructAnnotation{Foo: {Bar, Baz}, Id: 10101, Name: "dilapidacious"}
@SliceAnnotation{1, 2, 3, 4, 5, 6, 7, 8}
@MapAnnotation{0x101: "foo", 0x202: "bar", 0x303: "baz"}
@Arithmetic(imag(100i * complex(0.01, 2.0002)) - 1.0234E-56 + 1024/4096 * real(3456+9876i))
@Logical(111 >= 109 && -111 <= 109)
@Strings("foo" + "bar" + "baz" + "bonkers" + "bedazzle")
`

	annos, err := ParseAnnotations("foo", strings.NewReader(input))
	if err != nil {
		t.Fatalf("failed to parse annotations: %v", err)
	}
	if len(annos) != 8 {
		t.Fatalf("expecting to have parsed 8 annotations; instead parsed %d", len(annos))
	}
}
