package annogo

import "reflect"

func init() {
	v0 := Annotation{
		AllowedElements: []ElementType{
			2,
		},
		RuntimeVisible: true,
	}
	RegisterTypeAnnotation(reflect.TypeOf((*Annotation)(nil)).Elem(),
		reflect.TypeOf((*Annotation)(nil)).Elem(), v0)

	v1 := Annotation{
		AllowedElements: []ElementType{
			2,
			5,
		},
		RuntimeVisible: true,
	}
	RegisterTypeAnnotation(reflect.TypeOf((*FactoryFunc)(nil)).Elem(),
		reflect.TypeOf((*Annotation)(nil)).Elem(), v1)

	v2 := Annotation{
		AllowedElements: []ElementType{
			1,
		},
		RuntimeVisible: true,
	}
	RegisterTypeAnnotation(reflect.TypeOf((*DefaultValue)(nil)).Elem(),
		reflect.TypeOf((*Annotation)(nil)).Elem(), v2)

	v3 := Annotation{
		AllowedElements: []ElementType{
			1,
		},
		RuntimeVisible: true,
	}
	RegisterTypeAnnotation(reflect.TypeOf((*Required)(nil)).Elem(),
		reflect.TypeOf((*Annotation)(nil)).Elem(), v3)

	v4 := DefaultValue{
		Value: true,
	}
	RegisterFieldAnnotation(reflect.TypeOf((*Annotation)(nil)).Elem(), "RuntimeVisible",
		reflect.TypeOf((*DefaultValue)(nil)).Elem(), v4)

	v5 := Required(true)
	RegisterFieldAnnotation(reflect.TypeOf((*DefaultValue)(nil)).Elem(), "Value",
		reflect.TypeOf((*Required)(nil)).Elem(), v5)
}
