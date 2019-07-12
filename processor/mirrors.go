package processor

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"math"
	"reflect"
	"sort"
	"unsafe"

	"github.com/jhump/annogo"
	"unicode"
	"unicode/utf8"
)

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

// Reify attempts to populate the given annotation value with the data in this
// mirror. It requires that the given value be a pointer and the pointed-to type
// must match the type of the mirror. If the type matches, it then delegates to
// the mirror's value:
//     m.Value.Reify(target).
// If the types do not match, an error is returned. If the known type of the
// annotation -- reflect.TypeOf(target) -- is structurally different than the
// source form of the same type -- m.Metadata.Type -- then an error may be
// returned.
func (m AnnotationMirror) Reify(target interface{}) error {
	targetType := reflect.TypeOf(target)
	for targetType.Kind() == reflect.Ptr {
		targetType = targetType.Elem()
	}
	if targetType.PkgPath() != m.Metadata.Type.Pkg().Path() ||
		targetType.Name() != m.Metadata.Type.Name() {

		return fmt.Errorf("annotation mirror of type %v cannot be reified into value of type %T", m.Metadata.Type, target)
	}

	return m.Value.Reify(target)
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

var kindNames = map[ValueKind]string{
	KindInt:     "int",
	KindUint:    "uint",
	KindFloat:   "float",
	KindComplex: "complex",
	KindString:  "string",
	KindBool:    "bool",
	KindNil:     "nil",
	KindFunc:    "func",
	KindSlice:   "slice",
	KindMap:     "map",
	KindStruct:  "struct",
}

func (k ValueKind) String() string {
	if s, ok := kindNames[k]; ok {
		return s
	}
	return "<invalid>"
}

func (k ValueKind) IsScalar() bool {
	return k >= KindInt && k <= KindBool
}

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
	// If the value is a reference to a constant, this is the constant that
	// was referenced. Expressions that include references to constants do not
	// count. E.g. the expression "someConst + 1" would result in a nil Ref, but
	// "someConst" would result in a non-nil Ref to someConst.
	Ref *types.Const

	// The position in source where this annotation value is defined.
	Pos token.Position
}

// AsInt is a convenience function that type asserts the value as an int64.
func (v *AnnotationValue) AsInt() int64 {
	return v.Value.(int64)
}

// AsUint is a convenience function that type asserts the value as a uint64.
func (v *AnnotationValue) AsUint() uint64 {
	return v.Value.(uint64)
}

// AsFloat is a convenience function that type asserts the value as a float64.
func (v *AnnotationValue) AsFloat() float64 {
	return v.Value.(float64)
}

// AsComplex is a convenience function that type asserts the value as a
// complex128.
func (v *AnnotationValue) AsComplex() complex128 {
	return v.Value.(complex128)
}

// AsString is a convenience function that type asserts the value as a string.
func (v *AnnotationValue) AsString() string {
	return v.Value.(string)
}

// AsBool is a convenience function that type asserts the value as a bool.
func (v *AnnotationValue) AsBool() bool {
	return v.Value.(bool)
}

// AsFunc is a convenience function that type asserts the value as a
// *types.Func.
func (v *AnnotationValue) AsFunc() *types.Func {
	return v.Value.(*types.Func)
}

// AsSlice is a convenience function that type asserts the value as a
// []AnnotationValue.
func (v *AnnotationValue) AsSlice() []AnnotationValue {
	return v.Value.([]AnnotationValue)
}

// AsMap is a convenience function that type asserts the value as a
// []AnnotationMapEntry.
func (v *AnnotationValue) AsMap() []AnnotationMapEntry {
	return v.Value.([]AnnotationMapEntry)
}

// AsStruct is a convenience function that type asserts the value as a
// []AnnotationStructEntry.
func (v *AnnotationValue) AsStruct() []AnnotationStructEntry {
	return v.Value.([]AnnotationStructEntry)
}

// Reify attempts to populate the given target value with the data in this
// value. An error is returned if the given value's type is not structurally
// compatible with the value (such as trying to reify a string value from a
// map annotation value).
//
// The given value must be a non-nil pointer. The value to which it points
// is updated to reflect the value represented by v.
//
// If the annotation value is a composite value (slice, array, struct, or map)
// then the elements are recursively reified.
//
// If the annotation value is a function or contains any references to
// functions, the reified value will be a function that panics if called. The
// value recovered from the panic will be a *processor.ErrMirroredFunction,
// which provides access to the *types.Func representation of the actual
// referenced function.
//
// Due to limitations in Go reflection (and the "reflect" package), reification
// is not able to produce values of concrete named types or unnamed structs that
// have unexported fields unless the declared type is also concrete. When the
// declared type is an interface, the reified value must instead use a
// MirroredValue. In some cases, the reified value will actually implement
// MirroredValue directly. In others, the reified value may have a synthetic
// type that *wraps* a MirroredValue. In both cases, the MirroredValue can be
// extracted using processor.GetMirroredValue().
//
// The value will be a synthetic type when the declared type is an interface
// with one or more methods. In that case the synthetic type will implement the
// declared interface (not MirroredValue). But any attempt to invoke an
// interface method on the value will panic.
func (v *AnnotationValue) Reify(target interface{}) error {
	rv := reflect.ValueOf(target)
	if rv.Kind() != reflect.Ptr {
		return fmt.Errorf("cannot reify into non-pointer value of type %T", target)
	}
	if rv.IsNil() {
		return fmt.Errorf("cannot reify into nil pointer of type %T", target)
	}
	// clear out existing value
	rv.Elem().Set(reflect.Zero(rv.Elem().Type()))

	return v.reify(rv.Elem(), false)
}

var typeOfFactoryFunc = reflect.TypeOf(annogo.FactoryFunc(nil))
var typeOfMirroredVal = reflect.TypeOf((*MirroredValue)(nil)).Elem()

func (v *AnnotationValue) reify(target reflect.Value, ignoreFieldNames bool) error {
	if v.Kind == KindNil {
		return reifyFromNil(target)
	}

	if target.Kind() == reflect.Ptr {
		target.Set(reflect.New(target.Type().Elem()))
		return v.reify(target.Elem(), ignoreFieldNames)
	}

	annos := annogo.GetAnnotationsForType(target.Type(), typeOfFactoryFunc)
	if len(annos) != 0 {
		fn := annos[0].Interface()
		if fn != nil {
			fn = annogo.GetUnderlyingFunction(fn)
		}
		rvFn := reflect.ValueOf(fn)
		// For factory functions, we reify the annotation value into
		// the function's argument type.
		arg := reflect.New(rvFn.Type().In(0)).Elem()
		if err := v.reify(arg, ignoreFieldNames); err != nil {
			return err
		}
		// Then we pass the argument to the function and its return
		// value is the value of the target annotation.
		ret := rvFn.Call([]reflect.Value{arg})
		target.Set(ret[0])
		return nil
	}

	if target.Kind() == reflect.Interface {
		return v.reifyInterface(target)
	}

	if v.Kind.IsScalar() {
		return v.reifyScalar(target)
	}

	switch v.Kind {
	case KindFunc:
		return reifyFromFunc(target, v.AsFunc())

	case KindSlice:
		return reifyFromSlice(target, v.AsSlice(), ignoreFieldNames)

	case KindMap:
		return reifyFromMap(target, v.AsMap(), ignoreFieldNames)

	case KindStruct:
		return reifyFromStruct(target, v.Type, v.AsStruct(), ignoreFieldNames)

	default:
		panic(fmt.Sprintf("unexpected kind of annotation value: %v", v.Kind))
	}
}

func (v *AnnotationValue) reifyInterface(target reflect.Value) error {
	typ, ok := makeType(v.Type)
	if typ.Kind() == reflect.Interface {
		panic("no concrete type?")
	}

	reified := reflect.New(typ).Elem()
	if err := v.reify(reified, !ok); err != nil {
		return err
	}

	if !ok || target.NumMethod() > 0 {
		reified = reflect.ValueOf(&mirroredVal{av: v, v: reified.Interface()})
	}

	if target.NumMethod() > 0 {
		wrapperType := reflect.StructOf([]reflect.StructField{
			// embed the interface, so wrapper implements it
			{Name: "Interface", Type: target.Type(), Anonymous: true},
			// we don't embed this one because we don't want wrapper type to
			// implement it since reflect.StructOf doesn't really wire up
			// method forwarding (so we might think we have a valid
			// MirroredValue, but all method calls would panic instead of
			// being properly delegated to the value of this field)
			{Name: "Value", Type: typeOfMirroredVal},
		})
		wrapper := reflect.New(wrapperType).Elem()
		// Leave field 0 nil since we have no valid value we can put there.
		// This means that attempts to invoke methods on the wrapper will
		// panic due to nil de-reference. But, on the up side, the wrapper
		// properly implements the interface, so we'll be able to make the
		// assignment to target below.
		wrapper.Field(1).Set(reified)
		reified = wrapper

	} else if reified.Kind() == reflect.Interface && reified.IsNil() {
		// Reflection is strange with interfaces: the nil interface must
		// be boxed in a value that has a reference to the interface type
		// or else it is invalid (and basically cannot be used). But when
		// boxed, even though nil interface can be assigned to *any*
		// interface type, reflect.Value.Set fails unless the boxed
		// interface type is the same as the destination interface type.
		// Furthermore, reflect.Value.Convert
		//
		// So... if the value is nil, we must use "convert" it to the right
		// type. Luckily, the nil interface of the right type is just the
		// destination interface type's zero value.
		reified = reflect.Zero(target.Type())
	}

	target.Set(reified)
	return nil
}

func (v *AnnotationValue) reifyScalar(target reflect.Value) error {
	var val reflect.Value
	var err error

	switch v.Kind {
	case KindInt:
		val, err = reifyFromInt(target.Type(), v.AsInt())

	case KindUint:
		val, err = reifyFromUint(target.Type(), v.AsUint())

	case KindFloat:
		val, err = reifyFromFloat(target.Type(), v.AsFloat())

	case KindComplex:
		val, err = reifyFromComplex(target.Type(), v.AsComplex())

	case KindString:
		val, err = reifyFromString(target.Type(), v.AsString())

	case KindBool:
		val, err = reifyFromBool(target.Type(), v.AsBool())

	default:
		panic(fmt.Sprintf("unexpected scalar type %v", target.Type()))
	}

	if err != nil {
		return err
	}
	val = val.Convert(target.Type())
	target.Set(val)
	return nil
}

func reifyFromNil(target reflect.Value) error {
	switch target.Kind() {
	case reflect.Slice, reflect.Map, reflect.Chan, reflect.Func,
		reflect.UnsafePointer, reflect.Ptr, reflect.Interface:
		// nil is the zero value for various reference and interface types
		target.Set(reflect.Zero(target.Type()))
		return nil
	}
	return fmt.Errorf("nil is not a valid value for type %v", target.Type())
}

func reifyFromInt(target reflect.Type, intVal int64) (reflect.Value, error) {
	switch target.Kind() {
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Uintptr:
		// unsigned values can't be negative
		if intVal < 0 {
			return reflect.Value{}, fmt.Errorf("value %d is out of range for type %v", intVal, target)
		}
		fallthrough

	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		result := reflect.ValueOf(intVal)
		int64Type := result.Type()
		result = result.Convert(target)
		roundTripped := result.Convert(int64Type).Interface().(int64)
		if roundTripped != intVal {
			return reflect.Value{}, fmt.Errorf("value %d is out of range for type %v", intVal, target)
		}
		return result, nil

	case reflect.Float32, reflect.Float64:
		return reflect.ValueOf(intVal), nil

	case reflect.Complex64, reflect.Complex128:
		return reflect.ValueOf(complex(float64(intVal), 0)), nil

	default:
		return reflect.Value{}, fmt.Errorf("value %d is not valid for type %v (%v)", intVal, target, target.Kind())
	}
}

func reifyFromUint(target reflect.Type, uintVal uint64) (reflect.Value, error) {
	switch target.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		// signed values can't exceed 2^63-1 (other limits, like overflowing int32,
		// will be implicitly checked by round-trip test below)
		if uintVal > math.MaxInt64 {
			return reflect.Value{}, fmt.Errorf("value %d is out of range for type %v", uintVal, target)
		}
		fallthrough

	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64,
		reflect.Uintptr:

		result := reflect.ValueOf(uintVal)
		uint64Type := result.Type()
		result = result.Convert(target)
		roundTripped := result.Convert(uint64Type).Interface().(uint64)
		if roundTripped != uintVal {
			return reflect.Value{}, fmt.Errorf("value %d is out of range for type %v", uintVal, target)
		}
		return result, nil

	case reflect.Float32, reflect.Float64:
		return reflect.ValueOf(uintVal), nil

	case reflect.Complex64, reflect.Complex128:
		return reflect.ValueOf(complex(float64(uintVal), 0)), nil

	default:
		return reflect.Value{}, fmt.Errorf("value %d is not valid for type %v (%v)", uintVal, target, target.Kind())
	}
}

func reifyFromFloat(target reflect.Type, floatVal float64) (reflect.Value, error) {
	switch target.Kind() {
	case reflect.Float32, reflect.Float64:
		return reflect.ValueOf(floatVal), nil

	case reflect.Complex64, reflect.Complex128:
		return reflect.ValueOf(complex(floatVal, 0)), nil

	default:
		return reflect.Value{}, fmt.Errorf("value %f is not valid for type %v (%v)", floatVal, target, target.Kind())
	}
}

func reifyFromComplex(target reflect.Type, complexVal complex128) (reflect.Value, error) {
	switch target.Kind() {
	case reflect.Complex64, reflect.Complex128:
		return reflect.ValueOf(complexVal), nil

	default:
		return reflect.Value{}, fmt.Errorf("value %f is not valid for type %v (%v)", complexVal, target, target.Kind())
	}
}

func reifyFromBool(target reflect.Type, boolVal bool) (reflect.Value, error) {
	if target.Kind() != reflect.Bool {
		return reflect.Value{}, fmt.Errorf("value %v is not valid for type %v (%v)", boolVal, target, target.Kind())
	}
	return reflect.ValueOf(boolVal), nil
}

func reifyFromString(target reflect.Type, strVal string) (reflect.Value, error) {
	if target.Kind() != reflect.String {
		return reflect.Value{}, fmt.Errorf("value %q is not valid for type %v (%v)", strVal, target, target.Kind())
	}
	return reflect.ValueOf(strVal), nil
}

func reifyFromFunc(target reflect.Value, fn *types.Func) error {
	if target.Kind() != reflect.Func {
		return fmt.Errorf("function %s.%s is not valid for type %v (%v)", fn.Pkg().Path(), fn.Name(), target, target.Kind())
	}
	val := reflect.MakeFunc(target.Type(), func([]reflect.Value) []reflect.Value {
		panic((*ErrMirroredFunction)(fn))
	})
	target.Set(val)
	return nil
}

func reifyFromSlice(target reflect.Value, sl []AnnotationValue, ignoreFieldNames bool) error {
	// slice syntax could mean a slice or an array or even an unkeyed struct literal
	switch target.Kind() {
	case reflect.Slice, reflect.Array:
		if target.Kind() == reflect.Array {
			if target.Type().Len() != len(sl) {
				return fmt.Errorf("array literal with %d elements is not valid for type %v (%v)", len(sl), target, target.Kind())
			}
		} else {
			target.Set(reflect.MakeSlice(target.Type(), len(sl), len(sl)))
		}

		for i := range sl {
			el := &sl[i]
			if err := el.reify(target.Index(i), ignoreFieldNames); err != nil {
				return err
			}
		}
		return nil

	case reflect.Struct:
		if target.NumField() != len(sl) {
			return fmt.Errorf("unkeyed struct literal with %d elements is not valid for type %v (%v)", len(sl), target, target.Kind())
		}
		for i := range sl {
			el := &sl[i]
			if err := el.reify(makeSettable(target.Field(i)), ignoreFieldNames); err != nil {
				return err
			}
		}
		return nil

	default:
		return fmt.Errorf("slice value is not valid for type %v (%v)", target, target.Kind())
	}
}

func reifyFromMap(target reflect.Value, mp []AnnotationMapEntry, ignoreFieldNames bool) error {
	// map syntax could mean a map (obviously) but also could be a sparse
	// slice or array (if keys are all integers)
	switch target.Kind() {
	case reflect.Map:
		target.Set(reflect.MakeMap(target.Type()))
		key := reflect.New(target.Type().Key()).Elem()
		val := reflect.New(target.Type().Elem()).Elem()
		for i := range mp {
			entry := &mp[i]
			if err := entry.Key.reify(key, ignoreFieldNames); err != nil {
				return err
			}
			if err := entry.Value.reify(val, ignoreFieldNames); err != nil {
				return err
			}
			target.SetMapIndex(key, val)
		}
		return nil

	case reflect.Slice, reflect.Array:
		maxIndex, err := findMaxIndex(mp)
		if err != nil {
			return err
		}

		if target.Kind() == reflect.Array {
			if target.Type().Len() <= maxIndex {
				return fmt.Errorf("sparse array literal with %d elements is not valid for type %v (%v)", maxIndex+1, target, target.Kind())
			}
		} else {
			target.Set(reflect.MakeSlice(target.Type(), maxIndex+1, maxIndex+1))
		}

		for i := range mp {
			entry := &mp[i]
			index, _ := getIndex(&entry.Key)
			if err := entry.Value.reify(target.Index(index), ignoreFieldNames); err != nil {
				return err
			}
		}
		return nil

	default:
		return fmt.Errorf("map value is not valid for type %v (%v)", target, target.Kind())
	}
}

func findMaxIndex(entries []AnnotationMapEntry) (int, error) {
	maxIndex := -1
	for i := range entries {
		entry := &entries[i]
		index, err := getIndex(&entry.Key)
		if err != nil {
			return 0, err
		}
		if index > maxIndex {
			maxIndex = index
		}
	}
	return maxIndex, nil
}

func getIndex(v *AnnotationValue) (int, error) {
	switch v.Kind {
	case KindInt:
		i64 := v.AsInt()
		index := int(i64)
		if index < 0 || int64(index) != i64 {
			return 0, fmt.Errorf("value %d is out of range of int (array/slice index)", i64)
		}
		return index, nil
	case KindUint:
		u64 := v.AsUint()
		index := int(u64)
		if uint64(index) != u64 || u64 > math.MaxInt64 {
			return 0, fmt.Errorf("value %d is out of range of int (array/slice index)", u64)
		}
		return index, nil
	default:
		return 0, fmt.Errorf("sparse array/slice literal must have integer keys")
	}
}

func reifyFromStruct(target reflect.Value, t types.Type, st []AnnotationStructEntry, ignoreFieldNames bool) error {
	if target.Kind() != reflect.Struct {
		return fmt.Errorf("struct value is not valid for type %v (%v)", target, target.Kind())
	}

	var fieldFinder func(string) (int, error)
	if ignoreFieldNames {
		st := extractStruct(t)
		fieldFinder = func(name string) (int, error) {
			return findSyntheticField(name, st)
		}
	} else {
		typ := target.Type()
		fieldFinder = func(name string) (int, error) {
			return findField(name, typ)
		}
	}

	for i := range st {
		entry := &st[i]
		index, err := fieldFinder(entry.Field.Name())
		if err != nil {
			return err
		}
		if err := entry.Value.reify(makeSettable(target.Field(index)), ignoreFieldNames); err != nil {
			return err
		}
	}
	return nil
}

func extractStruct(t types.Type) *types.Struct {
	switch t := t.(type) {
	case *types.Struct:
		return t
	case *types.Pointer:
		return extractStruct(t.Elem())
	case *types.Named:
		return extractStruct(t.Underlying())
	default:
		panic(fmt.Sprintf("cannot extract struct from %v", t))
	}
}

func findSyntheticField(name string, target *types.Struct) (int, error) {
	// Synthetic structs will have field order that matches the
	// go/types struct, but maybe not names. So we must search the
	// go/types version to find the field index.
	for i := 0; i < target.NumFields(); i++ {
		if target.Field(i).Name() == name {
			return i, nil
		}
	}
	return 0, fmt.Errorf("struct has no field named %q", name)
}

func findField(name string, target reflect.Type) (int, error) {
	// Real structs (as opposed to those synthesized from go/types form)
	// could possibly differ from the go/types form in terms of field
	// order. So we search the actual reflect.Type for a field with a
	// matching name.
	for i := 0; i < target.NumField(); i++ {
		if target.Field(i).Name == name {
			return i, nil
		}
	}
	return 0, fmt.Errorf("struct has no field named %q", name)
}

func makeSettable(v reflect.Value) reflect.Value {
	// we cheat (using unsafe) to be able to set values of unexported fields
	if v.CanSet() {
		return v
	}
	addr := unsafe.Pointer(v.UnsafeAddr())
	return reflect.NewAt(v.Type(), addr)
}

func makeType(t types.Type) (reflect.Type, bool) {
	switch t := t.(type) {
	case *types.Interface:
		return typeOfEmptyInterface, t.NumMethods() == 0
	case *types.Named:
		typ, _ := makeType(t.Underlying())
		return typ, false
	case *types.Pointer:
		typ, ok := makeType(t.Elem())
		return reflect.PtrTo(typ), ok
	case *types.Basic:
		switch t.Kind() {
		case types.Int, types.UntypedInt:
			return reflect.TypeOf(int(0)), true
		case types.Int64:
			return reflect.TypeOf(int64(0)), true
		case types.Int32, types.UntypedRune:
			return reflect.TypeOf(int32(0)), true
		case types.Int16:
			return reflect.TypeOf(int16(0)), true
		case types.Int8:
			return reflect.TypeOf(int8(0)), true
		case types.Uint:
			return reflect.TypeOf(uint(0)), true
		case types.Uint64:
			return reflect.TypeOf(uint64(0)), true
		case types.Uint32:
			return reflect.TypeOf(uint32(0)), true
		case types.Uint16:
			return reflect.TypeOf(uint16(0)), true
		case types.Uint8:
			return reflect.TypeOf(uint8(0)), true
		case types.Uintptr:
			return reflect.TypeOf(uintptr(0)), true
		case types.Float64, types.UntypedFloat:
			return reflect.TypeOf(float64(0)), true
		case types.Float32:
			return reflect.TypeOf(float32(0)), true
		case types.Complex128, types.UntypedComplex:
			return reflect.TypeOf(complex128(0)), true
		case types.Complex64:
			return reflect.TypeOf(complex64(0)), true
		case types.String, types.UntypedString:
			return reflect.TypeOf(""), true
		case types.Bool, types.UntypedBool:
			return reflect.TypeOf(false), true
		case types.UntypedNil:
			return typeOfEmptyInterface, true
		case types.UnsafePointer:
			return reflect.TypeOf(unsafe.Pointer(nil)), true
		default:
			panic(fmt.Sprintf("unexpected basic kind: %v", t.Kind()))
		}
	case *types.Chan:
		typ, ok := makeType(t.Elem())
		var dir reflect.ChanDir
		switch t.Dir() {
		case types.RecvOnly:
			dir = reflect.RecvDir
		case types.SendOnly:
			dir = reflect.SendDir
		case types.SendRecv:
			dir = reflect.BothDir
		}
		return reflect.ChanOf(dir, typ), ok
	case *types.Signature:
		in, inOk := makeTypes(t.Params())
		out, outOk := makeTypes(t.Results())
		return reflect.FuncOf(in, out, t.Variadic()), inOk && outOk
	case *types.Slice:
		typ, ok := makeType(t.Elem())
		return reflect.SliceOf(typ), ok
	case *types.Array:
		typ, ok := makeType(t.Elem())
		return reflect.ArrayOf(int(t.Len()), typ), ok
	case *types.Map:
		ktyp, kOk := makeType(t.Key())
		vtyp, vOk := makeType(t.Elem())
		return reflect.MapOf(ktyp, vtyp), kOk && vOk
	case *types.Struct:
		return makeStruct(t)
	}
	panic(fmt.Sprintf("unexpected type: %T", t))
}

func makeTypes(t *types.Tuple) ([]reflect.Type, bool) {
	ret := make([]reflect.Type, t.Len())
	retOk := true
	for i := 0; i < t.Len(); i++ {
		typ, ok := makeType(t.At(i).Type())
		ret[i] = typ
		if !ok {
			retOk = false
		}
	}
	return ret, retOk
}

func makeStruct(st *types.Struct) (reflect.Type, bool) {
	flds := make([]reflect.StructField, st.NumFields())
	stOk := true
	names := map[string]struct{}{}
	for i := 0; i < st.NumFields(); i++ {
		f := st.Field(i)
		name := f.Name()

		if name == "" {
			// construct field name from the type
			fType := f.Type()
			if ptr, ok := fType.(*types.Pointer); ok {
				fType = ptr.Elem()
			}
			if nm, ok := fType.(*types.Named); ok {
				name = nm.Obj().Name()
			} else {
				// WTF? Embedded types should only be allowed to
				// be named types or pointer to named types.
				name = "_"
			}
		}

		if !f.Exported() {
			stOk = false
			name = export(name)
		}

		typ, tOk := makeType(f.Type())
		if !tOk {
			stOk = false
		}

		for {
			if _, ok := names[name]; !ok {
				// this name has not been used
				break
			}
			name = name + "_"
			stOk = false
		}

		anon := f.Anonymous()
		if anon && i != 0 {
			// reflect.StructOf gets unhappy when embedded types
			// with methods are not in position 0, so we just
			// make them not-anonymous
			anon = false
			stOk = false
		}

		flds[i] = reflect.StructField{
			Name:      name,
			Tag:       reflect.StructTag(st.Tag(i)),
			Type:      typ,
			Anonymous: anon,
		}
	}

	return reflect.StructOf(flds), stOk
}

func export(name string) string {
	r, sz := utf8.DecodeRuneInString(name)
	upperR := unicode.ToUpper(r)
	if upperR == r {
		// should only happen if r == '_'
		return "X" + name
	}
	return string(upperR) + name[sz:]
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

type annoType struct {
	packagePath, name string
}

// ErrMirroredFunction is an error that is caused by invoking a function whose
// underlying function cannot be executed because it is present only in source
// form. The source form, a *types.Func, can be retrieved using the AsFunc()
// method.
//
// When a value is reified from an AnnotationMirror or AnnotationValue, and that
// value is or contains a function reference, the reified function value, if
// called, will panic. The value recovered from that panic will be of type
// *ErrMirroredFunction.
type ErrMirroredFunction types.Func

// Error implements the error interface, returning a string that describes the
// error.
func (e *ErrMirroredFunction) Error() string {
	return fmt.Sprintf("cannot invoke mirrored function %s.%s", e.Pkg().Path(), e.Name())
}

// AsFunc returns the function that was referenced by the source of the error.
func (e *ErrMirroredFunction) AsFunc() *types.Func {
	return (*types.Func)(e)
}

// GetMirroredValue unwraps a MirroredValue from the given val. The returned
// bool is true on success. If val directly implemented MirroredValue, it is
// returned unchanged. If val is a synthetic value, that wraps a MirroredValue
// while also implementing some other interface, the MirroredValue therein is
// returned. Otherwise, the given value does not wrap a MirroredValue, and this
// function will return nil, false.
func GetMirroredValue(val interface{}) (MirroredValue, bool) {
	if mv, ok := val.(MirroredValue); ok {
		return mv, true
	}

	rv := reflect.ValueOf(val)
	rt := rv.Type()
	if rt.Name() != "" || rt.Kind() != reflect.Struct || rt.NumField() != 2 ||
		rt.Field(0).Type.Kind() != reflect.Interface || rt.Field(0).Name != "Interface" ||
		!rt.Field(0).Anonymous || rt.Field(0).Type.NumMethod() < 1 ||
		rt.Field(1).Type != typeOfMirroredVal || rt.Field(1).Name != "Value" {

		return nil, false
	}
	return rv.Field(1).Interface().(MirroredValue), true
}

// UnwrapMirror tries to unwrap a MirroredValue and return the raw value
// therein. If the given value does not wrap a MirroredVal, it is returned
// unchanged.
func UnwrapMirror(val interface{}) interface{} {
	mv, ok := GetMirroredValue(val)
	if ok {
		return mv.GetRawValue()
	}
	return val
}

// MirroredValue represents a "malformed" value, reified from a mirror. This
// happens when a mirror (e.g. AnnotationMirror or AnnotationValue) refers to a
// type that cannot be resolved to a runtime type. In that case, a MirroredValue
// is used instead. From the MirroredValue, the original AnnotationValue may be
// accessed as well as a "best effort" representation of the value, via the
// GetRawValue() method.
//
// If the raw value is a struct, it will have no unexported fields, regardless
// of whether the intended struct type has unexported fields. Typically,
// converting an unexported field name to an exported one just involves
// capitalizing the first letter. But if the unexported field name does not
// start with a letter, it will get an "X" prefix to make it exported. As
// necessary, field names may be changed further to avoid naming conflicts.
// Field names are changed in order (e.g. first field renamed first), and if
// the changes cause later field names to conflict with earlier field names, the
// later fields will have one or more underscores ("_") appended to their name,
// to ensure all fields have unique names.
type MirroredValue interface {
	// GetRawValue returns the underlying "raw" value.
	//
	// Where possible, it will have the same structure and underlying type as
	// the unresolved type found in GetAnnotationValue().Type, but will simply
	// be unnamed. This will not be possible if the underlying type includes
	// references to other unresolved types (such as a slice or map whose
	// elements are of unresolvable types, or a struct that has a field with an
	// unresolvable type). In those cases, all unresolvable types are replaced
	// with MirroredValue.
	GetRawValue() interface{}
	// GetAnnotationValue returns the AnnotationValue from which this mirrored
	// value came.
	GetAnnotationValue() AnnotationValue

	isMirroredVal()
}

type mirroredVal struct {
	av *AnnotationValue
	v  interface{}
}

var _ MirroredValue = (*mirroredVal)(nil)

func (m *mirroredVal) GetRawValue() interface{} {
	return m.v
}

func (m *mirroredVal) GetAnnotationValue() AnnotationValue {
	return *m.av
}

func (m *mirroredVal) isMirroredVal() {}
