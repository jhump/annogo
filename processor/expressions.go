package processor

import (
	"fmt"
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"math"
	"reflect"

	"github.com/jhump/annogo/parser"
)

var (
	typeInt   = types.Typ[types.Int]
	typeInt64 = types.Typ[types.Int64]
	typeInt32 = types.Typ[types.Int32]
	typeInt16 = types.Typ[types.Int16]
	typeInt8  = types.Typ[types.Int8]

	typeUintptr = types.Typ[types.Uintptr]
	typeUint    = types.Typ[types.Uint]
	typeUint64  = types.Typ[types.Uint64]
	typeUint32  = types.Typ[types.Uint32]
	typeUint16  = types.Typ[types.Uint16]
	typeUint8   = types.Typ[types.Uint8]

	typeFloat64    = types.Typ[types.Float64]
	typeFloat32    = types.Typ[types.Float32]
	typeComplex128 = types.Typ[types.Complex128]
	typeComplex64  = types.Typ[types.Complex64]

	typeBool   = types.Typ[types.Bool]
	typeString = types.Typ[types.String]

	typeUntypedInt     = types.Typ[types.UntypedInt]
	typeUntypedRune    = types.Typ[types.UntypedRune]
	typeUntypedFloat   = types.Typ[types.UntypedFloat]
	typeUntypedComplex = types.Typ[types.UntypedComplex]
	typeUntypedBool    = types.Typ[types.UntypedBool]
	typeUntypedString  = types.Typ[types.UntypedString]
	typeUntypedNil     = types.Typ[types.UntypedNil]
)

type funcConstant struct {
	constant.Value
	fn *types.Func
}

func (c *Context) getExpressionValue(file *ast.File, node parser.ExpressionNode, adjuster posAdjuster) (types.Type, interface{}, error) {
	switch node := node.(type) {
	case parser.AggregateNode:
		return nil, node.Contents, nil

	case parser.TypedExpressionNode:
		if agg, ok := node.Value.(parser.AggregateNode); ok {
			t, err := c.convertType(file, node.Type, adjuster)
			if err != nil {
				return nil, nil, err
			}
			return t, agg.Contents, nil
		}
	}

	t, v, err := c.determineConstantValue(file, node, adjuster)
	if err != nil {
		return nil, nil, err
	}
	val := getConstantValue(t, v)
	if isUntyped(t) {
		t = nil
	}
	return t, val, nil
}

func getConstantValue(t types.Type, v constant.Value) interface{} {
	if v == nil {
		return nil
	}
	if isInt(t) {
		if constant.Sign(v) < 0 {
			r, _ := constant.Int64Val(v)
			return r
		} else {
			r, _ := constant.Uint64Val(v)
			return r
		}
	}
	if isFloat(t) {
		if v.Kind() == constant.Unknown {
			return math.NaN()
		}
		r, _ := constant.Float64Val(v)
		return r
	}
	if isComplex(t) {
		rv := constant.Real(v)
		iv := constant.Imag(v)
		if rv.Kind() == constant.Unknown || iv.Kind() == constant.Unknown {
			return complex(math.NaN(), math.NaN())
		}
		r, _ := constant.Float64Val(rv)
		i, _ := constant.Float64Val(iv)
		return complex(r, i)
	}
	if isBool(t) {
		return constant.BoolVal(v)
	}
	if isString(t) {
		return constant.StringVal(v)
	}
	if fn, ok := v.(funcConstant); ok {
		return fn
	}
	panic(fmt.Sprintf("unrecognized type and value: %v, %v", t, v))
}

func (c *Context) determineConstantValue(file *ast.File, node parser.ExpressionNode, adjuster posAdjuster) (types.Type, constant.Value, error) {
	switch node := node.(type) {
	case parser.LiteralNode:
		if node.Val == nil {
			return typeUntypedNil, nil, nil
		}
		switch node.Val.Kind() {
		case constant.Int:
			return typeUntypedInt, node.Val, nil
		case constant.Float, constant.Unknown:
			// unknown kind == NaN
			return typeUntypedFloat, node.Val, nil
		case constant.Complex:
			return typeUntypedComplex, node.Val, nil
		case constant.String:
			return typeUntypedString, node.Val, nil
		case constant.Bool:
			return typeUntypedBool, node.Val, nil
		default:
			panic(fmt.Sprintf("unknown kind, %v, for constant value %v", node.Val.Kind(), node.Val))
		}

	case parser.RefNode:
		_, obj, err := c.resolveSymbol(file, node.Ident, adjuster)
		if err != nil {
			return nil, nil, err
		}
		if cnst, ok := obj.(*types.Const); ok {
			return obj.Type(), cnst.Val(), nil
		} else if fn, ok := obj.(*types.Func); ok {
			return obj.Type(), funcConstant{Value: constant.MakeUnknown(), fn: fn}, nil
		} else {
			pos := adjuster.adjustPosition(node.Ident.Pos)
			return nil, nil, posError(pos, fmt.Errorf("identifier must be a constant or a function"))
		}

	case parser.AggregateNode:
		pos := adjuster.adjustPosition(node.Pos())
		return nil, nil, posError(pos, fmt.Errorf("aggregate cannot be used in a constant expression"))

	case parser.TypedExpressionNode:
		vt, v, err := c.determineConstantValue(file, node.Value, adjuster)
		if err != nil {
			return nil, nil, err
		}
		t, err := c.convertType(file, node.Type, adjuster)
		if err != nil {
			return nil, nil, err
		}
		pos := adjuster.adjustPosition(node.Pos())
		if !types.ConvertibleTo(vt, t) {
			return nil, nil, posError(pos, fmt.Errorf("cannot convert %v to %v", vt, t))
		}
		basic, ok := t.Underlying().(*types.Basic)
		typePos := adjuster.adjustPosition(node.Type.Pos())
		if !ok {
			// it's a function or typed nil; no conversion to do
			return t, v, nil
		}
		if types.Identical(basic, vt.Underlying()) {
			// no overflow checks needed
			return t, v, nil
		}
		if basic.Kind() == types.Bool {
			// no conversions needed for bools since they are only
			// convertible to other bool types (e.g. no-op)
			return t, v, nil
		}

		var fn func(constant.Value, interface{}, token.Position) (constant.Value, error)
		var target interface{}
		switch basic.Kind() {
		case types.Int:
			fn = convertToInt
			target = int(0)
		case types.Int8:
			fn = convertToInt
			target = int8(0)
		case types.Int16:
			fn = convertToInt
			target = int16(0)
		case types.Int32:
			fn = convertToInt
			target = int32(0)
		case types.Int64:
			fn = convertToInt
			target = int64(0)
		case types.Uint:
			fn = convertToUint
			target = uint(0)
		case types.Uintptr:
			fn = convertToUint
			target = uintptr(0)
		case types.Uint8:
			fn = convertToUint
			target = uint8(0)
		case types.Uint16:
			fn = convertToUint
			target = uint16(0)
		case types.Uint32:
			fn = convertToUint
			target = uint32(0)
		case types.Uint64:
			fn = convertToUint
			target = uint64(0)
		case types.Float32:
			fn = convertToFloat
			target = float32(0)
		case types.Float64:
			fn = convertToFloat
			target = float64(0)
		case types.Complex64:
			fn = convertToComplex
			target = complex64(0)
		case types.Complex128:
			fn = convertToComplex
			target = complex128(0)
		case types.String:
			fn = convertToString
			target = ""
		default:
			return nil, nil, posError(typePos, fmt.Errorf("non-basic types cannot be used in a constant expression"))
		}
		rv, err := fn(v, target, pos)
		if err != nil {
			return nil, nil, err
		}
		return t, rv, nil

	case parser.BinaryOperatorNode:
		lt, lv, err := c.determineConstantValue(file, node.Left, adjuster)
		if err != nil {
			return nil, nil, err
		}
		rt, rv, err := c.determineConstantValue(file, node.Right, adjuster)
		if err != nil {
			return nil, nil, err
		}

		leftPos := adjuster.adjustPosition(node.Left.Pos())
		rightPos := adjuster.adjustPosition(node.Right.Pos())
		opPos := adjuster.adjustPosition(node.OperatorPos)

		if isNil(lt) {
			return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for nil values"))
		} else if isNil(rt) {
			return nil, nil, posError(rightPos, fmt.Errorf("operator not valid for nil values"))
		}
		if isNaN(lv) {
			return nil, nil, posError(leftPos, fmt.Errorf("operators not valid with NaN values"))
		} else if isNaN(rv) {
			return nil, nil, posError(rightPos, fmt.Errorf("operators not valid with NaN values"))
		}

		opTok := toToken(node.Operator)
		var tt types.Type
		if isShift(opTok) {
			tt = lt
		} else {
			if constAssignableTo(lt, rt) {
				tt = rt
			} else if constAssignableTo(rt, lt) {
				tt = lt
			}
		}
		if tt == nil {
			return nil, nil, posError(opPos, fmt.Errorf("incompatible types: %v and %v", lt, rt))
		}

		if _, ok := lt.Underlying().(*types.Basic); !ok {
			return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for %v values", lt))
		}

		var v constant.Value
		if isShift(opTok) {
			if !isInt(lt) {
				return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for %v values (only integers)", lt))
			}
			if !isUint(rt, rv) {
				return nil, nil, posError(rightPos, fmt.Errorf("shift operand must be uint"))
			}

			// bitwise shift operation
			sh64, ok := constant.Uint64Val(rv)
			if !ok {
				return nil, nil, posError(rightPos, fmt.Errorf("shift operand overflows uint: %v", rv))
			}
			sh := uint(sh64)
			if uint64(sh) != sh64 {
				return nil, nil, posError(rightPos, fmt.Errorf("shift operand overflows uint: %v", rv))
			}

			v = constant.Shift(lv, opTok, sh)

		} else if isComparison(opTok) {
			// comparison operation
			tt = typeUntypedBool

			if isOrderedComparison(opTok) {
				if isComplex(lt) || isBool(lt) {
					return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for %v values", lt))
				} else if isComplex(rt) || isBool(rt) {
					return nil, nil, posError(rightPos, fmt.Errorf("operator not valid for %v values", rt))
				}
			} else if isFunc(lv) || isFunc(rv) || lv == nil || rv == nil {
				// TODO
			}

			v = constant.MakeBool(constant.Compare(lv, opTok, rv))

		} else {
			if isFunc(lv) || lv == nil {
				return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for %v values", lt))
			} else if isFunc(rv) || rv == nil {
				return nil, nil, posError(rightPos, fmt.Errorf("operator not valid for %v values", rt))
			}

			// normal binary operation
			if opTok == token.QUO {
				if isZero(rv) {
					return nil, nil, posError(rightPos, fmt.Errorf("divide by zero not allowed"))
				}
				if isInt(tt) {
					opTok = token.QUO_ASSIGN
				}
			}
			if isIntOnlyOperation(opTok) {
				if !isInt(lt) {
					return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for %v values (only integers)", lt))
				} else if !isInt(rt) {
					return nil, nil, posError(rightPos, fmt.Errorf("operator not valid for %v values (only integers)", rt))
				}
			}
			if isLogicalOperation(opTok) {
				if !isBool(lt) {
					return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for %v values (only bools)", lt))
				} else if !isBool(rt) {
					return nil, nil, posError(rightPos, fmt.Errorf("operator not valid for %v values (only bools", rt))
				}
			}
			if isString(tt) && opTok != token.ADD {
				return nil, nil, posError(leftPos, fmt.Errorf("operator not valid for string values"))
			}

			v = constant.BinaryOp(lv, opTok, rv)

		}
		return tt, v, nil

	case parser.PrefixOperatorNode:
		t, v, err := c.determineConstantValue(file, node.Value, adjuster)
		if err != nil {
			return nil, nil, err
		}

		pos := adjuster.adjustPosition(node.Value.Pos())
		if isNaN(v) {
			return nil, nil, posError(pos, fmt.Errorf("operators not valid with NaN values"))
		}

		opTok := toToken(node.Operator)
		var res constant.Value
		switch opTok {
		case token.XOR:
			// bitwise negation
			if !isInt(t) {
				return nil, nil, posError(pos, fmt.Errorf("operator not valid for %v values (only integers)", t))
			}
			// ^x == -(x+1) in 2's complement
			v = constant.BinaryOp(constant.MakeInt64(1), token.ADD, v)
			fallthrough

		case token.SUB:
			// unary minus
			if !isNumber(t) {
				return nil, nil, posError(pos, fmt.Errorf("operator not valid for %v values (only integers)", t))
			}
			res = constant.BinaryOp(constant.MakeInt64(-1), token.MUL, v)

		case token.NOT:
			// logical not
			if !isBool(t) {
				return nil, nil, posError(pos, fmt.Errorf("operator not valid for %v values (only bools)", t))
			}
			res = constant.MakeBool(!constant.BoolVal(v))

		default:
			panic(fmt.Sprintf("unknown prefix operator %v", node.Operator))
		}
		return t, res, nil

	case parser.ParenthesizedExpressionNode:
		return c.determineConstantValue(file, node.Contents, adjuster)

	case parser.InvokeRealNode, parser.InvokeImagNode:
		var arg parser.ExpressionNode
		var fn func(constant.Value) constant.Value
		var name string
		if n, ok := node.(parser.InvokeRealNode); ok {
			arg = n.Argument
			fn = constant.Real
			name = "real"
		} else {
			arg = node.(parser.InvokeImagNode).Argument
			fn = constant.Imag
			name = "imag"
		}
		t, v, err := c.determineConstantValue(file, arg, adjuster)
		if err != nil {
			return nil, nil, err
		}
		pos := adjuster.adjustPosition(arg.Pos())
		if !isComplex(t) && !constAssignableTo(t, typeComplex64) && !constAssignableTo(t, typeComplex128) {
			return nil, nil, posError(pos, fmt.Errorf("invalid argument type for %s: %v", name, t))
		}
		res := fn(v)
		switch t.Underlying().(*types.Basic).Kind() {
		case types.Complex128:
			t = typeFloat64
		case types.Complex64:
			t = typeFloat32
		default:
			t = typeUntypedFloat
		}
		return t, res, nil

	case parser.InvokeComplexNode:
		rt, rv, err := c.determineConstantValue(file, node.RealArg, adjuster)
		if err != nil {
			return nil, nil, err
		}
		it, iv, err := c.determineConstantValue(file, node.ImagArg, adjuster)
		if err != nil {
			return nil, nil, err
		}

		pos := adjuster.adjustPosition(node.Pos())
		realPos := adjuster.adjustPosition(node.RealArg.Pos())
		imagPos := adjuster.adjustPosition(node.ImagArg.Pos())

		if !constAssignableTo(rt, it) && !constAssignableTo(rt, it) {
			return nil, nil, posError(pos, fmt.Errorf("incompatible types: %v and %v", rt, it))
		}
		if !isFloat(rt) && !constAssignableTo(rt, typeFloat64) && !constAssignableTo(rt, typeFloat32) {
			return nil, nil, posError(realPos, fmt.Errorf("invalid argument type for complex: %v", rt))
		} else if !isFloat(it) && !constAssignableTo(it, typeFloat64) && !constAssignableTo(it, typeFloat32) {
			return nil, nil, posError(imagPos, fmt.Errorf("invalid argument type for complex: %v", it))
		}

		if rv.Kind() == constant.Unknown {
			return nil, nil, posError(realPos, fmt.Errorf("invalid argument NaN for complex"))
		}
		if iv.Kind() == constant.Unknown {
			return nil, nil, posError(imagPos, fmt.Errorf("invalid argument NaN for complex"))
		}

		iv = constant.MakeImag(iv)
		res := constant.BinaryOp(rv, token.ADD, iv)
		var t types.Type
		switch rt.Underlying().(*types.Basic).Kind() {
		case types.Float64:
			t = typeComplex128
		case types.Float32:
			t = typeComplex64
		default:
			switch it.Underlying().(*types.Basic).Kind() {
			case types.Float64:
				t = typeComplex128
			case types.Float32:
				t = typeComplex64
			default:
				t = typeUntypedComplex
			}
		}
		return t, res, nil

	default:
		panic(fmt.Sprintf("unexpected kind of expression node: %T", node))
	}
}

func constAssignableTo(t1, t2 types.Type) bool {
	b1, ok := t1.(*types.Basic)
	if ok {
		b2, ok := t2.Underlying().(*types.Basic)
		if ok {
			k := b2.Kind()
			switch b1.Kind() {
			case types.UntypedInt, types.UntypedRune:
				if k == types.Int64 ||
					k == types.Int32 ||
					k == types.Int16 ||
					k == types.Int8 ||
					k == types.Int ||
					k == types.Uint64 ||
					k == types.Uint32 ||
					k == types.Uint16 ||
					k == types.Uint8 ||
					k == types.Uint ||
					k == types.Uintptr ||
					k == types.UntypedInt {
					return true
				}
				fallthrough

			case types.UntypedFloat:
				if k == types.Float64 ||
					k == types.Float32 ||
					k == types.UntypedFloat {
					return true
				}
				fallthrough

			case types.UntypedComplex:
				if k == types.Complex128 ||
					k == types.Complex64 ||
					k == types.UntypedComplex {
					return true
				}
			}
		}
	}
	return types.AssignableTo(t1, t2)
}

func isOrderedComparison(t token.Token) bool {
	return t == token.GTR || t == token.LSS || t == token.GEQ || t == token.LEQ
}

func isComparison(t token.Token) bool {
	return t == token.EQL || t == token.NEQ || isOrderedComparison(t)
}

func isShift(t token.Token) bool {
	return t == token.SHR || t == token.SHL
}

func isIntOnlyOperation(t token.Token) bool {
	return t == token.AND || t == token.OR || t == token.XOR || t == token.AND_NOT ||
		t == token.REM || isShift(t)
}

func isLogicalOperation(t token.Token) bool {
	return t == token.LAND || t == token.LOR || t == token.NOT
}

func isZero(v constant.Value) bool {
	switch v.Kind() {
	case constant.Int, constant.Float, constant.Complex:
		return constant.Sign(v) == 0
	default:
		return false
	}
}

func isNil(t types.Type) bool {
	b, ok := t.Underlying().(*types.Basic)
	return ok && b.Kind() == types.UntypedNil
}

func isInt(t types.Type) bool {
	b, ok := t.Underlying().(*types.Basic)
	if !ok {
		return false
	}
	switch b.Kind() {
	case types.UntypedInt, types.UntypedRune, types.Uintptr,
		types.Int, types.Int64, types.Int32, types.Int16, types.Int8,
		types.Uint, types.Uint64, types.Uint32, types.Uint16, types.Uint8:
		return true
	default:
		return false
	}
}

func isUint(t types.Type, v constant.Value) bool {
	b, ok := t.Underlying().(*types.Basic)
	if !ok {
		return false
	}
	k := b.Kind()
	if k == types.UntypedInt {
		return constant.Sign(v) >= 0
	}
	return k == types.Uintptr || k == types.Uint || k == types.Uint64 ||
		k == types.Uint32 || k == types.Uint16 || k == types.Uint8
}

func isString(t types.Type) bool {
	b, ok := t.Underlying().(*types.Basic)
	return ok && b.Kind() == types.UntypedString || b.Kind() == types.String
}

func isBool(t types.Type) bool {
	b, ok := t.Underlying().(*types.Basic)
	return ok && b.Kind() == types.UntypedBool || b.Kind() == types.Bool
}

func isFloat(t types.Type) bool {
	b, ok := t.Underlying().(*types.Basic)
	return ok && b.Kind() == types.UntypedFloat ||
		b.Kind() == types.Float64 || b.Kind() == types.Float32
}

func isComplex(t types.Type) bool {
	b, ok := t.Underlying().(*types.Basic)
	return ok && b.Kind() == types.UntypedComplex ||
		b.Kind() == types.Complex128 || b.Kind() == types.Complex64
}

func isNumber(t types.Type) bool {
	return isInt(t) || isFloat(t) || isComplex(t)
}

func toToken(op string) token.Token {
	switch op {
	case "==":
		return token.EQL
	case "!=":
		return token.NEQ
	case ">":
		return token.GTR
	case "<":
		return token.LSS
	case ">=":
		return token.GEQ
	case "<=":
		return token.LEQ
	case "&&":
		return token.LAND
	case "||":
		return token.LOR
	case "!":
		return token.NOT
	case "+":
		return token.ADD
	case "-":
		return token.SUB
	case "*":
		return token.MUL
	case "/":
		return token.QUO
	case "%":
		return token.REM
	case "&":
		return token.AND
	case "|":
		return token.OR
	case "^":
		return token.XOR
	case "&^":
		return token.AND_NOT
	case "<<":
		return token.SHL
	case ">>":
		return token.SHR
	default:
		panic(fmt.Sprintf("unrecognized operator %s", op))
	}
}

func isNaN(v constant.Value) bool {
	return v.Kind() == constant.Unknown && !isFunc(v)
}

func isFunc(v constant.Value) bool {
	_, isFunc := v.(funcConstant)
	return isFunc
}

func convertToInt(v constant.Value, t interface{}, pos token.Position) (constant.Value, error) {
	var src int64
	switch v.Kind() {
	case constant.Unknown:
		// shouldn't be a func since we should have already done convertibility check
		return nil, posError(pos, fmt.Errorf("cannot convert NaN to %T", t))
	case constant.Int:
		var ok bool
		src, ok = constant.Int64Val(v)
		if !ok {
			return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
		}
	case constant.Float:
		f, ok := constant.Float64Val(v)
		if !ok {
			return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
		}
		if f > math.MaxInt64 || f < math.MinInt64 {
			return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
		}
		src = int64(f)
	default:
		return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T", t))
	}

	rv := reflect.New(reflect.TypeOf(t)).Elem()
	rv.SetInt(src)
	roundTripped := rv.Int()
	if src != roundTripped {
		return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
	}
	return constant.MakeInt64(src), nil
}

func convertToUint(v constant.Value, t interface{}, pos token.Position) (constant.Value, error) {
	var src uint64
	switch v.Kind() {
	case constant.Unknown:
		// shouldn't be a func since we should have already done convertibility check
		return nil, posError(pos, fmt.Errorf("cannot convert NaN to %T", t))
	case constant.Int:
		var ok bool
		src, ok = constant.Uint64Val(v)
		if !ok {
			return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
		}
	case constant.Float:
		f, ok := constant.Float64Val(v)
		if !ok {
			return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
		}
		if f > math.MaxUint64 || f < 0 {
			return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
		}
		src = uint64(f)
	default:
		return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T", t))
	}

	rv := reflect.New(reflect.TypeOf(t)).Elem()
	rv.SetUint(src)
	roundTripped := rv.Uint()
	if src != roundTripped {
		return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
	}
	return constant.MakeUint64(src), nil
}

func convertToFloat(v constant.Value, t interface{}, pos token.Position) (constant.Value, error) {
	switch v.Kind() {
	case constant.Unknown:
		// shouldn't be a func since we should have already done convertibility check
		return nil, posError(pos, fmt.Errorf("cannot convert NaN to %T", t))
	case constant.Int, constant.Float:
		switch t.(type) {
		case float64:
			src, ok := constant.Float64Val(v)
			if !ok && math.IsInf(src, 0) {
				return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
			}
			return v, nil
		case float32:
			// get value as float32, to reduce to expected precision
			src, ok := constant.Float32Val(v)
			src64 := float64(src)
			if !ok && math.IsInf(src64, 0) {
				return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
			}
			return constant.MakeFloat64(src64), nil
		default:
			panic(fmt.Sprintf("invalid float type %T", t))
		}
	default:
		return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T", t))
	}
}

func convertToComplex(v constant.Value, t interface{}, pos token.Position) (constant.Value, error) {
	switch v.Kind() {
	case constant.Unknown:
		// shouldn't be a func since we should have already done convertibility check
		return nil, posError(pos, fmt.Errorf("cannot convert NaN to %T", t))
	case constant.Int, constant.Float:
		switch t.(type) {
		case float64:
			srcR, ok := constant.Float64Val(constant.Real(v))
			if !ok && math.IsInf(srcR, 0) {
				return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
			}
			srcI, ok := constant.Float64Val(constant.Imag(v))
			if !ok && math.IsInf(srcI, 0) {
				return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
			}
			return v, nil
		case float32:
			// get values as float32, to reduce to expected precision
			srcR, ok := constant.Float32Val(constant.Real(v))
			r64 := float64(srcR)
			if !ok && math.IsInf(r64, 0) {
				return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
			}
			srcI, ok := constant.Float32Val(constant.Imag(v))
			i64 := float64(srcI)
			if !ok && math.IsInf(i64, 0) {
				return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T because it overflows", t))
			}
			r := constant.MakeFloat64(r64)
			i := constant.MakeImag(constant.MakeFloat64(i64))
			return constant.BinaryOp(r, token.ADD, i), nil
		default:
			panic(fmt.Sprintf("invalid float type %T", t))
		}
	default:
		return nil, posError(pos, fmt.Errorf("cannot convert constant value to %T", t))
	}
}

func convertToString(v constant.Value, _ interface{}, pos token.Position) (constant.Value, error) {
	if v.Kind() == constant.String {
		return v, nil
	}
	if v.Kind() != constant.Int {
		return nil, posError(pos, fmt.Errorf("cannot convert constant value to string (integers only)"))
	}
	u64, ok := constant.Uint64Val(v)
	if !ok {
		// this is the value for conversion from numbers to string for values
		// that are not valid unicode points
		return constant.MakeString("\uFFFD"), nil
	}
	return constant.MakeString(string(u64)), nil
}

func isUntyped(t types.Type) bool {
	b, ok := t.(*types.Basic)
	return ok && (types.IsUntyped&b.Info()) != 0
}
