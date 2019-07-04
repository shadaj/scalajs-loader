// Linking info - must be in sync with scala.scalajs.runtime.LinkingInfo
var $linkingInfo = {
  "semantics": {




    "asInstanceOfs": 1,








    "arrayIndexOutOfBounds": 1,










    "moduleInit": 2,





    "strictFloats": false,




    "productionMode": false

  },



  "assumingES6": false,

  "linkerVersion": "1.0.0-M6",
  "globalThis": this
};
Object["freeze"]($linkingInfo);
Object["freeze"]($linkingInfo["semantics"]);

// Snapshots of builtins and polyfills






var $imul = Math["imul"] || (function(a, b) {
  // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
  var ah = (a >>> 16) & 0xffff;
  var al = a & 0xffff;
  var bh = (b >>> 16) & 0xffff;
  var bl = b & 0xffff;
  // the shift by 0 fixes the sign on the high part
  // the final |0 converts the unsigned value into a signed value
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0);
});

var $fround = Math["fround"] ||













































  (function(v) {
    return +v;
  });


var $clz32 = Math["clz32"] || (function(i) {
  // See Hacker's Delight, Section 5-3
  if (i === 0) return 32;
  var r = 1;
  if ((i & 0xffff0000) === 0) { i <<= 16; r += 16; };
  if ((i & 0xff000000) === 0) { i <<= 8; r += 8; };
  if ((i & 0xf0000000) === 0) { i <<= 4; r += 4; };
  if ((i & 0xc0000000) === 0) { i <<= 2; r += 2; };
  return r + (i >> 31);
});



// Cached instance of RuntimeLong for 0L
var $L0;


// identityHashCode support
var $lastIDHash = 0; // last value attributed to an id hash code



var $idHashCodeMap = typeof WeakMap !== "undefined" ? new WeakMap() : null;


// Core mechanism

function $makeIsArrayOfPrimitive(primitiveData) {
  return function(obj, depth) {
    return !!(obj && obj.$classData &&
      (obj.$classData.arrayDepth === depth) &&
      (obj.$classData.arrayBase === primitiveData));
  }
};


function $makeAsArrayOfPrimitive(isInstanceOfFunction, arrayEncodedName) {
  return function(obj, depth) {
    if (isInstanceOfFunction(obj, depth) || (obj === null))
      return obj;
    else
      $throwArrayCastException(obj, arrayEncodedName, depth);
  }
};


/** Encode a property name for runtime manipulation
  *  Usage:
  *    env.propertyName({someProp:0})
  *  Returns:
  *    "someProp"
  *  Useful when the property is renamed by a global optimizer (like Closure)
  *  but we must still get hold of a string of that name for runtime
  * reflection.
  */
function $propertyName(obj) {
  for (var prop in obj)
    return prop;
};

// Boxed Char











function $Char(c) {
  this.c = c;
};
$Char.prototype.toString = (function() {
  return String["fromCharCode"](this.c);
});


// Runtime functions

function $isScalaJSObject(obj) {
  return !!(obj && obj.$classData);
};


function $throwClassCastException(instance, classFullName) {
  



  throw new (require("./request.sjs?sjsr_UndefinedBehaviorError").$c_sjsr_UndefinedBehaviorError)().init___jl_Throwable(
    new (require("./request.sjs?jl_ClassCastException").$c_jl_ClassCastException)().init___T(
      instance + " is not an instance of " + classFullName));

};

function $throwArrayCastException(instance, classArrayEncodedName, depth) {
  for (; depth; --depth)
    classArrayEncodedName = "[" + classArrayEncodedName;
  $throwClassCastException(instance, classArrayEncodedName);
};



function $throwArrayIndexOutOfBoundsException(i) {
  var msg = (i === null) ? null : ("" + i);



  throw new $c_sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new $c_jl_ArrayIndexOutOfBoundsException().init___T(msg));

};


function $noIsInstance(instance) {
  throw new TypeError(
    "Cannot call isInstance() on a Class representing a raw JS trait/object");
};

function $makeNativeArrayWrapper(arrayClassData, nativeArray) {
  return new arrayClassData.constr(nativeArray);
};

function $newArrayObject(arrayClassData, lengths) {
  return $newArrayObjectInternal(arrayClassData, lengths, 0);
};

function $newArrayObjectInternal(arrayClassData, lengths, lengthIndex) {
  var result = new arrayClassData.constr(lengths[lengthIndex]);

  if (lengthIndex < lengths.length-1) {
    var subArrayClassData = arrayClassData.componentData;
    var subLengthIndex = lengthIndex+1;
    var underlying = result.u;
    for (var i = 0; i < underlying.length; i++) {
      underlying[i] = $newArrayObjectInternal(
        subArrayClassData, lengths, subLengthIndex);
    }
  }

  return result;
};

function $objectGetClass(instance) {
  switch (typeof instance) {
    case "string":
      return $d_T.getClassOf();
    case "number": {
      var v = instance | 0;
      if (v === instance) { // is the value integral?
        if ($isByte(v))
          return $d_jl_Byte.getClassOf();
        else if ($isShort(v))
          return $d_jl_Short.getClassOf();
        else
          return $d_jl_Integer.getClassOf();
      } else {
        if ($isFloat(instance))
          return $d_jl_Float.getClassOf();
        else
          return $d_jl_Double.getClassOf();
      }
    }
    case "boolean":
      return $d_jl_Boolean.getClassOf();
    case "undefined":
      return $d_sr_BoxedUnit.getClassOf();
    default:
      if (instance === null)
        return instance.getClass__jl_Class();

      else if (require("./request.sjs?sjsr_RuntimeLong").$is_sjsr_RuntimeLong(instance))



        return $d_jl_Long.getClassOf();
      else if ($isChar(instance))
        return $d_jl_Character.getClassOf();
      else if ($isScalaJSObject(instance))
        return instance.$classData.getClassOf();
      else
        return null; // Exception?
  }
};

function $dp_toString__T(instance) {
  if (instance === void 0)
    return "undefined";
  else
    return instance.toString();
};

function $dp_getClass__jl_Class(instance) {
  return $objectGetClass(instance);
};

function $dp_clone__O(instance) {
  if ($isScalaJSObject(instance) || (instance === null))
    return instance.clone__O();
  else
    throw new $c_jl_CloneNotSupportedException().init___();
};

function $dp_notify__V(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notify__V();
};

function $dp_notifyAll__V(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notifyAll__V();
};

function $dp_finalize__V(instance) {
  if ($isScalaJSObject(instance) || (instance === null))
    instance.finalize__V();
  // else no-op
};

function $dp_equals__O__Z(instance, rhs) {
  if ($isScalaJSObject(instance) || (instance === null))
    return instance.equals__O__Z(rhs);
  else if (typeof instance === "number")
    return $f_jl_Double__equals__O__Z(instance, rhs);




  else if ($isChar(instance))
    return $f_jl_Character__equals__O__Z(instance, rhs);
  else
    return instance === rhs;
};

function $dp_hashCode__I(instance) {
  switch (typeof instance) {
    case "string":
      return $f_T__hashCode__I(instance);
    case "number":
      return $f_jl_Double__hashCode__I(instance);
    case "boolean":
      return $f_jl_Boolean__hashCode__I(instance);
    case "undefined":
      return $f_sr_BoxedUnit__hashCode__I(instance);
    default:
      if ($isScalaJSObject(instance) || instance === null)
        return instance.hashCode__I();




      else if ($isChar(instance))
        return $f_jl_Character__hashCode__I(instance);
      else
        return $systemIdentityHashCode(instance);
  }
};

function $dp_compareTo__O__I(instance, rhs) {
  switch (typeof instance) {
    case "string":
      return $f_T__compareTo__O__I(instance, rhs);
    case "number":
      return $f_jl_Double__compareTo__O__I(instance, rhs);
    case "boolean":
      return $f_jl_Boolean__compareTo__O__I(instance, rhs);
    default:
      if ($isChar(instance))
        return $f_jl_Character__compareTo__O__I(instance, rhs);




      else
        return instance.compareTo__O__I(rhs);
  }
};

function $dp_length__I(instance) {
  if (typeof(instance) === "string")
    return $f_T__length__I(instance);
  else
    return instance.length__I();
};

function $dp_charAt__I__C(instance, index) {
  if (typeof(instance) === "string")
    return $f_T__charAt__I__C(instance, index);
  else
    return instance.charAt__I__C(index);
};

function $dp_subSequence__I__I__jl_CharSequence(instance, start, end) {
  if (typeof(instance) === "string")
    return $f_T__subSequence__I__I__jl_CharSequence(instance, start, end);
  else
    return instance.subSequence__I__I__jl_CharSequence(start, end);
};

function $dp_byteValue__B(instance) {
  if (typeof instance === "number")
    return $f_jl_Double__byteValue__B(instance);




  else
    return instance.byteValue__B();
};
function $dp_shortValue__S(instance) {
  if (typeof instance === "number")
    return $f_jl_Double__shortValue__S(instance);




  else
    return instance.shortValue__S();
};
function $dp_intValue__I(instance) {
  if (typeof instance === "number")
    return $f_jl_Double__intValue__I(instance);




  else
    return instance.intValue__I();
};
function $dp_longValue__J(instance) {
  if (typeof instance === "number")
    return $f_jl_Double__longValue__J(instance);




  else
    return instance.longValue__J();
};
function $dp_floatValue__F(instance) {
  if (typeof instance === "number")
    return $f_jl_Double__floatValue__F(instance);




  else
    return instance.floatValue__F();
};
function $dp_doubleValue__D(instance) {
  if (typeof instance === "number")
    return $f_jl_Double__doubleValue__D(instance);




  else
    return instance.doubleValue__D();
};

function $doubleToInt(x) {
  return (x > 2147483647) ? (2147483647) : ((x < -2147483648) ? -2147483648 : (x | 0));
};




















/** Instantiates a JS object with variadic arguments to the constructor. */
function $newJSObjectWithVarargs(ctor, args) {
  // This basically emulates the ECMAScript specification for 'new'.
  var instance = Object["create"](ctor.prototype);
  var result = ctor["apply"](instance, args);
  switch (typeof result) {
    case "string": case "number": case "boolean": case "undefined": case "symbol":
      return instance;
    default:
      return result === null ? instance : result;
  }
};

function $resolveSuperRef(superClass, propName) {
  var getPrototypeOf = Object["getPrototypeOf"];
  var getOwnPropertyDescriptor = Object["getOwnPropertyDescriptor"];

  var superProto = superClass.prototype;
  while (superProto !== null) {
    var desc = getOwnPropertyDescriptor(superProto, propName);
    if (desc !== void 0)
      return desc;
    superProto = getPrototypeOf(superProto);
  }

  return void 0;
};

function $superGet(superClass, self, propName) {
  var desc = $resolveSuperRef(superClass, propName);
  if (desc !== void 0) {
    var getter = desc["get"];
    if (getter !== void 0)
      return getter["call"](self);
    else
      return desc["value"];
  }
  return void 0;
};

function $superSet(superClass, self, propName, value) {
  var desc = $resolveSuperRef(superClass, propName);
  if (desc !== void 0) {
    var setter = desc["set"];
    if (setter !== void 0) {
      setter["call"](self, value);
      return void 0;
    }
  }
  throw new TypeError("super has no setter '" + propName + "'.");
};


function $moduleDefault(m) {
  return (m && (typeof m === "object") && "default" in m) ? m["default"] : m;
};


function $systemArraycopy(src, srcPos, dest, destPos, length) {
  var srcu = src.u;
  var destu = dest.u;


  if (srcPos < 0 || destPos < 0 || length < 0 ||
      (srcPos > ((srcu.length - length) | 0)) ||
      (destPos > ((destu.length - length) | 0))) {
    $throwArrayIndexOutOfBoundsException(null);
  }


  if (srcu !== destu || destPos < srcPos || (((srcPos + length) | 0) < destPos)) {
    for (var i = 0; i < length; i = (i + 1) | 0)
      destu[(destPos + i) | 0] = srcu[(srcPos + i) | 0];
  } else {
    for (var i = (length - 1) | 0; i >= 0; i = (i - 1) | 0)
      destu[(destPos + i) | 0] = srcu[(srcPos + i) | 0];
  }
};

var $systemIdentityHashCode =

  ($idHashCodeMap !== null) ?

  (function(obj) {
    switch (typeof obj) {
      case "string": case "number": case "bigint": case "boolean": case "undefined":
        return $dp_hashCode__I(obj);
      default:
        if (obj === null) {
          return 0;
        } else {
          var hash = $idHashCodeMap["get"](obj);
          if (hash === void 0) {
            hash = ($lastIDHash + 1) | 0;
            $lastIDHash = hash;
            $idHashCodeMap["set"](obj, hash);
          }
          return hash;
        }
    }

  }) :
  (function(obj) {
    switch (typeof obj) {
      case "string": case "number": case "bigint": case "boolean": case "undefined":
        return $dp_hashCode__I(obj);
      default:
        if ($isScalaJSObject(obj)) {
          var hash = obj["$idHashCode$0"];
          if (hash !== void 0) {
            return hash;
          } else if (!Object["isSealed"](obj)) {
            hash = ($lastIDHash + 1) | 0;
            $lastIDHash = hash;
            obj["$idHashCode$0"] = hash;
            return hash;
          } else {
            return 42;
          }
        } else if (obj === null) {
          return 0;
        } else {
          return 42;
        }
    }

  });

// is/as for hijacked boxed classes (the non-trivial ones)

function $isChar(v) {
  return v instanceof $Char;
};

function $isByte(v) {
  return typeof v === "number" && (v << 24 >> 24) === v && 1/v !== 1/-0;
};

function $isShort(v) {
  return typeof v === "number" && (v << 16 >> 16) === v && 1/v !== 1/-0;
};

function $isInt(v) {
  return typeof v === "number" && (v | 0) === v && 1/v !== 1/-0;
};







function $isFloat(v) {



  return typeof v === "number";

};


function $asUnit(v) {
  if (v === void 0 || v === null)
    return v;
  else
    $throwClassCastException(v, "scala.runtime.BoxedUnit");
};

function $asBoolean(v) {
  if (typeof v === "boolean" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Boolean");
};

function $asChar(v) {
  if (v instanceof $Char || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Character");
};

function $asByte(v) {
  if ($isByte(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Byte");
};

function $asShort(v) {
  if ($isShort(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Short");
};

function $asInt(v) {
  if ($isInt(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Integer");
};










function $asFloat(v) {
  if ($isFloat(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Float");
};

function $asDouble(v) {
  if (typeof v === "number" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Double");
};


// Boxes

function $bC(c) {
  return new $Char(c);
}
var $bC0 = $bC(0);

// Unboxes


function $uZ(value) {
  return !!$asBoolean(value);
};
function $uC(value) {
  return null === value ? 0 : $asChar(value).c;
};
function $uB(value) {
  return $asByte(value) | 0;
};
function $uS(value) {
  return $asShort(value) | 0;
};
function $uI(value) {
  return $asInt(value) | 0;
};
function $uJ(value) {



  return null === value ? $L0 : $as_sjsr_RuntimeLong(value);

};
function $uF(value) {
  /* Here, it is fine to use + instead of fround, because asFloat already
   * ensures that the result is either null or a float.
   */
  return +$asFloat(value);
};
function $uD(value) {
  return +$asDouble(value);
};













// TypeArray conversions

function $byteArray2TypedArray(value) { return new Int8Array(value.u); };
function $shortArray2TypedArray(value) { return new Int16Array(value.u); };
function $charArray2TypedArray(value) { return new Uint16Array(value.u); };
function $intArray2TypedArray(value) { return new Int32Array(value.u); };
function $floatArray2TypedArray(value) { return new Float32Array(value.u); };
function $doubleArray2TypedArray(value) { return new Float64Array(value.u); };

function $typedArray2ByteArray(value) {
  var arrayClassData = $d_B.getArrayOf();
  return new arrayClassData.constr(new Int8Array(value));
};
function $typedArray2ShortArray(value) {
  var arrayClassData = $d_S.getArrayOf();
  return new arrayClassData.constr(new Int16Array(value));
};
function $typedArray2CharArray(value) {
  var arrayClassData = $d_C.getArrayOf();
  return new arrayClassData.constr(new Uint16Array(value));
};
function $typedArray2IntArray(value) {
  var arrayClassData = $d_I.getArrayOf();
  return new arrayClassData.constr(new Int32Array(value));
};
function $typedArray2FloatArray(value) {
  var arrayClassData = $d_F.getArrayOf();
  return new arrayClassData.constr(new Float32Array(value));
};
function $typedArray2DoubleArray(value) {
  var arrayClassData = $d_D.getArrayOf();
  return new arrayClassData.constr(new Float64Array(value));
};

// TypeData class


/** @constructor */
function $TypeData() {




  // Runtime support
  this.constr = void 0;
  this.parentData = void 0;
  this.ancestors = null;
  this.componentData = null;
  this.arrayBase = null;
  this.arrayDepth = 0;
  this.zero = null;
  this.arrayEncodedName = "";
  this._classOf = void 0;
  this._arrayOf = void 0;
  this.isArrayOf = void 0;

  // java.lang.Class support
  this["name"] = "";
  this["isPrimitive"] = false;
  this["isInterface"] = false;
  this["isArrayClass"] = false;
  this["isRawJSType"] = false;
  this["isInstance"] = void 0;
};


$TypeData.prototype.initPrim = function(



    zero, arrayEncodedName, displayName) {
  // Runtime support
  this.ancestors = {};
  this.componentData = null;
  this.zero = zero;
  this.arrayEncodedName = arrayEncodedName;
  this.isArrayOf = function(obj, depth) { return false; };

  // java.lang.Class support
  this["name"] = displayName;
  this["isPrimitive"] = true;
  this["isInstance"] = function(obj) { return false; };

  return this;
};


$TypeData.prototype.initClass = function(



    internalNameObj, isInterface, fullName,
    ancestors, isRawJSType, parentData, isInstance, isArrayOf) {
  var internalName = $propertyName(internalNameObj);

  isInstance = isInstance || function(obj) {
    return !!(obj && obj.$classData && obj.$classData.ancestors[internalName]);
  };

  isArrayOf = isArrayOf || function(obj, depth) {
    return !!(obj && obj.$classData && (obj.$classData.arrayDepth === depth)
      && obj.$classData.arrayBase.ancestors[internalName])
  };

  // Runtime support
  this.parentData = parentData;
  this.ancestors = ancestors;
  this.arrayEncodedName = "L"+fullName+";";
  this.isArrayOf = isArrayOf;

  // java.lang.Class support
  this["name"] = fullName;
  this["isInterface"] = isInterface;
  this["isRawJSType"] = !!isRawJSType;
  this["isInstance"] = isInstance;

  return this;
};


$TypeData.prototype.initArray = function(



    componentData) {
  // The constructor




  // The zero for the Long runtime representation
  // is a special case here, since the class has not
  // been defined yet when this constructor is called.
  var componentZero0 = componentData.zero;
  var componentZero = (componentZero0 == "longZero") ? $L0 : componentZero0;



  /** @constructor */
  var ArrayClass = function(arg) {
    if (typeof(arg) === "number") {
      // arg is the length of the array
      this.u = new Array(arg);
      for (var i = 0; i < arg; i++)
        this.u[i] = componentZero;
    } else {
      // arg is a native array that we wrap
      this.u = arg;
    }
  }
  ArrayClass.prototype = new $h_O;
  ArrayClass.prototype.constructor = ArrayClass;


  ArrayClass.prototype.get = function(i) {
    if (i < 0 || i >= this.u.length)
      $throwArrayIndexOutOfBoundsException(i);
    return this.u[i];
  };
  ArrayClass.prototype.set = function(i, v) {
    if (i < 0 || i >= this.u.length)
      $throwArrayIndexOutOfBoundsException(i);
    this.u[i] = v;
  };


  ArrayClass.prototype.clone__O = function() {
    if (this.u instanceof Array)
      return new ArrayClass(this.u["slice"](0));
    else
      // The underlying Array is a TypedArray
      return new ArrayClass(new this.u.constructor(this.u));
  };






































  ArrayClass.prototype.$classData = this;

  // Don't generate reflective call proxies. The compiler special cases
  // reflective calls to methods on scala.Array

  // The data

  var encodedName = "[" + componentData.arrayEncodedName;
  var componentBase = componentData.arrayBase || componentData;
  var arrayDepth = componentData.arrayDepth + 1;

  var isInstance = function(obj) {
    return componentBase.isArrayOf(obj, arrayDepth);
  }

  // Runtime support
  this.constr = ArrayClass;
  this.parentData = require("./request.sjs?O").$d_O;
  this.ancestors = {O: 1, jl_Cloneable: 1, Ljava_io_Serializable: 1};
  this.componentData = componentData;
  this.arrayBase = componentBase;
  this.arrayDepth = arrayDepth;
  this.zero = null;
  this.arrayEncodedName = encodedName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isArrayOf = undefined;

  // java.lang.Class support
  this["name"] = encodedName;
  this["isPrimitive"] = false;
  this["isInterface"] = false;
  this["isArrayClass"] = true;
  this["isInstance"] = isInstance;

  return this;
};


$TypeData.prototype.getClassOf = function() {



  if (!this._classOf)
    this._classOf = new (require("./request.sjs?jl_Class").$c_jl_Class)(this);
  return this._classOf;
};


$TypeData.prototype.getArrayOf = function() {



  if (!this._arrayOf)
    this._arrayOf = new $TypeData().initArray(this);
  return this._arrayOf;
};

// java.lang.Class support


$TypeData.prototype["isAssignableFrom"] = function(that) {



  if (this["isPrimitive"] || that["isPrimitive"]) {
    return this === that;
  } else {
    var thatFakeInstance;
    if (that === $d_T)
      thatFakeInstance = "some string";
    else if (that === $d_jl_Boolean)
      thatFakeInstance = false;
    else if (that === $d_jl_Byte ||
             that === $d_jl_Short ||
             that === $d_jl_Integer ||
             that === $d_jl_Float ||
             that === $d_jl_Double)
      thatFakeInstance = 0;
    else if (that === $d_jl_Long)



      thatFakeInstance = $L0;

    else if (that === $d_sr_BoxedUnit)
      thatFakeInstance = void 0;
    else
      thatFakeInstance = {$classData: that};
    return this["isInstance"](thatFakeInstance);
  }
};


$TypeData.prototype["getSuperclass"] = function() {



  return this.parentData ? this.parentData.getClassOf() : null;
};


$TypeData.prototype["getComponentType"] = function() {



  return this.componentData ? this.componentData.getClassOf() : null;
};


$TypeData.prototype["newArrayOfThisClass"] = function(lengths) {



  var arrayClassData = this;
  for (var i = 0; i < lengths.length; i++)
    arrayClassData = arrayClassData.getArrayOf();
  return $newArrayObject(arrayClassData, lengths);
};




// Create primitive types

var $d_V = new $TypeData().initPrim(undefined, "V", "void");
var $d_Z = new $TypeData().initPrim(false, "Z", "boolean");
var $d_C = new $TypeData().initPrim(0, "C", "char");
var $d_B = new $TypeData().initPrim(0, "B", "byte");
var $d_S = new $TypeData().initPrim(0, "S", "short");
var $d_I = new $TypeData().initPrim(0, "I", "int");



var $d_J = new $TypeData().initPrim("longZero", "J", "long");

var $d_F = new $TypeData().initPrim(0.0, "F", "float");
var $d_D = new $TypeData().initPrim(0.0, "D", "double");

// Instance tests for array of primitives

var $isArrayOf_Z = $makeIsArrayOfPrimitive($d_Z);
$d_Z.isArrayOf = $isArrayOf_Z;

var $isArrayOf_C = $makeIsArrayOfPrimitive($d_C);
$d_C.isArrayOf = $isArrayOf_C;

var $isArrayOf_B = $makeIsArrayOfPrimitive($d_B);
$d_B.isArrayOf = $isArrayOf_B;

var $isArrayOf_S = $makeIsArrayOfPrimitive($d_S);
$d_S.isArrayOf = $isArrayOf_S;

var $isArrayOf_I = $makeIsArrayOfPrimitive($d_I);
$d_I.isArrayOf = $isArrayOf_I;

var $isArrayOf_J = $makeIsArrayOfPrimitive($d_J);
$d_J.isArrayOf = $isArrayOf_J;

var $isArrayOf_F = $makeIsArrayOfPrimitive($d_F);
$d_F.isArrayOf = $isArrayOf_F;

var $isArrayOf_D = $makeIsArrayOfPrimitive($d_D);
$d_D.isArrayOf = $isArrayOf_D;


// asInstanceOfs for array of primitives
var $asArrayOf_Z = $makeAsArrayOfPrimitive($isArrayOf_Z, "Z");
var $asArrayOf_C = $makeAsArrayOfPrimitive($isArrayOf_C, "C");
var $asArrayOf_B = $makeAsArrayOfPrimitive($isArrayOf_B, "B");
var $asArrayOf_S = $makeAsArrayOfPrimitive($isArrayOf_S, "S");
var $asArrayOf_I = $makeAsArrayOfPrimitive($isArrayOf_I, "I");
var $asArrayOf_J = $makeAsArrayOfPrimitive($isArrayOf_J, "J");
var $asArrayOf_F = $makeAsArrayOfPrimitive($isArrayOf_F, "F");
var $asArrayOf_D = $makeAsArrayOfPrimitive($isArrayOf_D, "D");

window.$TypeData = $TypeData;
window.$throwClassCastException = $throwClassCastException;
window.$asBoolean = $asBoolean;
window.$objectGetClass = $objectGetClass;
window.$uZ = $uZ;
window.$uI = $uI;

window.foo = require("sjs://Lme_shadaj_test_Test$");
window.foo.$m_Lme_shadaj_test_Test$().run__V()
