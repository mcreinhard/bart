// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = f;
}

function F(f) {
    this.f = f;
}

// Special object used for blackholing.
var __blackhole = {};

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        f = B(f);
        if(!(f instanceof Function)) {
            return f;
        }
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f != __blackhole) {
            var f = t.f;
            t.f = __blackhole;
            t.x = f();
        }
        return t.x;
    } else {
        return t;
    }
}

/* Bounce
   Bounce on a trampoline for as long as we get a function back.
*/
function B(f) {
    while(f instanceof F) {
        var fun = f.f;
        f = __blackhole;
        f = fun();
    }
    return f;
}

// Export Haste, A, B and E. Haste because we need to preserve exports, A, B
// and E because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;
window['B'] = B;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
if(Math.imul) {
    var imul = Math.imul;
} else {
    var imul = function(a, b) {
        // ignore high a * high a as the result will always be truncated
        var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
        var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
        var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
        return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
    }
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = B(A(f, [[0, str.charCodeAt(i)], acc]));
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = B(A(f, [mv.x]));
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['v']['i8'][0] = 'U'.charCodeAt(0);
    le['v']['i8'][1] = 'T'.charCodeAt(0);
    le['v']['i8'][2] = 'F'.charCodeAt(0);
    le['v']['i8'][3] = '-'.charCodeAt(0);
    le['v']['i8'][4] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return B(A(act,[0]));
    } catch(e) {
        return B(A(handler,[e, 0]));
    }
}

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - (e.currentTarget.offsetLeft || 0),
	    posy - (e.currentTarget.offsetTop || 0)];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                B(A(cb,[[0,k.keyCode],0]));
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,x.button],[0,mx,my],0]));
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            B(A(cb,[[0,mx,my],0]));
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {B(A(cb,[[0,x.keyCode],0]));};
        break;        
    default:
        fun = function() {B(A(cb,[0]));};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {B(A(cb,[0]));}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsQuerySelectorAll(elem, query) {
  var els = [0],
      len, nl, i;

  if (!elem || typeof elem.querySelectorAll !== 'function') {
    return els;
  }

  nl = elem.querySelectorAll(query);
  len = nl.length;

  for (i=len-1; i >= 0; --i) {
    els = [1, [0, nl[i]], els];
  }

  return els;
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetFirstChild(elem) {
    var len = elem.childNodes.length;
    for(var i = 0; i < len; i++) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}


function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

var jsJSONParse = JSON.parse;

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst_json(obj, 0)];
        } else if (obj == null) {
            return [5];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);

    if(method == "POST") {
        xhr.setRequestHeader("Content-type",
                             "application/x-www-form-urlencoded");
    }
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                B(A(cb,[[1,[0,xhr.responseText]],0]));
            } else {
                B(A(cb,[[0],0])); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {return x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {return x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {return x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=function(_1,_2){var _3=E(_1);return _3[0]==0?E(_2):[1,_3[1],new T(function(){return B(_0(_3[2],_2));})];},_4=function(_5,_6){var _7=jsShowI(_5),_8=_7;return new F(function(){return _0(fromJSStr(_8),_6);});},_9=[0,41],_a=[0,40],_b=function(_c,_d,_e){return _d>=0?B(_4(_d,_e)):_c<=6?B(_4(_d,_e)):[1,_a,new T(function(){var _f=jsShowI(_d),_g=_f;return B(_0(fromJSStr(_g),[1,_9,_e]));})];},_h=[0],_i=function(_j){var _k=E(_j);if(!_k[0]){return [0];}else{return new F(function(){return _0(_k[1],new T(function(){return B(_i(_k[2]));}));});}},_l=function(_m,_n){var _o=E(_n);return _o[0]==0?[0]:[1,_m,[1,_o[1],new T(function(){return B(_l(_m,_o[2]));})]];},_p=[0,41],_q=[1,_p,_h],_r=[0,44],_s=[1,_r,_h],_t=function(_u){var _v=B(A(_u,[_])),_w=_v;return E(_w);},_x=function(_y){return new F(function(){return _t(function(_){var _=0;return new F(function(){return eval(E(_y)[1]);});});});},_z=function(_A,_B,_C,_D,_){var _E=B(A(_x,[new T(function(){return [0,toJSStr(B(unAppCStr("Raphael(",new T(function(){return B(_0(B(_i([1,new T(function(){return B(_b(0,E(_A)[1],_h));}),new T(function(){return B(_l(_s,[1,new T(function(){return B(_b(0,E(_B)[1],_h));}),[1,new T(function(){return B(_b(0,E(_C)[1],_h));}),[1,new T(function(){return B(_b(0,E(_D)[1],_h));}),_h]]]));})])),_q));}))))];}),_])),_F=_E;return [0,_F];},_G=0,_H=function(_I,_J,_){var _K=jsWriteHandle(E(_I)[1],toJSStr(E(_J)));return _G;},_L=[0,10],_M=[1,_L,_h],_N=function(_O,_P,_){var _Q=E(_O),_R=jsWriteHandle(_Q[1],toJSStr(E(_P)));return new F(function(){return _H(_Q,_M,_);});},_S=function(_T,_){while(1){var _U=E(_T);if(!_U[0]){return _G;}else{var _V=_U[2],_W=E(_U[1]);switch(_W[0]){case 0:var _X=B(A(_W[1],[_])),_Y=_X;_T=B(_0(_V,[1,_Y,_h]));continue;case 1:_T=B(_0(_V,_W[1]));continue;default:_T=_V;continue;}}}},_Z=new T(function(){return B(unCStr(")})"));}),_10=[1,_r,_h],_11=function(_12,_13,_14,_15){return function(_16,_){var _17=B(A(new T(function(){return B(_x(new T(function(){return [0,toJSStr(B(unAppCStr("(function(paper){paper.setViewBox(",new T(function(){return B(_0(B(_i([1,new T(function(){return B(_b(0,E(_12)[1],_h));}),new T(function(){return B(_l(_10,[1,new T(function(){return B(_b(0,E(_13)[1],_h));}),[1,new T(function(){return B(_b(0,E(_14)[1],_h));}),[1,new T(function(){return B(_b(0,E(_15)[1],_h));}),_h]]]));})])),_Z));}))))];})));}),[E(E(_16)[1]),_])),_18=_17;return [0,_18];};},_19=[0,42],_1a=[0,64],_1b=[0,-2],_1c=new T(function(){return B(_11(_1b,_1b,_1a,_19));}),_1d=[2],_1e=function(_1f,_1g){return E(_1g);},_1h=function(_1i,_1j){return new F(function(){return _1e(_1i,_1j);});},_1k=function(_1l,_1m){return new F(function(){return A(_1m,[_1l]);});},_1n=function(_1o){return new F(function(){return err(_1o);});},_1p=function(_1q){return E(_1q);},_1r=[0,_1k,_1h,_1p,_1n],_1s=new T(function(){return B(unCStr("Prelude.(!!): negative index\n"));}),_1t=new T(function(){return B(err(_1s));}),_1u=new T(function(){return B(unCStr("Prelude.(!!): index too large\n"));}),_1v=new T(function(){return B(err(_1u));}),_1w=function(_1x,_1y){while(1){var _1z=E(_1x);if(!_1z[0]){return E(_1v);}else{var _1A=E(_1y);if(!_1A){return E(_1z[1]);}else{_1x=_1z[2];_1y=_1A-1|0;continue;}}}},_1B=new T(function(){return B(unCStr("ACK"));}),_1C=new T(function(){return B(unCStr("BEL"));}),_1D=new T(function(){return B(unCStr("BS"));}),_1E=new T(function(){return B(unCStr("SP"));}),_1F=[1,_1E,_h],_1G=new T(function(){return B(unCStr("US"));}),_1H=[1,_1G,_1F],_1I=new T(function(){return B(unCStr("RS"));}),_1J=[1,_1I,_1H],_1K=new T(function(){return B(unCStr("GS"));}),_1L=[1,_1K,_1J],_1M=new T(function(){return B(unCStr("FS"));}),_1N=[1,_1M,_1L],_1O=new T(function(){return B(unCStr("ESC"));}),_1P=[1,_1O,_1N],_1Q=new T(function(){return B(unCStr("SUB"));}),_1R=[1,_1Q,_1P],_1S=new T(function(){return B(unCStr("EM"));}),_1T=[1,_1S,_1R],_1U=new T(function(){return B(unCStr("CAN"));}),_1V=[1,_1U,_1T],_1W=new T(function(){return B(unCStr("ETB"));}),_1X=[1,_1W,_1V],_1Y=new T(function(){return B(unCStr("SYN"));}),_1Z=[1,_1Y,_1X],_20=new T(function(){return B(unCStr("NAK"));}),_21=[1,_20,_1Z],_22=new T(function(){return B(unCStr("DC4"));}),_23=[1,_22,_21],_24=new T(function(){return B(unCStr("DC3"));}),_25=[1,_24,_23],_26=new T(function(){return B(unCStr("DC2"));}),_27=[1,_26,_25],_28=new T(function(){return B(unCStr("DC1"));}),_29=[1,_28,_27],_2a=new T(function(){return B(unCStr("DLE"));}),_2b=[1,_2a,_29],_2c=new T(function(){return B(unCStr("SI"));}),_2d=[1,_2c,_2b],_2e=new T(function(){return B(unCStr("SO"));}),_2f=[1,_2e,_2d],_2g=new T(function(){return B(unCStr("CR"));}),_2h=[1,_2g,_2f],_2i=new T(function(){return B(unCStr("FF"));}),_2j=[1,_2i,_2h],_2k=new T(function(){return B(unCStr("VT"));}),_2l=[1,_2k,_2j],_2m=new T(function(){return B(unCStr("LF"));}),_2n=[1,_2m,_2l],_2o=new T(function(){return B(unCStr("HT"));}),_2p=[1,_2o,_2n],_2q=[1,_1D,_2p],_2r=[1,_1C,_2q],_2s=[1,_1B,_2r],_2t=new T(function(){return B(unCStr("ENQ"));}),_2u=[1,_2t,_2s],_2v=new T(function(){return B(unCStr("EOT"));}),_2w=[1,_2v,_2u],_2x=new T(function(){return B(unCStr("ETX"));}),_2y=[1,_2x,_2w],_2z=new T(function(){return B(unCStr("STX"));}),_2A=[1,_2z,_2y],_2B=new T(function(){return B(unCStr("SOH"));}),_2C=[1,_2B,_2A],_2D=new T(function(){return B(unCStr("NUL"));}),_2E=[1,_2D,_2C],_2F=[0,92],_2G=new T(function(){return B(unCStr("\\DEL"));}),_2H=new T(function(){return B(unCStr("\\a"));}),_2I=new T(function(){return B(unCStr("\\\\"));}),_2J=new T(function(){return B(unCStr("\\SO"));}),_2K=new T(function(){return B(unCStr("\\r"));}),_2L=new T(function(){return B(unCStr("\\f"));}),_2M=new T(function(){return B(unCStr("\\v"));}),_2N=new T(function(){return B(unCStr("\\n"));}),_2O=new T(function(){return B(unCStr("\\t"));}),_2P=new T(function(){return B(unCStr("\\b"));}),_2Q=function(_2R,_2S){if(_2R<=127){var _2T=E(_2R);switch(_2T){case 92:return new F(function(){return _0(_2I,_2S);});break;case 127:return new F(function(){return _0(_2G,_2S);});break;default:if(_2T<32){var _2U=E(_2T);switch(_2U){case 7:return new F(function(){return _0(_2H,_2S);});break;case 8:return new F(function(){return _0(_2P,_2S);});break;case 9:return new F(function(){return _0(_2O,_2S);});break;case 10:return new F(function(){return _0(_2N,_2S);});break;case 11:return new F(function(){return _0(_2M,_2S);});break;case 12:return new F(function(){return _0(_2L,_2S);});break;case 13:return new F(function(){return _0(_2K,_2S);});break;case 14:return new F(function(){return _0(_2J,new T(function(){var _2V=E(_2S);if(!_2V[0]){var _2W=[0];}else{var _2W=E(E(_2V[1])[1])==72?B(unAppCStr("\\&",_2V)):E(_2V);}return _2W;}));});break;default:return new F(function(){return _0([1,_2F,new T(function(){var _2X=_2U;return _2X>=0?B(_1w(_2E,_2X)):E(_1t);})],_2S);});}}else{return [1,[0,_2T],_2S];}}}else{return [1,_2F,new T(function(){var _2Y=jsShowI(_2R),_2Z=_2Y;return B(_0(fromJSStr(_2Z),new T(function(){var _30=E(_2S);if(!_30[0]){var _31=[0];}else{var _32=E(_30[1])[1],_31=_32<48?E(_30):_32>57?E(_30):B(unAppCStr("\\&",_30));}return _31;})));})];}},_33=[0,39],_34=[1,_33,_h],_35=new T(function(){return B(unCStr("\'\\\'\'"));}),_36=function(_37){var _38=E(E(_37)[1]);return _38==39?E(_35):[1,_33,new T(function(){return B(_2Q(_38,_34));})];},_39=[0,34],_3a=new T(function(){return B(unCStr("\\\""));}),_3b=function(_3c,_3d){var _3e=E(_3c);if(!_3e[0]){return E(_3d);}else{var _3f=_3e[2],_3g=E(E(_3e[1])[1]);if(_3g==34){return new F(function(){return _0(_3a,new T(function(){return B(_3b(_3f,_3d));}));});}else{return new F(function(){return _2Q(_3g,new T(function(){return B(_3b(_3f,_3d));}));});}}},_3h=function(_3i,_3j){return [1,_39,new T(function(){return B(_3b(_3i,[1,_39,_3j]));})];},_3k=function(_3l){return new F(function(){return _0(_35,_3l);});},_3m=function(_3n,_3o){var _3p=E(E(_3o)[1]);return _3p==39?E(_3k):function(_3q){return [1,_33,new T(function(){return B(_2Q(_3p,[1,_33,_3q]));})];};},_3r=[0,_3m,_36,_3h],_3s=function(_3t){return E(E(_3t)[1]);},_3u=[0,E(_h)],_3v=[1,_3u,_h],_3w=function(_3x,_3y,_3z,_3A,_3B,_3C,_3D){return new F(function(){return A(_3s,[_3x,new T(function(){return B(A(_3y,[_3z]));}),function(_3E){var _3F=E(_3E);if(!_3F[0]){return E(new T(function(){return B(A(_3D,[[0,E(_3A),_3v]]));}));}else{var _3G=E(_3F[1]);return new F(function(){return A(_3C,[_3G[1],[0,_3G[2],E(_3A),E(_3B)],[0,E(_3A),_h]]);});}}]);});},_3H=function(_3I,_3J){while(1){var _3K=E(_3I);if(!_3K[0]){return E(_3J)[0]==0?1:0;}else{var _3L=E(_3J);if(!_3L[0]){return 2;}else{var _3M=E(_3K[1])[1],_3N=E(_3L[1])[1];if(_3M!=_3N){return _3M>_3N?2:0;}else{_3I=_3K[2];_3J=_3L[2];continue;}}}}},_3O=new T(function(){return B(_0(_h,_h));}),_3P=function(_3Q,_3R,_3S,_3T){var _3U=function(_){var _3V=E(_3R);if(!_3V[0]){var _3W=E(_3T);if(!_3W[0]){var _3X=E(_3Q),_3Y=E(_3X[2])[1],_3Z=E(_3X[3])[1],_40=E(_3S),_41=E(_40[2])[1],_42=E(_40[3])[1];switch(B(_3H(_3X[1],_40[1]))){case 0:return [0,_40,_h];case 1:return _3Y>=_41?_3Y!=_41?[0,_3X,_h]:_3Z>=_42?_3Z!=_42?[0,_3X,_h]:[0,_3X,_3O]:[0,_40,_h]:[0,_40,_h];default:return [0,_3X,_h];}}else{return [0,_3S,_3W];}}else{var _43=E(_3Q),_44=E(_43[2])[1],_45=E(_43[3])[1],_46=E(_3S),_47=E(_46[2])[1],_48=E(_46[3])[1];switch(B(_3H(_43[1],_46[1]))){case 0:return [0,_46,_3T];case 1:return _44>=_47?_44!=_47?[0,_43,_3V]:_45>=_48?_45!=_48?[0,_43,_3V]:[0,_43,new T(function(){return B(_0(_3V,_3T));})]:[0,_46,_3T]:[0,_46,_3T];default:return [0,_43,_3V];}}};if(!E(_3T)[0]){var _49=E(_3R);return _49[0]==0?B(_3U(_)):[0,_3Q,_49];}else{return new F(function(){return _3U(_);});}},_4a=function(_4b,_4c){var _4d=E(_4b),_4e=E(_4c),_4f=B(_3P(_4d[1],_4d[2],_4e[1],_4e[2]));return [0,E(_4f[1]),_4f[2]];},_4g=function(_4h,_4i,_4j,_4k,_4l,_4m,_4n){return new F(function(){return A(_4h,[_4j,_4k,_4l,_4m,function(_4o){return new F(function(){return A(_4i,[_4j,_4k,_4l,function(_4p,_4q,_4r){return new F(function(){return A(_4m,[_4p,_4q,new T(function(){return B(_4a(_4o,_4r));})]);});},function(_4s){return new F(function(){return A(_4n,[new T(function(){return B(_4a(_4o,_4s));})]);});}]);});}]);});},_4t=function(_4u,_4v,_4w,_4x,_4y){return new F(function(){return A(_4x,[_G,_4u,new T(function(){return [0,E(E(_4u)[2]),_h];})]);});},_4z=function(_4A){return E(E(_4A)[2]);},_4B=function(_4C,_4D,_4E,_4F,_4G,_4H){var _4I=function(_4J){return function(_4K,_4L,_4M,_4N,_4O){return new F(function(){return A(_4O,[new T(function(){return [0,E(E(_4K)[2]),[1,new T(function(){return [1,E(B(A(new T(function(){return B(_4z(_4C));}),[_4J])))];}),_h]];})]);});};};return new F(function(){return _4g(function(_4P,_4Q,_4R,_4S,_4T){return new F(function(){return A(_4D,[_4P,function(_4U,_4V,_4W){return new F(function(){return A(_4I,[_4U,_4V,_4Q,_4R,function(_4X,_4Y,_4Z){return new F(function(){return A(_4Q,[_4X,_4Y,new T(function(){return B(_4a(_4W,_4Z));})]);});},function(_50){return new F(function(){return A(_4R,[new T(function(){return B(_4a(_4W,_50));})]);});}]);});},_4T,function(_51,_52,_53){return new F(function(){return A(_4I,[_51,_52,_4Q,_4R,function(_54,_55,_56){return new F(function(){return A(_4S,[_54,_55,new T(function(){return B(_4a(_53,_56));})]);});},function(_57){return new F(function(){return A(_4T,[new T(function(){return B(_4a(_53,_57));})]);});}]);});},_4T]);});},_4t,_4E,_4F,_4H,_4G,_4H);});},_58=function(_59){return [2,E(E(_59))];},_5a=function(_5b,_5c){switch(E(_5b)[0]){case 0:switch(E(_5c)[0]){case 0:return false;case 1:return true;case 2:return true;default:return true;}break;case 1:switch(E(_5c)[0]){case 0:return true;case 1:return false;case 2:return true;default:return true;}break;case 2:switch(E(_5c)[0]){case 0:return true;case 1:return true;case 2:return false;default:return true;}break;default:switch(E(_5c)[0]){case 0:return true;case 1:return true;case 2:return true;default:return false;}}},_5d=[2,E(_h)],_5e=function(_5f){return new F(function(){return _5a(_5d,_5f);});},_5g=function(_5h,_5i){while(1){var _5j=(function(_5k,_5l){var _5m=E(_5l);if(!_5m[0]){return [0];}else{var _5n=_5m[1],_5o=_5m[2];if(!B(A(_5k,[_5n]))){var _5p=_5k;_5i=_5o;_5h=_5p;return null;}else{return [1,_5n,new T(function(){return B(_5g(_5k,_5o));})];}}})(_5h,_5i);if(_5j!=null){return _5j;}}},_5q=function(_5r,_5s,_5t){var _5u=E(_5t);if(!_5u[0]){return [0,_5r,[1,_5d,new T(function(){return B(_5g(_5e,_5s));})]];}else{var _5v=_5u[1],_5w=E(_5u[2]);if(!_5w[0]){var _5x=new T(function(){return [2,E(E(_5v))];});return [0,_5r,[1,_5x,new T(function(){return B(_5g(function(_5f){return new F(function(){return _5a(_5x,_5f);});},_5s));})]];}else{var _5y=new T(function(){return [2,E(E(_5v))];}),_5z=function(_5A){var _5B=E(_5A);if(!_5B[0]){return [0,_5r,[1,_5y,new T(function(){return B(_5g(function(_5f){return new F(function(){return _5a(_5y,_5f);});},_5s));})]];}else{var _5C=B(_5z(_5B[2]));return [0,_5C[1],[1,new T(function(){return B(_58(_5B[1]));}),_5C[2]]];}};return new F(function(){return (function(_5D,_5E){var _5F=B(_5z(_5E));return [0,_5F[1],[1,new T(function(){return B(_58(_5D));}),_5F[2]]];})(_5w[1],_5w[2]);});}}},_5G=function(_5H,_5I){var _5J=E(_5H),_5K=B(_5q(_5J[1],_5J[2],_5I));return [0,E(_5K[1]),_5K[2]];},_5L=function(_5M,_5N,_5O,_5P,_5Q,_5R,_5S){return new F(function(){return A(_5M,[_5O,_5P,_5Q,function(_5T,_5U,_5V){return new F(function(){return A(_5R,[_5T,_5U,new T(function(){var _5W=E(_5V),_5X=E(_5W[2]);if(!_5X[0]){var _5Y=E(_5W);}else{var _5Z=B(_5q(_5W[1],_5X,_5N)),_5Y=[0,E(_5Z[1]),_5Z[2]];}var _60=_5Y;return _60;})]);});},function(_61){return new F(function(){return A(_5S,[new T(function(){return B(_5G(_61,_5N));})]);});}]);});},_62=new T(function(){return B(unCStr("end of input"));}),_63=[1,_62,_h],_64=function(_65,_66,_67,_68,_69,_6a,_6b,_6c){return new F(function(){return _5L(function(_6d,_6e,_6f,_6g,_6h){return new F(function(){return _4B(_67,function(_6i,_6j,_6k,_6l,_6m){var _6n=E(_6i);return new F(function(){return _3w(_65,_66,_6n[1],_6n[2],_6n[3],_6j,_6m);});},_6d,_6e,_6g,_6h);});},_63,_68,_69,_6a,_6b,_6c);});},_6o=function(_6p,_6q){while(1){var _6r=E(_6q);if(!_6r[0]){return 0;}else{var _6s=E(_6r[1]);_6q=_6r[2];continue;}}},_6t=function(_6u){return E(E(_6u)[1]);},_6v=function(_6w,_6x){var _6y=E(_6x),_6z=B(A(_6t,[_6w,_6y[1]]));return new F(function(){return _6o(new T(function(){return B(_6A(_6w));}),_6y[2]);});},_6B=function(_6C,_6D){while(1){var _6E=E(_6D);if(!_6E[0]){return 0;}else{var _6F=B(A(_6C,[_6E[1]]));_6D=_6E[2];continue;}}},_6G=function(_6H,_6I){var _6J=E(_6I),_6K=B(A(_6t,[_6H,_6J[1]]));return new F(function(){return _6B(function(_6L){return new F(function(){return _6v(_6H,_6L);});},_6J[2]);});},_6A=function(_6M){return [0,function(_6L){return new F(function(){return _6v(_6M,_6L);});},function(_6L){return new F(function(){return _6G(_6M,_6L);});}];},_6N=new T(function(){return B(_6A(_6O));}),_6P=function(_6Q){var _6R=E(_6Q);return 0;},_6S=[0,_6P,_6P],_6T=function(_6U){var _6V=E(_6U);return 0;},_6W=function(_6X){return new F(function(){return _6T(_6X);});},_6Y=function(_6Z){while(1){var _70=E(_6Z);if(!_70[0]){return 0;}else{var _71=E(_70[1]),_72=B(_6o(_6S,_71[1])),_73=B(_6o(_6S,_71[2]));_6Z=_70[2];continue;}}},_74=function(_75){var _76=E(_75);switch(_76[0]){case 0:return new F(function(){return _6o(_6S,_76[1]);});break;case 1:return 0;case 2:var _77=E(_76[1]);return 0;case 3:return new F(function(){return _6o(_6S,_76[1]);});break;case 4:return new F(function(){return _6o(_6S,_76[1]);});break;case 5:return new F(function(){return _6o(_6S,_76[1]);});break;case 6:var _78=E(_76[1]);return new F(function(){return _6o(_6N,_76[2]);});break;case 7:var _79=E(_76[1]);return new F(function(){return _6o(_6N,_76[2]);});break;case 8:var _7a=E(_76[1]);return new F(function(){return _6Y(_76[2]);});break;case 9:return new F(function(){return _6W(_76[1]);});break;default:var _7b=E(_76[1]);return new F(function(){return _6o(_6S,_76[2]);});}},_6O=new T(function(){return [0,_74,_74];}),_7c=new T(function(){return B(_6A(_6O));}),_7d=[0,58],_7e=new T(function(){return B(unCStr("Text.ParserCombinators.Parsec.Prim.many: combinator \'many\' is applied to a parser that accepts an empty string."));}),_7f=new T(function(){return B(err(_7e));}),_7g=function(_7h,_7i,_7j,_7k,_7l){var _7m=function(_7n,_7o){return new F(function(){return A(_7h,[_7o,new T(function(){var _7p=E(_7n);return function(_7q,_7r,_7s){return new F(function(){return _7m(_h,_7r);});};}),_7k,_7f,function(_7t){return new F(function(){return A(_7j,[_G,_7o,new T(function(){var _7u=E(_7t),_7v=B(_3P(_7u[1],_7u[2],E(_7o)[2],_h));return [0,E(_7v[1]),_7v[2]];})]);});}]);});};return new F(function(){return A(_7h,[_7i,function(_7w,_7x,_7y){return new F(function(){return _7m(_h,_7x);});},_7k,_7f,function(_7z){return new F(function(){return A(_7l,[_G,_7i,new T(function(){var _7A=E(_7z),_7B=B(_3P(_7A[1],_7A[2],E(_7i)[2],_h));return [0,E(_7B[1]),_7B[2]];})]);});}]);});},_7C=function(_7D,_7E){return E(_7D)[1]!=E(_7E)[1];},_7F=function(_7G,_7H){return E(_7G)[1]==E(_7H)[1];},_7I=[0,_7F,_7C],_7J=[1,_39,_h],_7K=[0,E(_h)],_7L=[1,_7K,_h],_7M=[0,95],_7N=[0,_7M,_7M],_7O=[1,_7N,_h],_7P=[0,382],_7Q=[0,330],_7R=[0,_7Q,_7P],_7S=[0,451],_7T=[0,384],_7U=[0,_7T,_7S],_7V=[0,2529],_7W=[0,2527],_7X=[0,_7W,_7V],_7Y=[0,2545],_7Z=[0,2544],_80=[0,_7Z,_7Y],_81=[0,2570],_82=[0,2565],_83=[0,_82,_81],_84=[0,2576],_85=[0,2575],_86=[0,_85,_84],_87=[0,2600],_88=[0,2579],_89=[0,_88,_87],_8a=[0,2608],_8b=[0,2602],_8c=[0,_8b,_8a],_8d=[0,2611],_8e=[0,2610],_8f=[0,_8e,_8d],_8g=[0,2614],_8h=[0,2613],_8i=[0,_8h,_8g],_8j=[0,2617],_8k=[0,2616],_8l=[0,_8k,_8j],_8m=[0,2652],_8n=[0,2649],_8o=[0,_8n,_8m],_8p=[0,2654],_8q=[0,_8p,_8p],_8r=[0,2676],_8s=[0,2674],_8t=[0,_8s,_8r],_8u=[0,2699],_8v=[0,2693],_8w=[0,_8v,_8u],_8x=[0,2701],_8y=[0,_8x,_8x],_8z=[0,2705],_8A=[0,2703],_8B=[0,_8A,_8z],_8C=[0,2728],_8D=[0,2707],_8E=[0,_8D,_8C],_8F=[0,2736],_8G=[0,2730],_8H=[0,_8G,_8F],_8I=[0,2739],_8J=[0,2738],_8K=[0,_8J,_8I],_8L=[0,2745],_8M=[0,2741],_8N=[0,_8M,_8L],_8O=[0,2749],_8P=[0,_8O,_8O],_8Q=[0,2784],_8R=[0,_8Q,_8Q],_8S=[0,2828],_8T=[0,2821],_8U=[0,_8T,_8S],_8V=[0,2832],_8W=[0,2831],_8X=[0,_8W,_8V],_8Y=[0,2856],_8Z=[0,2835],_90=[0,_8Z,_8Y],_91=[0,2864],_92=[0,2858],_93=[0,_92,_91],_94=[0,2867],_95=[0,2866],_96=[0,_95,_94],_97=[0,2873],_98=[0,2870],_99=[0,_98,_97],_9a=[0,2877],_9b=[0,_9a,_9a],_9c=[0,2909],_9d=[0,2908],_9e=[0,_9d,_9c],_9f=[0,2913],_9g=[0,2911],_9h=[0,_9g,_9f],_9i=[0,2954],_9j=[0,2949],_9k=[0,_9j,_9i],_9l=[0,2960],_9m=[0,2958],_9n=[0,_9m,_9l],_9o=[0,2965],_9p=[0,2962],_9q=[0,_9p,_9o],_9r=[0,2970],_9s=[0,2969],_9t=[0,_9s,_9r],_9u=[0,2972],_9v=[0,_9u,_9u],_9w=[0,2975],_9x=[0,2974],_9y=[0,_9x,_9w],_9z=[0,2980],_9A=[0,2979],_9B=[0,_9A,_9z],_9C=[0,2986],_9D=[0,2984],_9E=[0,_9D,_9C],_9F=[0,2997],_9G=[0,2990],_9H=[0,_9G,_9F],_9I=[0,3001],_9J=[0,2999],_9K=[0,_9J,_9I],_9L=[0,3084],_9M=[0,3077],_9N=[0,_9M,_9L],_9O=[0,55203],_9P=[0,44032],_9Q=[0,_9P,_9O],_9R=[1,_9Q,_h],_9S=[0,12588],_9T=[0,12549],_9U=[0,_9T,_9S],_9V=[1,_9U,_9R],_9W=[0,12538],_9X=[0,12449],_9Y=[0,_9X,_9W],_9Z=[1,_9Y,_9V],_a0=[0,12436],_a1=[0,12353],_a2=[0,_a1,_a0],_a3=[1,_a2,_9Z],_a4=[0,8578],_a5=[0,8576],_a6=[0,_a5,_a4],_a7=[1,_a6,_a3],_a8=[0,8494],_a9=[0,_a8,_a8],_aa=[1,_a9,_a7],_ab=[0,8491],_ac=[0,8490],_ad=[0,_ac,_ab],_ae=[1,_ad,_aa],_af=[0,8486],_ag=[0,_af,_af],_ah=[1,_ag,_ae],_ai=[0,8188],_aj=[0,8182],_ak=[0,_aj,_ai],_al=[1,_ak,_ah],_am=[0,8180],_an=[0,8178],_ao=[0,_an,_am],_ap=[1,_ao,_al],_aq=[0,8172],_ar=[0,8160],_as=[0,_ar,_aq],_at=[1,_as,_ap],_au=[0,8155],_av=[0,8150],_aw=[0,_av,_au],_ax=[1,_aw,_at],_ay=[0,8147],_az=[0,8144],_aA=[0,_az,_ay],_aB=[1,_aA,_ax],_aC=[0,8140],_aD=[0,8134],_aE=[0,_aD,_aC],_aF=[1,_aE,_aB],_aG=[0,8132],_aH=[0,8130],_aI=[0,_aH,_aG],_aJ=[1,_aI,_aF],_aK=[0,8126],_aL=[0,_aK,_aK],_aM=[1,_aL,_aJ],_aN=[0,8124],_aO=[0,8118],_aP=[0,_aO,_aN],_aQ=[1,_aP,_aM],_aR=[0,8116],_aS=[0,8064],_aT=[0,_aS,_aR],_aU=[1,_aT,_aQ],_aV=[0,8061],_aW=[0,8031],_aX=[0,_aW,_aV],_aY=[1,_aX,_aU],_aZ=[0,8029],_b0=[0,_aZ,_aZ],_b1=[1,_b0,_aY],_b2=[0,8027],_b3=[0,_b2,_b2],_b4=[1,_b3,_b1],_b5=[0,8025],_b6=[0,_b5,_b5],_b7=[1,_b6,_b4],_b8=[0,8023],_b9=[0,8016],_ba=[0,_b9,_b8],_bb=[1,_ba,_b7],_bc=[0,8013],_bd=[0,8008],_be=[0,_bd,_bc],_bf=[1,_be,_bb],_bg=[0,8005],_bh=[0,7968],_bi=[0,_bh,_bg],_bj=[1,_bi,_bf],_bk=[0,7965],_bl=[0,7960],_bm=[0,_bl,_bk],_bn=[1,_bm,_bj],_bo=[0,7957],_bp=[0,7936],_bq=[0,_bp,_bo],_br=[1,_bq,_bn],_bs=[0,7929],_bt=[0,7840],_bu=[0,_bt,_bs],_bv=[1,_bu,_br],_bw=[0,7835],_bx=[0,7680],_by=[0,_bx,_bw],_bz=[1,_by,_bv],_bA=[0,4601],_bB=[0,_bA,_bA],_bC=[1,_bB,_bz],_bD=[0,4592],_bE=[0,_bD,_bD],_bF=[1,_bE,_bC],_bG=[0,4587],_bH=[0,_bG,_bG],_bI=[1,_bH,_bF],_bJ=[0,4546],_bK=[0,4540],_bL=[0,_bK,_bJ],_bM=[1,_bL,_bI],_bN=[0,4538],_bO=[0,_bN,_bN],_bP=[1,_bO,_bM],_bQ=[0,4536],_bR=[0,4535],_bS=[0,_bR,_bQ],_bT=[1,_bS,_bP],_bU=[0,4527],_bV=[0,4526],_bW=[0,_bV,_bU],_bX=[1,_bW,_bT],_bY=[0,4523],_bZ=[0,_bY,_bY],_c0=[1,_bZ,_bX],_c1=[0,4520],_c2=[0,_c1,_c1],_c3=[1,_c2,_c0],_c4=[0,4510],_c5=[0,_c4,_c4],_c6=[1,_c5,_c3],_c7=[0,4469],_c8=[0,_c7,_c7],_c9=[1,_c8,_c6],_ca=[0,4467],_cb=[0,4466],_cc=[0,_cb,_ca],_cd=[1,_cc,_c9],_ce=[0,4462],_cf=[0,4461],_cg=[0,_cf,_ce],_ch=[1,_cg,_cd],_ci=[0,4457],_cj=[0,_ci,_ci],_ck=[1,_cj,_ch],_cl=[0,4455],_cm=[0,_cl,_cl],_cn=[1,_cm,_ck],_co=[0,4453],_cp=[0,_co,_co],_cq=[1,_cp,_cn],_cr=[0,4451],_cs=[0,_cr,_cr],_ct=[1,_cs,_cq],_cu=[0,4449],_cv=[0,4447],_cw=[0,_cv,_cu],_cx=[1,_cw,_ct],_cy=[0,4441],_cz=[0,_cy,_cy],_cA=[1,_cz,_cx],_cB=[0,4437],_cC=[0,4436],_cD=[0,_cC,_cB],_cE=[1,_cD,_cA],_cF=[0,4432],_cG=[0,_cF,_cF],_cH=[1,_cG,_cE],_cI=[0,4430],_cJ=[0,_cI,_cI],_cK=[1,_cJ,_cH],_cL=[0,4428],_cM=[0,_cL,_cL],_cN=[1,_cM,_cK],_cO=[0,4416],_cP=[0,_cO,_cO],_cQ=[1,_cP,_cN],_cR=[0,4414],_cS=[0,_cR,_cR],_cT=[1,_cS,_cQ],_cU=[0,4412],_cV=[0,_cU,_cU],_cW=[1,_cV,_cT],_cX=[0,4370],_cY=[0,4366],_cZ=[0,_cY,_cX],_d0=[1,_cZ,_cW],_d1=[0,4364],_d2=[0,4363],_d3=[0,_d2,_d1],_d4=[1,_d3,_d0],_d5=[0,4361],_d6=[0,_d5,_d5],_d7=[1,_d6,_d4],_d8=[0,4359],_d9=[0,4357],_da=[0,_d9,_d8],_db=[1,_da,_d7],_dc=[0,4355],_dd=[0,4354],_de=[0,_dd,_dc],_df=[1,_de,_db],_dg=[0,4352],_dh=[0,_dg,_dg],_di=[1,_dh,_df],_dj=[0,4342],_dk=[0,4304],_dl=[0,_dk,_dj],_dm=[1,_dl,_di],_dn=[0,4293],_do=[0,4256],_dp=[0,_do,_dn],_dq=[1,_dp,_dm],_dr=[0,3945],_ds=[0,3913],_dt=[0,_ds,_dr],_du=[1,_dt,_dq],_dv=[0,3911],_dw=[0,3904],_dx=[0,_dw,_dv],_dy=[1,_dx,_du],_dz=[0,3780],_dA=[0,3776],_dB=[0,_dA,_dz],_dC=[1,_dB,_dy],_dD=[0,3773],_dE=[0,_dD,_dD],_dF=[1,_dE,_dC],_dG=[0,3763],_dH=[0,3762],_dI=[0,_dH,_dG],_dJ=[1,_dI,_dF],_dK=[0,3760],_dL=[0,_dK,_dK],_dM=[1,_dL,_dJ],_dN=[0,3758],_dO=[0,3757],_dP=[0,_dO,_dN],_dQ=[1,_dP,_dM],_dR=[0,3755],_dS=[0,3754],_dT=[0,_dS,_dR],_dU=[1,_dT,_dQ],_dV=[0,3751],_dW=[0,_dV,_dV],_dX=[1,_dW,_dU],_dY=[0,3749],_dZ=[0,_dY,_dY],_e0=[1,_dZ,_dX],_e1=[0,3747],_e2=[0,3745],_e3=[0,_e2,_e1],_e4=[1,_e3,_e0],_e5=[0,3743],_e6=[0,3737],_e7=[0,_e6,_e5],_e8=[1,_e7,_e4],_e9=[0,3735],_ea=[0,3732],_eb=[0,_ea,_e9],_ec=[1,_eb,_e8],_ed=[0,3725],_ee=[0,_ed,_ed],_ef=[1,_ee,_ec],_eg=[0,3722],_eh=[0,_eg,_eg],_ei=[1,_eh,_ef],_ej=[0,3720],_ek=[0,3719],_el=[0,_ek,_ej],_em=[1,_el,_ei],_en=[0,3716],_eo=[0,_en,_en],_ep=[1,_eo,_em],_eq=[0,3714],_er=[0,3713],_es=[0,_er,_eq],_et=[1,_es,_ep],_eu=[0,3653],_ev=[0,3648],_ew=[0,_ev,_eu],_ex=[1,_ew,_et],_ey=[0,3635],_ez=[0,3634],_eA=[0,_ez,_ey],_eB=[1,_eA,_ex],_eC=[0,3632],_eD=[0,_eC,_eC],_eE=[1,_eD,_eB],_eF=[0,3630],_eG=[0,3585],_eH=[0,_eG,_eF],_eI=[1,_eH,_eE],_eJ=[0,3425],_eK=[0,3424],_eL=[0,_eK,_eJ],_eM=[1,_eL,_eI],_eN=[0,3385],_eO=[0,3370],_eP=[0,_eO,_eN],_eQ=[1,_eP,_eM],_eR=[0,3368],_eS=[0,3346],_eT=[0,_eS,_eR],_eU=[1,_eT,_eQ],_eV=[0,3344],_eW=[0,3342],_eX=[0,_eW,_eV],_eY=[1,_eX,_eU],_eZ=[0,3340],_f0=[0,3333],_f1=[0,_f0,_eZ],_f2=[1,_f1,_eY],_f3=[0,3297],_f4=[0,3296],_f5=[0,_f4,_f3],_f6=[1,_f5,_f2],_f7=[0,3294],_f8=[0,_f7,_f7],_f9=[1,_f8,_f6],_fa=[0,3257],_fb=[0,3253],_fc=[0,_fb,_fa],_fd=[1,_fc,_f9],_fe=[0,3251],_ff=[0,3242],_fg=[0,_ff,_fe],_fh=[1,_fg,_fd],_fi=[0,3240],_fj=[0,3218],_fk=[0,_fj,_fi],_fl=[1,_fk,_fh],_fm=[0,3216],_fn=[0,3214],_fo=[0,_fn,_fm],_fp=[1,_fo,_fl],_fq=[0,3212],_fr=[0,3205],_fs=[0,_fr,_fq],_ft=[1,_fs,_fp],_fu=[0,3169],_fv=[0,3168],_fw=[0,_fv,_fu],_fx=[1,_fw,_ft],_fy=[0,3129],_fz=[0,3125],_fA=[0,_fz,_fy],_fB=[1,_fA,_fx],_fC=[0,3123],_fD=[0,3114],_fE=[0,_fD,_fC],_fF=[1,_fE,_fB],_fG=[0,3112],_fH=[0,3090],_fI=[0,_fH,_fG],_fJ=[1,_fI,_fF],_fK=[0,3088],_fL=[0,3086],_fM=[0,_fL,_fK],_fN=[1,_fM,_fJ],_fO=[1,_9N,_fN],_fP=[1,_9K,_fO],_fQ=[1,_9H,_fP],_fR=[1,_9E,_fQ],_fS=[1,_9B,_fR],_fT=[1,_9y,_fS],_fU=[1,_9v,_fT],_fV=[1,_9t,_fU],_fW=[1,_9q,_fV],_fX=[1,_9n,_fW],_fY=[1,_9k,_fX],_fZ=[1,_9h,_fY],_g0=[1,_9e,_fZ],_g1=[1,_9b,_g0],_g2=[1,_99,_g1],_g3=[1,_96,_g2],_g4=[1,_93,_g3],_g5=[1,_90,_g4],_g6=[1,_8X,_g5],_g7=[1,_8U,_g6],_g8=[1,_8R,_g7],_g9=[1,_8P,_g8],_ga=[1,_8N,_g9],_gb=[1,_8K,_ga],_gc=[1,_8H,_gb],_gd=[1,_8E,_gc],_ge=[1,_8B,_gd],_gf=[1,_8y,_ge],_gg=[1,_8w,_gf],_gh=[1,_8t,_gg],_gi=[1,_8q,_gh],_gj=[1,_8o,_gi],_gk=[1,_8l,_gj],_gl=[1,_8i,_gk],_gm=[1,_8f,_gl],_gn=[1,_8c,_gm],_go=[1,_89,_gn],_gp=[1,_86,_go],_gq=[1,_83,_gp],_gr=[1,_80,_gq],_gs=[1,_7X,_gr],_gt=[0,2525],_gu=[0,2524],_gv=[0,_gu,_gt],_gw=[1,_gv,_gs],_gx=[0,2489],_gy=[0,2486],_gz=[0,_gy,_gx],_gA=[1,_gz,_gw],_gB=[0,2482],_gC=[0,_gB,_gB],_gD=[1,_gC,_gA],_gE=[0,2480],_gF=[0,2474],_gG=[0,_gF,_gE],_gH=[1,_gG,_gD],_gI=[0,2472],_gJ=[0,2451],_gK=[0,_gJ,_gI],_gL=[1,_gK,_gH],_gM=[0,2448],_gN=[0,2447],_gO=[0,_gN,_gM],_gP=[1,_gO,_gL],_gQ=[0,2444],_gR=[0,2437],_gS=[0,_gR,_gQ],_gT=[1,_gS,_gP],_gU=[0,2401],_gV=[0,2392],_gW=[0,_gV,_gU],_gX=[1,_gW,_gT],_gY=[0,2365],_gZ=[0,_gY,_gY],_h0=[1,_gZ,_gX],_h1=[0,2361],_h2=[0,2309],_h3=[0,_h2,_h1],_h4=[1,_h3,_h0],_h5=[0,1766],_h6=[0,1765],_h7=[0,_h6,_h5],_h8=[1,_h7,_h4],_h9=[0,1749],_ha=[0,_h9,_h9],_hb=[1,_ha,_h8],_hc=[0,1747],_hd=[0,1744],_he=[0,_hd,_hc],_hf=[1,_he,_hb],_hg=[0,1742],_hh=[0,1728],_hi=[0,_hh,_hg],_hj=[1,_hi,_hf],_hk=[0,1726],_hl=[0,1722],_hm=[0,_hl,_hk],_hn=[1,_hm,_hj],_ho=[0,1719],_hp=[0,1649],_hq=[0,_hp,_ho],_hr=[1,_hq,_hn],_hs=[0,1610],_ht=[0,1601],_hu=[0,_ht,_hs],_hv=[1,_hu,_hr],_hw=[0,1594],_hx=[0,1569],_hy=[0,_hx,_hw],_hz=[1,_hy,_hv],_hA=[0,1522],_hB=[0,1520],_hC=[0,_hB,_hA],_hD=[1,_hC,_hz],_hE=[0,1514],_hF=[0,1488],_hG=[0,_hF,_hE],_hH=[1,_hG,_hD],_hI=[0,1414],_hJ=[0,1377],_hK=[0,_hJ,_hI],_hL=[1,_hK,_hH],_hM=[0,1369],_hN=[0,_hM,_hM],_hO=[1,_hN,_hL],_hP=[0,1366],_hQ=[0,1329],_hR=[0,_hQ,_hP],_hS=[1,_hR,_hO],_hT=[0,1273],_hU=[0,1272],_hV=[0,_hU,_hT],_hW=[1,_hV,_hS],_hX=[0,1269],_hY=[0,1262],_hZ=[0,_hY,_hX],_i0=[1,_hZ,_hW],_i1=[0,1259],_i2=[0,1232],_i3=[0,_i2,_i1],_i4=[1,_i3,_i0],_i5=[0,1228],_i6=[0,1227],_i7=[0,_i6,_i5],_i8=[1,_i7,_i4],_i9=[0,1224],_ia=[0,1223],_ib=[0,_ia,_i9],_ic=[1,_ib,_i8],_id=[0,1220],_ie=[0,1168],_if=[0,_ie,_id],_ig=[1,_if,_ic],_ih=[0,1153],_ii=[0,1118],_ij=[0,_ii,_ih],_ik=[1,_ij,_ig],_il=[0,1116],_im=[0,1105],_in=[0,_im,_il],_io=[1,_in,_ik],_ip=[0,1103],_iq=[0,1038],_ir=[0,_iq,_ip],_is=[1,_ir,_io],_it=[0,1036],_iu=[0,1025],_iv=[0,_iu,_it],_iw=[1,_iv,_is],_ix=[0,1011],_iy=[0,994],_iz=[0,_iy,_ix],_iA=[1,_iz,_iw],_iB=[0,992],_iC=[0,_iB,_iB],_iD=[1,_iC,_iA],_iE=[0,990],_iF=[0,_iE,_iE],_iG=[1,_iF,_iD],_iH=[0,988],_iI=[0,_iH,_iH],_iJ=[1,_iI,_iG],_iK=[0,986],_iL=[0,_iK,_iK],_iM=[1,_iL,_iJ],_iN=[0,982],_iO=[0,976],_iP=[0,_iO,_iN],_iQ=[1,_iP,_iM],_iR=[0,974],_iS=[0,931],_iT=[0,_iS,_iR],_iU=[1,_iT,_iQ],_iV=[0,929],_iW=[0,910],_iX=[0,_iW,_iV],_iY=[1,_iX,_iU],_iZ=[0,908],_j0=[0,_iZ,_iZ],_j1=[1,_j0,_iY],_j2=[0,906],_j3=[0,904],_j4=[0,_j3,_j2],_j5=[1,_j4,_j1],_j6=[0,902],_j7=[0,_j6,_j6],_j8=[1,_j7,_j5],_j9=[0,705],_ja=[0,699],_jb=[0,_ja,_j9],_jc=[1,_jb,_j8],_jd=[0,680],_je=[0,592],_jf=[0,_je,_jd],_jg=[1,_jf,_jc],_jh=[0,535],_ji=[0,506],_jj=[0,_ji,_jh],_jk=[1,_jj,_jg],_jl=[0,501],_jm=[0,500],_jn=[0,_jm,_jl],_jo=[1,_jn,_jk],_jp=[0,496],_jq=[0,461],_jr=[0,_jq,_jp],_js=[1,_jr,_jo],_jt=[1,_7U,_js],_ju=[1,_7R,_jt],_jv=[0,328],_jw=[0,321],_jx=[0,_jw,_jv],_jy=[1,_jx,_ju],_jz=[0,318],_jA=[0,308],_jB=[0,_jA,_jz],_jC=[1,_jB,_jy],_jD=[0,305],_jE=[0,248],_jF=[0,_jE,_jD],_jG=[1,_jF,_jC],_jH=[0,246],_jI=[0,216],_jJ=[0,_jI,_jH],_jK=[1,_jJ,_jG],_jL=[0,214],_jM=[0,192],_jN=[0,_jM,_jL],_jO=[1,_jN,_jK],_jP=[0,122],_jQ=[0,97],_jR=[0,_jQ,_jP],_jS=[1,_jR,_jO],_jT=[0,90],_jU=[0,65],_jV=[0,_jU,_jT],_jW=[1,_jV,_jS],_jX=[0,40869],_jY=[0,19968],_jZ=[0,_jY,_jX],_k0=[1,_jZ,_h],_k1=[0,12329],_k2=[0,12321],_k3=[0,_k2,_k1],_k4=[1,_k3,_k0],_k5=[0,12295],_k6=[0,_k5,_k5],_k7=[1,_k6,_k4],_k8=new T(function(){return B(unCStr("Prelude.Enum.Char.pred: bad argument"));}),_k9=new T(function(){return B(err(_k8));}),_ka=function(_kb,_kc,_kd){while(1){var _ke=(function(_kf,_kg,_kh){var _ki=E(_kh);if(!_ki[0]){return [1,[0,_kf,_kg],_h];}else{var _kj=E(_ki[1]),_kk=E(_kg),_kl=_kk[1],_km=E(_kj[1])[1];if(!_km){return E(_k9);}else{if(_kl>=(_km-1|0)){var _kn=_kf;_kc=new T(function(){var _ko=E(_kj[2]);return _kl>_ko[1]?E(_kk):E(_ko);});_kd=_ki[2];_kb=_kn;return null;}else{return [1,[0,_kf,_kk],_ki];}}}})(_kb,_kc,_kd);if(_ke!=null){return _ke;}}},_kp=function(_kq,_kr){var _ks=E(_kq);if(!_ks[0]){return E(_kr);}else{var _kt=_ks[2],_ku=E(_kr);if(!_ku[0]){return E(_ks);}else{var _kv=_ku[2],_kw=E(_ks[1]),_kx=_kw[2],_ky=E(_ku[1]),_kz=_ky[2],_kA=E(_kw[1]),_kB=_kA[1],_kC=E(_ky[1]),_kD=_kC[1];if(_kB>=_kD){if(_kB!=_kD){return new F(function(){return _ka(_kC,_kz,B(_kp(_ks,_kv)));});}else{return new F(function(){return _ka(_kA,new T(function(){var _kE=E(_kx),_kF=E(_kz);return _kE[1]>_kF[1]?E(_kE):E(_kF);}),B(_kp(_kt,_kv)));});}}else{return new F(function(){return _ka(_kA,_kx,B(_kp(_kt,_ku)));});}}}},_kG=new T(function(){return B(_kp(_jW,_k7));}),_kH=[0,58],_kI=[0,_kH,_kH],_kJ=[1,_kI,_h],_kK=new T(function(){return B(_kp(_kG,_kJ));}),_kL=new T(function(){return B(_kp(_kK,_7O));}),_kM=function(_kN){return E(E(_kN)[1]);},_kO=function(_kP,_kQ,_kR){while(1){var _kS=E(_kR);if(!_kS[0]){return false;}else{if(!B(A(_kM,[_kP,_kQ,_kS[1]]))){_kR=_kS[2];continue;}else{return true;}}}},_kT=[0,1],_kU=new T(function(){return B(unCStr("legal XML name start character"));}),_kV=[1,_kU,_h],_kW=[0,58],_kX=[0,95],_kY=[1,_kX,_h],_kZ=[1,_kW,_kY],_l0=function(_l1,_l2){while(1){var _l3=E(_l1);if(!_l3[0]){return E(_l2);}else{_l1=_l3[2];var _l4=[1,_l3[1],_l2];_l2=_l4;continue;}}},_l5=function(_l6){return function(_l7,_l8,_l9,_la,_lb){return new F(function(){return A(_la,[new T(function(){return B(_l0(_l6,_h));}),_l7,new T(function(){return [0,E(E(_l7)[2]),_h];})]);});};},_lc=function(_ld,_le,_lf,_lg,_lh,_li){var _lj=function(_lk,_ll,_lm){var _ln=[1,_ll,_lk];return new F(function(){return A(_ld,[_lm,new T(function(){var _lo=E(_lk);return function(_lp,_lq,_lr){return new F(function(){return _lj(_ln,_lp,_lq);});};}),_lg,_7f,function(_ls){return new F(function(){return A(_l5,[_ln,_lm,_lf,_lg,function(_lt,_lu,_lv){return new F(function(){return A(_lf,[_lt,_lu,new T(function(){return B(_4a(_ls,_lv));})]);});},function(_lw){return new F(function(){return A(_lg,[new T(function(){return B(_4a(_ls,_lw));})]);});}]);});}]);});};return new F(function(){return A(_ld,[_le,function(_lx,_ly,_lz){return new F(function(){return _lj(_h,_lx,_ly);});},_lg,_7f,function(_lA){return new F(function(){return A(_l5,[_h,_le,_lf,_lg,function(_lB,_lC,_lD){return new F(function(){return A(_lh,[_lB,_lC,new T(function(){return B(_4a(_lA,_lD));})]);});},function(_lE){return new F(function(){return A(_li,[new T(function(){return B(_4a(_lA,_lE));})]);});}]);});}]);});},_lF=function(_lG,_lH){var _lI=_lG%_lH;if(_lG<=0){if(_lG>=0){return E(_lI);}else{if(_lH<=0){return E(_lI);}else{var _lJ=E(_lI);return _lJ==0?0:_lJ+_lH|0;}}}else{if(_lH>=0){if(_lG>=0){return E(_lI);}else{if(_lH<=0){return E(_lI);}else{var _lK=E(_lI);return _lK==0?0:_lK+_lH|0;}}}else{var _lL=E(_lI);return _lL==0?0:_lL+_lH|0;}}},_lM=function(_lN,_lO,_lP,_lQ,_lR,_lS,_lT,_lU,_lV,_lW){var _lX=[0,_lR,E(_lS),E(_lT)];return new F(function(){return A(_3s,[_lN,new T(function(){return B(A(_lO,[_lQ]));}),function(_lY){var _lZ=E(_lY);if(!_lZ[0]){return E(new T(function(){return B(A(_lW,[[0,E(_lX),_7L]]));}));}else{var _m0=E(_lZ[1]),_m1=_m0[1],_m2=_m0[2];if(!B(A(_lP,[_m1]))){return new F(function(){return A(_lW,[[0,E(_lX),[1,[0,E([1,_39,new T(function(){return B(_3b([1,_m1,_h],_7J));})])],_h]]]);});}else{var _m3=E(_lS),_m4=E(_m1);switch(E(_m4[1])){case 9:var _m5=E(_lT)[1],_m6=[0,_lR,E(_m3),E([0,(_m5+8|0)-B(_lF(_m5-1|0,8))|0])];break;case 10:var _m6=[0,_lR,E([0,_m3[1]+1|0]),E(_kT)];break;default:var _m6=[0,_lR,E(_m3),E([0,E(_lT)[1]+1|0])];}var _m7=_m6,_m8=[0,E(_m7),_h],_m9=[0,_m2,E(_m7),E(_lU)];return new F(function(){return A(_lV,[_m4,_m9,_m8]);});}}}]);});},_ma=[0,903],_mb=[0,_ma,_ma],_mc=[0,1600],_md=[0,_mc,_mc],_me=[0,3654],_mf=[0,_me,_me],_mg=[0,3782],_mh=[0,_mg,_mg],_mi=[0,12293],_mj=[0,_mi,_mi],_mk=[0,12341],_ml=[0,12337],_mm=[0,_ml,_mk],_mn=[0,12542],_mo=[0,12540],_mp=[0,_mo,_mn],_mq=[1,_mp,_h],_mr=[0,12446],_ms=[0,12445],_mt=[0,_ms,_mr],_mu=[1,_mt,_mq],_mv=[1,_mm,_mu],_mw=[1,_mj,_mv],_mx=[1,_mh,_mw],_my=[1,_mf,_mx],_mz=[1,_md,_my],_mA=[1,_mb,_mz],_mB=[0,721],_mC=[0,_mB,_mB],_mD=[1,_mC,_mA],_mE=[0,720],_mF=[0,_mE,_mE],_mG=[1,_mF,_mD],_mH=[0,183],_mI=[0,_mH,_mH],_mJ=[1,_mI,_mG],_mK=[0,1441],_mL=[0,1425],_mM=[0,_mL,_mK],_mN=[0,1465],_mO=[0,1443],_mP=[0,_mO,_mN],_mQ=[0,1469],_mR=[0,1467],_mS=[0,_mR,_mQ],_mT=[0,1471],_mU=[0,_mT,_mT],_mV=[0,1474],_mW=[0,1473],_mX=[0,_mW,_mV],_mY=[0,1476],_mZ=[0,_mY,_mY],_n0=[0,2504],_n1=[0,2503],_n2=[0,_n1,_n0],_n3=[0,2509],_n4=[0,2507],_n5=[0,_n4,_n3],_n6=[0,2519],_n7=[0,_n6,_n6],_n8=[0,2531],_n9=[0,2530],_na=[0,_n9,_n8],_nb=[0,2562],_nc=[0,_nb,_nb],_nd=[0,2620],_ne=[0,_nd,_nd],_nf=[0,2622],_ng=[0,_nf,_nf],_nh=[0,2623],_ni=[0,_nh,_nh],_nj=[0,2626],_nk=[0,2624],_nl=[0,_nk,_nj],_nm=[0,2632],_nn=[0,2631],_no=[0,_nn,_nm],_np=[0,2637],_nq=[0,2635],_nr=[0,_nq,_np],_ns=[0,2673],_nt=[0,2672],_nu=[0,_nt,_ns],_nv=[0,2691],_nw=[0,2689],_nx=[0,_nw,_nv],_ny=[0,2748],_nz=[0,_ny,_ny],_nA=[0,2757],_nB=[0,2750],_nC=[0,_nB,_nA],_nD=[0,2761],_nE=[0,2759],_nF=[0,_nE,_nD],_nG=[0,2765],_nH=[0,2763],_nI=[0,_nH,_nG],_nJ=[0,2819],_nK=[0,2817],_nL=[0,_nK,_nJ],_nM=[0,2876],_nN=[0,_nM,_nM],_nO=[0,2883],_nP=[0,2878],_nQ=[0,_nP,_nO],_nR=[0,2888],_nS=[0,2887],_nT=[0,_nS,_nR],_nU=[0,2893],_nV=[0,2891],_nW=[0,_nV,_nU],_nX=[0,2903],_nY=[0,2902],_nZ=[0,_nY,_nX],_o0=[0,2947],_o1=[0,2946],_o2=[0,_o1,_o0],_o3=[0,3010],_o4=[0,3006],_o5=[0,_o4,_o3],_o6=[0,3016],_o7=[0,3014],_o8=[0,_o7,_o6],_o9=[0,3021],_oa=[0,3018],_ob=[0,_oa,_o9],_oc=[0,3031],_od=[0,_oc,_oc],_oe=[0,3075],_of=[0,3073],_og=[0,_of,_oe],_oh=[0,3140],_oi=[0,3134],_oj=[0,_oi,_oh],_ok=[0,3144],_ol=[0,3142],_om=[0,_ol,_ok],_on=[0,3149],_oo=[0,3146],_op=[0,_oo,_on],_oq=[0,3158],_or=[0,3157],_os=[0,_or,_oq],_ot=[0,3203],_ou=[0,3202],_ov=[0,_ou,_ot],_ow=[0,3268],_ox=[0,3262],_oy=[0,_ox,_ow],_oz=[0,3272],_oA=[0,3270],_oB=[0,_oA,_oz],_oC=[0,3277],_oD=[0,3274],_oE=[0,_oD,_oC],_oF=[0,3286],_oG=[0,3285],_oH=[0,_oG,_oF],_oI=[0,3331],_oJ=[0,3330],_oK=[0,_oJ,_oI],_oL=[0,3395],_oM=[0,3390],_oN=[0,_oM,_oL],_oO=[0,3400],_oP=[0,3398],_oQ=[0,_oP,_oO],_oR=[0,3405],_oS=[0,3402],_oT=[0,_oS,_oR],_oU=[0,3415],_oV=[0,_oU,_oU],_oW=[0,3633],_oX=[0,_oW,_oW],_oY=[0,3642],_oZ=[0,3636],_p0=[0,_oZ,_oY],_p1=[0,3662],_p2=[0,3655],_p3=[0,_p2,_p1],_p4=[0,3761],_p5=[0,_p4,_p4],_p6=[0,3769],_p7=[0,3764],_p8=[0,_p7,_p6],_p9=[0,3772],_pa=[0,3771],_pb=[0,_pa,_p9],_pc=[0,3789],_pd=[0,3784],_pe=[0,_pd,_pc],_pf=[0,3865],_pg=[0,3864],_ph=[0,_pg,_pf],_pi=[0,3893],_pj=[0,_pi,_pi],_pk=[0,3895],_pl=[0,_pk,_pk],_pm=[0,3897],_pn=[0,_pm,_pm],_po=[0,3902],_pp=[0,_po,_po],_pq=[0,3903],_pr=[0,_pq,_pq],_ps=[0,3972],_pt=[0,3953],_pu=[0,_pt,_ps],_pv=[0,3979],_pw=[0,3974],_px=[0,_pw,_pv],_py=[0,3989],_pz=[0,3984],_pA=[0,_pz,_py],_pB=[0,3991],_pC=[0,_pB,_pB],_pD=[0,4013],_pE=[0,3993],_pF=[0,_pE,_pD],_pG=[0,4023],_pH=[0,4017],_pI=[0,_pH,_pG],_pJ=[0,4025],_pK=[0,_pJ,_pJ],_pL=[0,8412],_pM=[0,8400],_pN=[0,_pM,_pL],_pO=[0,8417],_pP=[0,_pO,_pO],_pQ=[0,12442],_pR=[0,_pQ,_pQ],_pS=[1,_pR,_h],_pT=[0,12441],_pU=[0,_pT,_pT],_pV=[1,_pU,_pS],_pW=[0,12335],_pX=[0,12330],_pY=[0,_pX,_pW],_pZ=[1,_pY,_pV],_q0=[1,_pP,_pZ],_q1=[1,_pN,_q0],_q2=[1,_pK,_q1],_q3=[1,_pI,_q2],_q4=[1,_pF,_q3],_q5=[1,_pC,_q4],_q6=[1,_pA,_q5],_q7=[1,_px,_q6],_q8=[1,_pu,_q7],_q9=[1,_pr,_q8],_qa=[1,_pp,_q9],_qb=[1,_pn,_qa],_qc=[1,_pl,_qb],_qd=[1,_pj,_qc],_qe=[1,_ph,_qd],_qf=[1,_pe,_qe],_qg=[1,_pb,_qf],_qh=[1,_p8,_qg],_qi=[1,_p5,_qh],_qj=[1,_p3,_qi],_qk=[1,_p0,_qj],_ql=[1,_oX,_qk],_qm=[1,_oV,_ql],_qn=[1,_oT,_qm],_qo=[1,_oQ,_qn],_qp=[1,_oN,_qo],_qq=[1,_oK,_qp],_qr=[1,_oH,_qq],_qs=[1,_oE,_qr],_qt=[1,_oB,_qs],_qu=[1,_oy,_qt],_qv=[1,_ov,_qu],_qw=[1,_os,_qv],_qx=[1,_op,_qw],_qy=[1,_om,_qx],_qz=[1,_oj,_qy],_qA=[1,_og,_qz],_qB=[1,_od,_qA],_qC=[1,_ob,_qB],_qD=[1,_o8,_qC],_qE=[1,_o5,_qD],_qF=[1,_o2,_qE],_qG=[1,_nZ,_qF],_qH=[1,_nW,_qG],_qI=[1,_nT,_qH],_qJ=[1,_nQ,_qI],_qK=[1,_nN,_qJ],_qL=[1,_nL,_qK],_qM=[1,_nI,_qL],_qN=[1,_nF,_qM],_qO=[1,_nC,_qN],_qP=[1,_nz,_qO],_qQ=[1,_nx,_qP],_qR=[1,_nu,_qQ],_qS=[1,_nr,_qR],_qT=[1,_no,_qS],_qU=[1,_nl,_qT],_qV=[1,_ni,_qU],_qW=[1,_ng,_qV],_qX=[1,_ne,_qW],_qY=[1,_nc,_qX],_qZ=[1,_na,_qY],_r0=[1,_n7,_qZ],_r1=[1,_n5,_r0],_r2=[1,_n2,_r1],_r3=[0,2500],_r4=[0,2496],_r5=[0,_r4,_r3],_r6=[1,_r5,_r2],_r7=[0,2495],_r8=[0,_r7,_r7],_r9=[1,_r8,_r6],_ra=[0,2494],_rb=[0,_ra,_ra],_rc=[1,_rb,_r9],_rd=[0,2492],_re=[0,_rd,_rd],_rf=[1,_re,_rc],_rg=[0,2435],_rh=[0,2433],_ri=[0,_rh,_rg],_rj=[1,_ri,_rf],_rk=[0,2403],_rl=[0,2402],_rm=[0,_rl,_rk],_rn=[1,_rm,_rj],_ro=[0,2388],_rp=[0,2385],_rq=[0,_rp,_ro],_rr=[1,_rq,_rn],_rs=[0,2381],_rt=[0,_rs,_rs],_ru=[1,_rt,_rr],_rv=[0,2380],_rw=[0,2366],_rx=[0,_rw,_rv],_ry=[1,_rx,_ru],_rz=[0,2364],_rA=[0,_rz,_rz],_rB=[1,_rA,_ry],_rC=[0,2307],_rD=[0,2305],_rE=[0,_rD,_rC],_rF=[1,_rE,_rB],_rG=[0,1773],_rH=[0,1770],_rI=[0,_rH,_rG],_rJ=[1,_rI,_rF],_rK=[0,1768],_rL=[0,1767],_rM=[0,_rL,_rK],_rN=[1,_rM,_rJ],_rO=[0,1764],_rP=[0,1760],_rQ=[0,_rP,_rO],_rR=[1,_rQ,_rN],_rS=[0,1759],_rT=[0,1757],_rU=[0,_rT,_rS],_rV=[1,_rU,_rR],_rW=[0,1756],_rX=[0,1750],_rY=[0,_rX,_rW],_rZ=[1,_rY,_rV],_s0=[0,1648],_s1=[0,_s0,_s0],_s2=[1,_s1,_rZ],_s3=[0,1618],_s4=[0,1611],_s5=[0,_s4,_s3],_s6=[1,_s5,_s2],_s7=[1,_mZ,_s6],_s8=[1,_mX,_s7],_s9=[1,_mU,_s8],_sa=[1,_mS,_s9],_sb=[1,_mP,_sa],_sc=[1,_mM,_sb],_sd=[0,1158],_se=[0,1155],_sf=[0,_se,_sd],_sg=[1,_sf,_sc],_sh=[0,865],_si=[0,864],_sj=[0,_si,_sh],_sk=[1,_sj,_sg],_sl=[0,837],_sm=[0,768],_sn=[0,_sm,_sl],_so=[1,_sn,_sk],_sp=[0,_7M,_7M],_sq=[1,_sp,_h],_sr=[0,_kH,_kH],_ss=[1,_sr,_h],_st=new T(function(){return B(_kp(_ss,_sq));}),_su=[0,2543],_sv=[0,2534],_sw=[0,_sv,_su],_sx=[0,2671],_sy=[0,2662],_sz=[0,_sy,_sx],_sA=[0,2799],_sB=[0,2790],_sC=[0,_sB,_sA],_sD=[0,2927],_sE=[0,2918],_sF=[0,_sE,_sD],_sG=[0,3055],_sH=[0,3047],_sI=[0,_sH,_sG],_sJ=[0,3881],_sK=[0,3872],_sL=[0,_sK,_sJ],_sM=[1,_sL,_h],_sN=[0,3801],_sO=[0,3792],_sP=[0,_sO,_sN],_sQ=[1,_sP,_sM],_sR=[0,3673],_sS=[0,3664],_sT=[0,_sS,_sR],_sU=[1,_sT,_sQ],_sV=[0,3439],_sW=[0,3430],_sX=[0,_sW,_sV],_sY=[1,_sX,_sU],_sZ=[0,3311],_t0=[0,3302],_t1=[0,_t0,_sZ],_t2=[1,_t1,_sY],_t3=[0,3183],_t4=[0,3174],_t5=[0,_t4,_t3],_t6=[1,_t5,_t2],_t7=[1,_sI,_t6],_t8=[1,_sF,_t7],_t9=[1,_sC,_t8],_ta=[1,_sz,_t9],_tb=[1,_sw,_ta],_tc=[0,2415],_td=[0,2406],_te=[0,_td,_tc],_tf=[1,_te,_tb],_tg=[0,1785],_th=[0,1776],_ti=[0,_th,_tg],_tj=[1,_ti,_tf],_tk=[0,1641],_tl=[0,1632],_tm=[0,_tl,_tk],_tn=[1,_tm,_tj],_to=[0,57],_tp=[0,48],_tq=[0,_tp,_to],_tr=[1,_tq,_tn],_ts=new T(function(){return B(_kp(_kG,_tr));}),_tt=[0,46],_tu=[0,_tt,_tt],_tv=[1,_tu,_h],_tw=[0,45],_tx=[0,_tw,_tw],_ty=[1,_tx,_h],_tz=new T(function(){return B(_kp(_ty,_tv));}),_tA=new T(function(){return B(_kp(_ts,_tz));}),_tB=new T(function(){return B(_kp(_tA,_st));}),_tC=new T(function(){return B(_kp(_tB,_so));}),_tD=new T(function(){return B(_kp(_tC,_mJ));}),_tE=[1,_7M,_h],_tF=[1,_kH,_tE],_tG=[1,_tt,_tF],_tH=[1,_tw,_tG],_tI=function(_tJ){var _tK=E(_tJ),_tL=_tK[1];if(_tL>122){if(_tL<183){return false;}else{return new F(function(){return (function(_tM){while(1){var _tN=E(_tM);if(!_tN[0]){return false;}else{var _tO=E(_tN[1]);if(_tL<E(_tO[1])[1]){return false;}else{if(_tL>E(_tO[2])[1]){_tM=_tN[2];continue;}else{return true;}}}}})(_tD);});}}else{if(_tL<97){var _tP=new T(function(){return _tL<48?B(_kO(_7I,_tK,_tH)):_tL>57?B(_kO(_7I,_tK,_tH)):true;});return _tL<65?E(_tP):_tL>90?E(_tP):true;}else{return true;}}},_tQ=[0],_tR=function(_tS){return E(E(_tS)[3]);},_tT=function(_tU,_tV){var _tW=E(_tV);return _tW[0]==0?B(A(_tR,[_tU,_tQ])):B(A(_tR,[_tU,[1,[0,_tW[1],_tW[2]]]]));},_tX=function(_tY){return new F(function(){return _tT(_1r,_tY);});},_tZ=function(_u0,_u1,_u2,_u3,_u4){var _u5=E(_u0),_u6=E(_u5[2]);return new F(function(){return _lM(_1r,_tX,_tI,_u5[1],_u6[1],_u6[2],_u6[3],_u5[3],_u1,_u4);});},_u7=new T(function(){return B(unCStr("legal XML name character"));}),_u8=[1,_u7,_h],_u9=function(_ua,_ub,_uc,_ud,_ue){return new F(function(){return _5L(_tZ,_u8,_ua,_ub,_uc,_ud,_ue);});},_uf=function(_ug,_uh,_ui,_uj,_tY){return new F(function(){return _u9(_ug,_uh,_ui,_uj,_tY);});},_uk=function(_ul,_um,_un,_uo,_up,_uq,_ur,_us){var _ut=E(_ul);if(!_ut[0]){return new F(function(){return A(_us,[new T(function(){var _uu=B(_5q([0,_um,E(_un),E(_uo)],_7L,_kV));return [0,E(_uu[1]),_uu[2]];})]);});}else{var _uv=_ut[2],_uw=E(_ut[1]),_ux=_uw[1],_uy=new T(function(){var _uz=E(_un),_uA=function(_uB,_uC,_uD){return new F(function(){return _lc(_uf,_uC,function(_uE,_uF,_uG){return new F(function(){return A(_uq,[[1,_uB,_uE],_uF,new T(function(){var _uH=E(_uG),_uI=B(_3P(_uH[1],_uH[2],E(_uF)[2],_h));return [0,E(_uI[1]),_uI[2]];})]);});},_ur,function(_uJ,_uK,_uL){return new F(function(){return A(_uq,[[1,_uB,_uJ],_uK,new T(function(){var _uM=E(_uD),_uN=E(_uL),_uO=B(_3P(_uN[1],_uN[2],E(_uK)[2],_h)),_uP=B(_3P(_uM[1],_uM[2],_uO[1],_uO[2]));return [0,E(_uP[1]),_uP[2]];})]);});},function(_uQ){return new F(function(){return A(_ur,[new T(function(){return B(_4a(_uD,_uQ));})]);});});});};switch(E(_ux)){case 9:var _uR=E(_uo)[1],_uS=[0,_um,E(_uz),E([0,(_uR+8|0)-B(_lF(_uR-1|0,8))|0])],_uT=B(_uA(_uw,[0,_uv,E(_uS),E(_up)],[0,E(_uS),_h]));break;case 10:var _uU=[0,_um,E([0,_uz[1]+1|0]),E(_kT)],_uT=B(_uA(_uw,[0,_uv,E(_uU),E(_up)],[0,E(_uU),_h]));break;default:var _uV=[0,_um,E(_uz),E([0,E(_uo)[1]+1|0])],_uT=B(_uA(_uw,[0,_uv,E(_uV),E(_up)],[0,E(_uV),_h]));}var _uW=_uT;return _uW;}),_uX=new T(function(){return B(A(_us,[new T(function(){var _uY=B(_5q([0,_um,E(_un),E(_uo)],[1,[0,E([1,_39,new T(function(){return B(_3b([1,_uw,_h],_7J));})])],_h],_kV));return [0,E(_uY[1]),_uY[2]];})]));});return _ux>122?_ux<192?E(_uX):!B((function(_uZ){while(1){var _v0=E(_uZ);if(!_v0[0]){return false;}else{var _v1=E(_v0[1]);if(_ux<E(_v1[1])[1]){return false;}else{if(_ux>E(_v1[2])[1]){_uZ=_v0[2];continue;}else{return true;}}}}})(_kL))?E(_uX):E(_uy):_ux<97?_ux<65?!B(_kO(_7I,_uw,_kZ))?E(_uX):E(_uy):_ux>90?!B(_kO(_7I,_uw,_kZ))?E(_uX):E(_uy):E(_uy):E(_uy);}},_v2=function(_v3,_v4,_v5,_v6,_v7){return new F(function(){return A(_v6,[_G,_v3,new T(function(){return [0,E(E(_v3)[2]),_h];})]);});},_v8=function(_v9,_va,_vb,_vc,_vd,_ve){return new F(function(){return A(_v9,[_va,function(_vf,_vg,_vh){return new F(function(){return _7g(_v9,_vg,_vb,_vc,function(_vi,_vj,_vk){return new F(function(){return A(_vb,[_vi,_vj,new T(function(){return B(_4a(_vh,_vk));})]);});});});},_vc,function(_vl,_vm,_vn){return new F(function(){return _7g(_v9,_vm,_vb,_vc,function(_vo,_vp,_vq){return new F(function(){return A(_vd,[_vo,_vp,new T(function(){return B(_4a(_vn,_vq));})]);});});});},_ve]);});},_vr=new T(function(){return B(unCStr("Control.Exception.Base"));}),_vs=new T(function(){return B(unCStr("base"));}),_vt=new T(function(){return B(unCStr("PatternMatchFail"));}),_vu=new T(function(){var _vv=hs_wordToWord64(18445595),_vw=_vv,_vx=hs_wordToWord64(52003073),_vy=_vx;return [0,_vw,_vy,[0,_vw,_vy,_vs,_vr,_vt],_h];}),_vz=function(_vA){return E(_vu);},_vB=function(_vC){return E(E(_vC)[1]);},_vD=new T(function(){return B(unCStr("Maybe.fromJust: Nothing"));}),_vE=new T(function(){return B(err(_vD));}),_vF=function(_vG,_vH,_vI){var _vJ=new T(function(){var _vK=B(A(_vG,[_vI])),_vL=B(A(_vH,[new T(function(){var _vM=E(_vJ);return _vM[0]==0?E(_vE):E(_vM[1]);})])),_vN=hs_eqWord64(_vK[1],_vL[1]),_vO=_vN;if(!E(_vO)){var _vP=[0];}else{var _vQ=hs_eqWord64(_vK[2],_vL[2]),_vR=_vQ,_vP=E(_vR)==0?[0]:[1,_vI];}var _vS=_vP,_vT=_vS;return _vT;});return E(_vJ);},_vU=function(_vV){var _vW=E(_vV);return new F(function(){return _vF(B(_vB(_vW[1])),_vz,_vW[2]);});},_vX=function(_vY){return E(E(_vY)[1]);},_vZ=function(_w0,_w1){return new F(function(){return _0(E(_w0)[1],_w1);});},_w2=[0,44],_w3=[0,93],_w4=[0,91],_w5=function(_w6,_w7,_w8){var _w9=E(_w7);return _w9[0]==0?B(unAppCStr("[]",_w8)):[1,_w4,new T(function(){return B(A(_w6,[_w9[1],new T(function(){var _wa=function(_wb){var _wc=E(_wb);return _wc[0]==0?E([1,_w3,_w8]):[1,_w2,new T(function(){return B(A(_w6,[_wc[1],new T(function(){return B(_wa(_wc[2]));})]));})];};return B(_wa(_w9[2]));})]));})];},_wd=function(_we,_wf){return new F(function(){return _w5(_vZ,_we,_wf);});},_wg=function(_wh,_wi,_wj){return new F(function(){return _0(E(_wi)[1],_wj);});},_wk=[0,_wg,_vX,_wd],_wl=new T(function(){return [0,_vz,_wk,_wm,_vU];}),_wm=function(_wn){return [0,_wl,_wn];},_wo=new T(function(){return B(unCStr("Irrefutable pattern failed for pattern"));}),_wp=function(_wq,_wr){return new F(function(){return die(new T(function(){return B(A(_wr,[_wq]));}));});},_ws=function(_wt,_wu){var _wv=E(_wu);if(!_wv[0]){return [0,_h,_h];}else{var _ww=_wv[1];if(!B(A(_wt,[_ww]))){return [0,_h,_wv];}else{var _wx=new T(function(){var _wy=B(_ws(_wt,_wv[2]));return [0,_wy[1],_wy[2]];});return [0,[1,_ww,new T(function(){return E(E(_wx)[1]);})],new T(function(){return E(E(_wx)[2]);})];}}},_wz=[0,32],_wA=[0,10],_wB=[1,_wA,_h],_wC=function(_wD){return E(E(_wD)[1])==124?false:true;},_wE=function(_wF,_wG){var _wH=B(_ws(_wC,B(unCStr(_wF)))),_wI=_wH[1],_wJ=function(_wK,_wL){return new F(function(){return _0(_wK,new T(function(){return B(unAppCStr(": ",new T(function(){return B(_0(_wG,new T(function(){return B(_0(_wL,_wB));})));})));}));});},_wM=E(_wH[2]);if(!_wM[0]){return new F(function(){return _wJ(_wI,_h);});}else{return E(E(_wM[1])[1])==124?B(_wJ(_wI,[1,_wz,_wM[2]])):B(_wJ(_wI,_h));}},_wN=function(_wO){return new F(function(){return _wp([0,new T(function(){return B(_wE(_wO,_wo));})],_wm);});},_wP=new T(function(){return B(_wN("src/Text/XML/HXT/DOM/QualifiedName.hs:(534,1)-(546,27)|initialXNames@[nullXName,\n                                                                      xmlnsNamespaceXName,\n                                                                      xmlnsXName,\n                                                                      xmlNamespaceXName,\n                                                                      xmlXName]"));}),_wQ=new T(function(){return B(unCStr("xmlns"));}),_wR=new T(function(){return B(unCStr("xml"));}),_wS=[1,_wR,_h],_wT=new T(function(){return B(unCStr("http://www.w3.org/XML/1998/namespace"));}),_wU=[1,_wT,_wS],_wV=[1,_wQ,_wU],_wW=new T(function(){return B(unCStr("http://www.w3.org/2000/xmlns/"));}),_wX=[1,_wW,_wV],_wY=[1,_h,_wX],_wZ=function(_x0,_x1){var _x2=E(_x1);return _x2[0]==0?[0]:[1,[0,E([0,_x0]),_x2[1]],new T(function(){var _x3=E(_x0);if(_x3==2147483647){var _x4=[0];}else{var _x4=B(_wZ(_x3+1|0,_x2[2]));}return _x4;})];},_x5=new T(function(){var _x6=B(_wZ(0,_wY));if(!_x6[0]){var _x7=E(_wP);}else{var _x8=E(_x6[2]);if(!_x8[0]){var _x9=E(_wP);}else{var _xa=E(_x8[2]);if(!_xa[0]){var _xb=E(_wP);}else{var _xc=E(_xa[2]);if(!_xc[0]){var _xd=E(_wP);}else{var _xe=E(_xc[2]),_xd=_xe[0]==0?E(_wP):E(_xe[2])[0]==0?[0,_x6,_x6[1],_x8[1],_xa[1],_xc[1],_xe[1]]:E(_wP);}var _xb=_xd;}var _x9=_xb;}var _x7=_x9;}return _x7;}),_xf=new T(function(){return E(E(_x5)[2]);}),_xg=function(_xh,_xi){var _xj=E(_xh),_xk=E(E(_xj[3])[1])[1],_xl=E(E(_xj[1])[1])[1],_xm=E(_xi),_xn=E(E(_xm[1])[1])[1];return _xk!=E(E(_xm[3])[1])[1]?true:_xk!=E(E(_xf)[1])[1]?_xl!=_xn?true:false:_xl!=_xn?true:E(E(_xj[2])[1])[1]!=E(E(_xm[2])[1])[1]?true:false;},_xo=function(_xp,_xq){var _xr=E(_xp),_xs=E(E(_xr[3])[1])[1],_xt=E(E(_xr[1])[1])[1],_xu=E(_xq),_xv=E(E(_xu[1])[1])[1];return _xs!=E(E(_xu[3])[1])[1]?false:_xs!=E(E(_xf)[1])[1]?_xt==_xv:_xt!=_xv?false:E(E(_xr[2])[1])[1]==E(E(_xu[2])[1])[1];},_xw=[0,_xo,_xg],_xx=function(_xy,_xz,_xA,_xB,_xC,_xD,_xE,_xF){var _xG=function(_xH,_xI,_xJ,_xK,_xL,_xM){return new F(function(){return A(_xz,[_xI,function(_xN,_xO,_xP){return new F(function(){return A(_xJ,[_xH,_xO,new T(function(){var _xQ=E(_xP),_xR=B(_3P(_xQ[1],_xQ[2],E(_xO)[2],_h));return [0,E(_xR[1]),_xR[2]];})]);});},_xK,function(_xS,_xT,_xU){return new F(function(){return A(_xL,[_xH,_xT,new T(function(){var _xV=E(_xU),_xW=B(_3P(_xV[1],_xV[2],E(_xT)[2],_h));return [0,E(_xW[1]),_xW[2]];})]);});},_xM]);});},_xX=function(_xY,_xZ,_y0,_y1,_y2){return new F(function(){return A(_xA,[_xY,function(_y3,_y4,_y5){return new F(function(){return _xG(_y3,_y4,_xZ,_y0,function(_y6,_y7,_y8){return new F(function(){return A(_xZ,[_y6,_y7,new T(function(){return B(_4a(_y5,_y8));})]);});},function(_y9){return new F(function(){return A(_y0,[new T(function(){return B(_4a(_y5,_y9));})]);});});});},_y0,function(_ya,_yb,_yc){return new F(function(){return _xG(_ya,_yb,_xZ,_y0,function(_yd,_ye,_yf){return new F(function(){return A(_y1,[_yd,_ye,new T(function(){return B(_4a(_yc,_yf));})]);});},function(_yg){return new F(function(){return A(_y2,[new T(function(){return B(_4a(_yc,_yg));})]);});});});},_y2]);});};return new F(function(){return A(_xy,[_xB,function(_yh,_yi,_yj){return new F(function(){return _xX(_yi,_xC,_xD,function(_yk,_yl,_ym){return new F(function(){return A(_xC,[_yk,_yl,new T(function(){return B(_4a(_yj,_ym));})]);});},function(_yn){return new F(function(){return A(_xD,[new T(function(){return B(_4a(_yj,_yn));})]);});});});},_xD,function(_yo,_yp,_yq){return new F(function(){return _xX(_yp,_xC,_xD,function(_yr,_ys,_yt){return new F(function(){return A(_xE,[_yr,_ys,new T(function(){return B(_4a(_yq,_yt));})]);});},function(_yu){return new F(function(){return A(_xF,[new T(function(){return B(_4a(_yq,_yu));})]);});});});},_xF]);});},_yv=new T(function(){return B(unCStr("attribute value (in quotes)"));}),_yw=[1,_yv,_h],_yx=new T(function(){return B(unCStr("<&\""));}),_yy=function(_yz,_yA,_yB,_yC,_yD,_yE){var _yF=function(_yG,_yH,_yI,_yJ,_yK,_yL){return new F(function(){return _lc(_yz,_yH,function(_yM,_yN,_yO){return new F(function(){return A(_yI,[[1,_yG,_yM],_yN,new T(function(){var _yP=E(_yO),_yQ=B(_3P(_yP[1],_yP[2],E(_yN)[2],_h));return [0,E(_yQ[1]),_yQ[2]];})]);});},_yJ,function(_yR,_yS,_yT){return new F(function(){return A(_yK,[[1,_yG,_yR],_yS,new T(function(){var _yU=E(_yT),_yV=B(_3P(_yU[1],_yU[2],E(_yS)[2],_h));return [0,E(_yV[1]),_yV[2]];})]);});},_yL);});};return new F(function(){return A(_yz,[_yA,function(_yW,_yX,_yY){return new F(function(){return _yF(_yW,_yX,_yB,_yC,function(_yZ,_z0,_z1){return new F(function(){return A(_yB,[_yZ,_z0,new T(function(){return B(_4a(_yY,_z1));})]);});},function(_z2){return new F(function(){return A(_yC,[new T(function(){return B(_4a(_yY,_z2));})]);});});});},_yC,function(_z3,_z4,_z5){return new F(function(){return _yF(_z3,_z4,_yB,_yC,function(_z6,_z7,_z8){return new F(function(){return A(_yD,[_z6,_z7,new T(function(){return B(_4a(_z5,_z8));})]);});},function(_z9){return new F(function(){return A(_yE,[new T(function(){return B(_4a(_z5,_z9));})]);});});});},_yE]);});},_za=function(_zb){return new F(function(){return _tT(_1r,_zb);});},_zc=[0,9],_zd=[1,_zc,_h],_ze=[0,10],_zf=[1,_ze,_zd],_zg=function(_zh){return E(E(_zh)[2]);},_zi=function(_zj,_zk,_zl){while(1){var _zm=E(_zl);if(!_zm[0]){return true;}else{if(!B(A(_zg,[_zj,_zk,_zm[1]]))){return false;}else{_zl=_zm[2];continue;}}}},_zn=new T(function(){return B(unCStr("newline"));}),_zo=[1,_zn,_h],_zp=[0,10],_zq=function(_zr,_zs){return function(_zt,_zu,_zv,_zw,_zx){return new F(function(){return _5L(function(_zy,_zz,_zA,_zB,_zC){var _zD=E(_zr),_zE=E(_zy),_zF=E(_zE[2]);return new F(function(){return _lM(_zD[1],_zD[2],function(_zG){return new F(function(){return _7F(_zG,_zs);});},_zE[1],_zF[1],_zF[2],_zF[3],_zE[3],_zz,_zC);});},[1,[1,_39,new T(function(){return B(_3b([1,_zs,_h],_7J));})],_h],_zt,_zu,_zv,_zw,_zx);});};},_zH=function(_zI){return [0,_zI,function(_5f){return new F(function(){return _tT(_zI,_5f);});}];},_zJ=new T(function(){return B(_zH(_1r));}),_zK=new T(function(){return B(_zq(_zJ,_zp));}),_zL=[0,13],_zM=function(_zN,_zO,_zP,_zQ){var _zR=E(_zN),_zS=_zR[2],_zT=new T(function(){var _zU=E(_zS),_zV=_zU[1];return B(_3H(_zV,_zV))==1?[0,E(_zU),_3O]:[0,E(_zU),_h];});if(!E(E(_zR[3])[1])){return new F(function(){return A(_zQ,[_zL,_zR,new T(function(){var _zW=E(_zT),_zX=B(_3P(_zW[1],_zW[2],_zS,_h));return [0,E(_zX[1]),_zX[2]];})]);});}else{return new F(function(){return A(_zK,[_zR,_zO,_zP,function(_zY,_zZ,_A0){return new F(function(){return A(_zQ,[_zY,_zZ,new T(function(){return B(_4a(_zT,_A0));})]);});},function(_A1){return new F(function(){return A(_zQ,[_zp,_zR,new T(function(){var _A2=E(_zT),_A3=E(_A1),_A4=B(_3P(_A3[1],_A3[2],_zS,_h)),_A5=B(_3P(_A2[1],_A2[2],_A4[1],_A4[2]));return [0,E(_A5[1]),_A5[2]];})]);});}]);});}},_A6=new T(function(){return B(_zq(_zJ,_zL));}),_A7=function(_A8,_A9,_Aa,_Ab,_Ac){return new F(function(){return A(_A6,[_A8,function(_Ad,_Ae,_Af){return new F(function(){return _zM(_Ae,_A9,_Aa,function(_Ag,_Ah,_Ai){return new F(function(){return A(_A9,[_Ag,_Ah,new T(function(){return B(_4a(_Af,_Ai));})]);});});});},_Aa,function(_Aj,_Ak,_Al){return new F(function(){return _zM(_Ak,_A9,_Aa,function(_Am,_An,_Ao){return new F(function(){return A(_Ab,[_Am,_An,new T(function(){return B(_4a(_Al,_Ao));})]);});});});},_Ac]);});},_Ap=function(_Aq,_Ar,_As,_At,_Au){return new F(function(){return _5L(_A7,_zo,_Aq,_Ar,_As,_At,_Au);});},_Av=function(_Aw,_Ax,_Ay,_Az,_AA,_AB){return new F(function(){return _4g(function(_AC,_AD,_AE,_AF,_AG){var _AH=E(_AC),_AI=E(_AH[2]);return new F(function(){return _lM(_1r,_za,function(_AJ){var _AK=E(_AJ),_AL=_AK[1],_AM=new T(function(){return !B(_kO(_7I,_AK,_zf))?_AL<57344?false:_AL>65533?_AL<65536?false:B(_zi(_7I,_AK,_Aw)):B(_zi(_7I,_AK,_Aw)):B(_zi(_7I,_AK,_Aw));});return _AL<32?E(_AM):_AL>55295?E(_AM):B(_zi(_7I,_AK,_Aw));},_AH[1],_AI[1],_AI[2],_AI[3],_AH[3],_AD,_AG);});},_Ap,_Ax,_Ay,_Az,_AA,_AB);});},_AN=function(_AO){return function(_AP,_AQ,_AR,_AS,_AT){return new F(function(){return A(_AS,[new T(function(){var _AU=[0,[0,_AO],_h],_AV=B(_6v(_6O,_AU));return E(_AU);}),_AP,new T(function(){return [0,E(E(_AP)[2]),_h];})]);});};},_AW=function(_AX,_AY,_AZ,_B0,_B1,_B2){return new F(function(){return _yy(function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _Av(_AX,_B3,_B4,_B5,_B6,_zb);});},_AY,function(_B7,_B8,_B9){return new F(function(){return A(_AN,[_B7,_B8,_AZ,_B0,function(_Ba,_Bb,_Bc){return new F(function(){return A(_AZ,[_Ba,_Bb,new T(function(){return B(_4a(_B9,_Bc));})]);});},function(_Bd){return new F(function(){return A(_B0,[new T(function(){return B(_4a(_B9,_Bd));})]);});}]);});},_B0,function(_Be,_Bf,_Bg){return new F(function(){return A(_AN,[_Be,_Bf,_AZ,_B0,function(_Bh,_Bi,_Bj){return new F(function(){return A(_B1,[_Bh,_Bi,new T(function(){return B(_4a(_Bg,_Bj));})]);});},function(_Bk){return new F(function(){return A(_B2,[new T(function(){return B(_4a(_Bg,_Bk));})]);});}]);});},_B2);});},_Bl=new T(function(){return B(unCStr("entity reference"));}),_Bm=[1,_Bl,_h],_Bn=[0,59],_Bo=new T(function(){return B(_zH(_1r));}),_Bp=new T(function(){return B(unCStr("Name"));}),_Bq=[1,_Bp,_h],_Br=function(_Bs,_Bt,_Bu,_Bv,_Bw,_Bx,_By,_Bz){return new F(function(){return _uk(_Bs,_Bt,_Bu,_Bv,_Bw,function(_BA,_BB,_BC){return new F(function(){return A(_zq,[_Bo,_Bn,_BB,function(_BD,_BE,_BF){return new F(function(){return A(_Bx,[_BA,_BE,new T(function(){var _BG=E(_BE)[2],_BH=E(_BF),_BI=B(_3P(_BH[1],_BH[2],_BG,_h)),_BJ=B(_3P(_BI[1],_BI[2],_BG,_h));return [0,E(_BJ[1]),_BJ[2]];})]);});},_By,function(_BK,_BL,_BM){return new F(function(){return A(_Bx,[_BA,_BL,new T(function(){var _BN=E(_BC),_BO=E(_BL)[2],_BP=E(_BM),_BQ=B(_3P(_BP[1],_BP[2],_BO,_h)),_BR=B(_3P(_BQ[1],_BQ[2],_BO,_h)),_BS=B(_3P(_BN[1],_BN[2],_BR[1],_BR[2]));return [0,E(_BS[1]),_BS[2]];})]);});},function(_BT){return new F(function(){return A(_By,[new T(function(){return B(_4a(_BC,_BT));})]);});}]);});},_By,function(_BU){return new F(function(){return A(_Bz,[new T(function(){var _BV=E(_BU),_BW=B(_5q(_BV[1],_BV[2],_Bq));return [0,E(_BW[1]),_BW[2]];})]);});});});},_BX=[0,38],_BY=function(_BZ,_C0,_C1,_C2){return new F(function(){return A(_zq,[_Bo,_BX,_BZ,function(_C3,_C4,_C5){var _C6=E(_C4),_C7=E(_C6[2]);return new F(function(){return _Br(_C6[1],_C7[1],_C7[2],_C7[3],_C6[3],_C0,_C1,function(_C8){return new F(function(){return A(_C1,[new T(function(){return B(_4a(new T(function(){var _C9=E(_C5),_Ca=B(_3P(_C9[1],_C9[2],_C7,_h));return [0,E(_Ca[1]),_Ca[2]];}),_C8));})]);});});});},_C1,function(_Cb,_Cc,_Cd){var _Ce=E(_Cc),_Cf=E(_Ce[2]);return new F(function(){return _Br(_Ce[1],_Cf[1],_Cf[2],_Cf[3],_Ce[3],_C0,_C1,function(_Cg){return new F(function(){return A(_C2,[new T(function(){return B(_4a(new T(function(){var _Ch=E(_Cd),_Ci=B(_3P(_Ch[1],_Ch[2],_Cf,_h));return [0,E(_Ci[1]),_Ci[2]];}),_Cg));})]);});});});},_C2]);});},_Cj=function(_Ck,_Cl,_Cm){while(1){var _Cn=E(_Cl);if(!_Cn[0]){return E(_Cm)[0]==0?true:false;}else{var _Co=E(_Cm);if(!_Co[0]){return false;}else{if(!B(A(_kM,[_Ck,_Cn[1],_Co[1]]))){return false;}else{_Cl=_Cn[2];_Cm=_Co[2];continue;}}}}},_Cp=function(_Cq,_Cr,_Cs){return !B(_Cj(_Cq,_Cr,_Cs))?true:false;},_Ct=function(_Cu){return [0,function(_Cv,_Cw){return new F(function(){return _Cj(_Cu,_Cv,_Cw);});},function(_Cv,_Cw){return new F(function(){return _Cp(_Cu,_Cv,_Cw);});}];},_Cx=new T(function(){return B(_Ct(_7I));}),_Cy=function(_Cz,_CA,_CB){while(1){var _CC=E(_CB);if(!_CC[0]){return [0];}else{var _CD=E(_CC[1]);if(!B(A(_kM,[_Cz,_CA,_CD[1]]))){_CB=_CC[2];continue;}else{return [1,_CD[2]];}}}},_CE=function(_CF){var _CG=[0,[2,_CF],_h],_CH=B(_6v(_6O,_CG));return E(_CG);},_CI=function(_CJ){var _CK=[0,[3,_CJ],_h],_CL=B(_6v(_6O,_CK));return E(_CK);},_CM=[0,34],_CN=new T(function(){return B(unCStr("quot"));}),_CO=[0,_CN,_CM],_CP=[1,_CO,_h],_CQ=[0,39],_CR=new T(function(){return B(unCStr("apos"));}),_CS=[0,_CR,_CQ],_CT=[1,_CS,_CP],_CU=[0,38],_CV=new T(function(){return B(unCStr("amp"));}),_CW=[0,_CV,_CU],_CX=[1,_CW,_CT],_CY=[0,62],_CZ=new T(function(){return B(unCStr("gt"));}),_D0=[0,_CZ,_CY],_D1=[1,_D0,_CX],_D2=[0,60],_D3=new T(function(){return B(unCStr("lt"));}),_D4=[0,_D3,_D2],_D5=[1,_D4,_D1],_D6=function(_D7,_D8,_D9,_Da){return new F(function(){return _BY(_D7,function(_Db,_Dc,_Dd){var _De=B(_Cy(_Cx,_Db,_D5));return new F(function(){return A(_D8,[_De[0]==0?B(_CI(_Db)):B(_CE(_De[1])),_Dc,new T(function(){var _Df=E(_Dd),_Dg=B(_3P(_Df[1],_Df[2],E(_Dc)[2],_h));return [0,E(_Dg[1]),_Dg[2]];})]);});},_D9,function(_Dh){return new F(function(){return A(_Da,[new T(function(){var _Di=E(_Dh),_Dj=B(_5q(_Di[1],_Di[2],_Bm));return [0,E(_Dj[1]),_Dj[2]];})]);});});});},_Dk=function(_Dl,_Dm,_Dn,_Do,_Dp){return new F(function(){return _D6(_Dl,_Dm,_Dn,_Dp);});},_Dq=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _Dk(_B3,_B4,_B5,_B6,_zb);});},_Dr=function(_Ds){return function(_Dt,_Du,_Dv,_Dw,_Dx){return new F(function(){return A(_Dw,[new T(function(){var _Dy=[0,[2,_Ds],_h],_Dz=B(_6v(_6O,_Dy));return E(_Dy);}),_Dt,new T(function(){return [0,E(E(_Dt)[2]),_h];})]);});};},_DA=new T(function(){return B(unCStr("character reference"));}),_DB=[1,_DA,_h],_DC=new T(function(){return B(unCStr("&#"));}),_DD=function(_DE){var _DF=E(_DE)[1];return _DF<48?false:_DF<=57;},_DG=function(_DH,_DI,_DJ,_DK,_DL){var _DM=E(_DH),_DN=E(_DM[2]);return new F(function(){return _lM(_1r,_za,_DD,_DM[1],_DN[1],_DN[2],_DN[3],_DM[3],_DI,_DL);});},_DO=new T(function(){return B(unCStr("digit"));}),_DP=[1,_DO,_h],_DQ=function(_DR,_DS,_DT,_DU,_DV){return new F(function(){return _5L(_DG,_DP,_DR,_DS,_DT,_DU,_DV);});},_DW=function(_DX){return new F(function(){return err(B(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return B(_b(9,_DX,_h));}))));});},_DY=[0,59],_DZ=[1,_DY,_h],_E0=function(_E1){return new F(function(){return unAppCStr("&#",new T(function(){return B(_0(B(_b(0,E(_E1)[1],_h)),_DZ));}));});},_E2=function(_E3,_E4){while(1){var _E5=E(_E3);if(!_E5[0]){return E(_E4);}else{_E3=_E5[2];var _E6=_E4+1|0;_E4=_E6;continue;}}},_E7=[0,48],_E8=[1,_DY,_h],_E9=function(_Ea,_Eb){if(_Ea<=0){if(_Ea>=0){return new F(function(){return quot(_Ea,_Eb);});}else{if(_Eb<=0){return new F(function(){return quot(_Ea,_Eb);});}else{return quot(_Ea+1|0,_Eb)-1|0;}}}else{if(_Eb>=0){if(_Ea>=0){return new F(function(){return quot(_Ea,_Eb);});}else{if(_Eb<=0){return new F(function(){return quot(_Ea,_Eb);});}else{return quot(_Ea+1|0,_Eb)-1|0;}}}else{return quot(_Ea-1|0,_Eb)-1|0;}}},_Ec=new T(function(){return B(unCStr("0123456789ABCDEF"));}),_Ed=function(_Ee){var _Ef=E(_Ee);if(!_Ef){return [0];}else{return new F(function(){return _0(B(_Ed(B(_E9(_Ef,16)))),[1,new T(function(){var _Eg=B(_lF(_Ef,16));return _Eg>=0?B(_1w(_Ec,_Eg)):E(_1t);}),_h]);});}},_Eh=[1,_E7,_h],_Ei=function(_Ej){return new F(function(){return err(B(unAppCStr("intToHexString: negative argument ",new T(function(){return B(_b(0,_Ej,_h));}))));});},_Ek=function(_El){var _Em=E(_El);return _Em==0?E(_Eh):_Em<=0?B(_Ei(_Em)):B(_Ed(_Em));},_En=function(_Eo){return new F(function(){return _Ek(E(_Eo)[1]);});},_Ep=function(_Eq){return new F(function(){return unAppCStr("&#x",new T(function(){var _Er=B(_En(_Eq));return B(_lF(B(_E2(_Er,0)),2))==1?B(_0([1,_E7,_Er],_E8)):B(_0(_Er,_E8));}));});},_Es=[0,13],_Et=[1,_Es,_h],_Eu=[1,_zc,_Et],_Ev=[1,_ze,_Eu],_Ew=function(_Ex){var _Ey=E(_Ex),_Ez=_Ey[1],_EA=function(_EB,_EC,_ED,_EE,_EF){return new F(function(){return A(_EF,[new T(function(){return [0,E(E(_EB)[2]),[1,new T(function(){return [1,E(B(unAppCStr("illegal value in character reference: ",new T(function(){return B(_0(B(_E0(_Ey)),new T(function(){return B(unAppCStr(" , in hex: ",new T(function(){return B(_Ep(_Ey));})));})));}))))];}),_h]];})]);});};if(_Ez>1114111){return E(_EA);}else{if(_Ez>>>0>1114111){return new F(function(){return _DW(_Ez);});}else{var _EG=_Ez,_EH=new T(function(){if(!B(_kO(_7I,[0,_EG],_Ev))){if(_EG<57344){var _EI=E(_EA);}else{if(_EG>65533){if(_EG<65536){var _EJ=E(_EA);}else{var _EJ=function(_EK,_EL,_EM,_EN,_EO){return new F(function(){return A(_EN,[_Ey,_EK,new T(function(){return [0,E(E(_EK)[2]),_h];})]);});};}var _EP=_EJ;}else{var _EP=function(_EQ,_ER,_ES,_ET,_EU){return new F(function(){return A(_ET,[_Ey,_EQ,new T(function(){return [0,E(E(_EQ)[2]),_h];})]);});};}var _EI=_EP;}var _EV=_EI;}else{var _EV=function(_EW,_EX,_EY,_EZ,_F0){return new F(function(){return A(_EZ,[_Ey,_EW,new T(function(){return [0,E(E(_EW)[2]),_h];})]);});};}return _EV;});return _EG<32?E(_EH):_EG>55295?E(_EH):function(_F1,_F2,_F3,_F4,_F5){return new F(function(){return A(_F4,[_Ey,_F1,new T(function(){return [0,E(E(_F1)[2]),_h];})]);});};}}},_F6=function(_F7){var _F8=E(_F7);if(!_F8[0]){return [0];}else{var _F9=_F8[2],_Fa=E(_F8[1])[1],_Fb=new T(function(){if(_Fa<65){if(_Fa<97){var _Fc=[0];}else{if(_Fa>122){var _Fd=[0];}else{var _Fd=[1,[0,(_Fa-97|0)+10|0],_h];}var _Fc=_Fd;}var _Fe=_Fc;}else{if(_Fa>90){if(_Fa<97){var _Ff=[0];}else{if(_Fa>122){var _Fg=[0];}else{var _Fg=[1,[0,(_Fa-97|0)+10|0],_h];}var _Ff=_Fg;}var _Fh=_Ff;}else{var _Fh=[1,[0,(_Fa-65|0)+10|0],_h];}var _Fe=_Fh;}return _Fe;});if(_Fa<48){return new F(function(){return _0(_Fb,new T(function(){return B(_F6(_F9));}));});}else{if(_Fa>57){return new F(function(){return _0(_Fb,new T(function(){return B(_F6(_F9));}));});}else{return [1,[0,_Fa-48|0],new T(function(){return B(_F6(_F9));})];}}}},_Fi=function(_Fj,_Fk){var _Fl=function(_Fm,_Fn){return imul(_Fm,B((function(_Fo,_Fp){while(1){var _Fq=E(_Fp);if(!_Fq[0]){return E(_Fo);}else{var _Fr=(imul(_Fo,E(_Fj)[1])|0)+E(_Fq[1])[1]|0;_Fp=_Fq[2];_Fo=_Fr;continue;}}})(0,B(_F6(_Fn)))))|0;},_Fs=E(_Fk);if(!_Fs[0]){return new F(function(){return _Fl(1,_h);});}else{var _Ft=_Fs[2];switch(E(E(_Fs[1])[1])){case 43:return new F(function(){return _Fl(1,_Ft);});break;case 45:return new F(function(){return _Fl(-1,_Ft);});break;default:return new F(function(){return _Fl(1,_Fs);});}}},_Fu=[0,10],_Fv=function(_Fw){return [0,B(_Fi(_Fu,_Fw))];},_Fx=function(_Fy){var _Fz=new T(function(){return B(_Ew(B(_Fv(_Fy))));});return function(_FA,_FB,_FC,_FD,_FE){return new F(function(){return A(_zq,[_Bo,_Bn,_FA,function(_FF,_FG,_FH){var _FI=new T(function(){var _FJ=E(_FH),_FK=B(_3P(_FJ[1],_FJ[2],E(_FG)[2],_h));return [0,E(_FK[1]),_FK[2]];});return new F(function(){return A(_Fz,[_FG,_FB,_FC,function(_FL,_FM,_FN){return new F(function(){return A(_FB,[_FL,_FM,new T(function(){return B(_4a(_FI,_FN));})]);});},function(_FO){return new F(function(){return A(_FC,[new T(function(){return B(_4a(_FI,_FO));})]);});}]);});},_FC,function(_FP,_FQ,_FR){var _FS=new T(function(){var _FT=E(_FR),_FU=B(_3P(_FT[1],_FT[2],E(_FQ)[2],_h));return [0,E(_FU[1]),_FU[2]];});return new F(function(){return A(_Fz,[_FQ,_FB,_FC,function(_FV,_FW,_FX){return new F(function(){return A(_FD,[_FV,_FW,new T(function(){return B(_4a(_FS,_FX));})]);});},function(_FY){return new F(function(){return A(_FE,[new T(function(){return B(_4a(_FS,_FY));})]);});}]);});},_FE]);});};},_FZ=function(_G0,_G1,_G2,_G3,_G4){return new F(function(){return _yy(_DQ,_G0,function(_G5,_G6,_G7){return new F(function(){return A(_Fx,[_G5,_G6,_G1,_G2,function(_G8,_G9,_Ga){return new F(function(){return A(_G1,[_G8,_G9,new T(function(){return B(_4a(_G7,_Ga));})]);});},function(_Gb){return new F(function(){return A(_G2,[new T(function(){return B(_4a(_G7,_Gb));})]);});}]);});},_G2,function(_Gc,_Gd,_Ge){return new F(function(){return A(_Fx,[_Gc,_Gd,_G1,_G2,function(_Gf,_Gg,_Gh){return new F(function(){return A(_G3,[_Gf,_Gg,new T(function(){return B(_4a(_Ge,_Gh));})]);});},function(_Gi){return new F(function(){return A(_G4,[new T(function(){return B(_4a(_Ge,_Gi));})]);});}]);});},_G4);});},_Gj=function(_Gk){return E(E(_Gk)[1]);},_Gl=[1,_7K,_h],_Gm=function(_Gn){return E(E(_Gn)[2]);},_Go=function(_Gp){return [0,E(E(_Gp)[2]),_h];},_Gq=function(_Gr,_Gs,_Gt,_Gu){while(1){var _Gv=E(_Gu);if(!_Gv[0]){return [0,_Gr,[0,_Gs],[0,_Gt]];}else{var _Gw=_Gv[2];switch(E(E(_Gv[1])[1])){case 9:var _Gx=(_Gt+8|0)-B(_lF(_Gt-1|0,8))|0;_Gu=_Gw;_Gt=_Gx;continue;case 10:var _Gy=_Gs+1|0;_Gt=1;_Gu=_Gw;_Gs=_Gy;continue;default:var _Gx=_Gt+1|0;_Gu=_Gw;_Gt=_Gx;continue;}}}},_Gz=function(_GA,_GB,_GC,_GD,_GE,_GF,_GG){var _GH=E(_GB);if(!_GH[0]){return new F(function(){return A(_GF,[_h,_GC,new T(function(){return B(_Go(_GC));})]);});}else{var _GI=_GH[1],_GJ=_GH[2],_GK=E(_GC),_GL=E(_GK[2]),_GM=_GL[1],_GN=E(_GL[2])[1],_GO=E(_GL[3])[1],_GP=new T(function(){return B(_Gj(_GA));}),_GQ=[0,E(_GL),[1,[2,E([1,_39,new T(function(){return B(_3b(_GH,_7J));})])],_Gl]],_GR=[2,E([1,_39,new T(function(){return B(_3b(_GH,_7J));})])],_GS=new T(function(){switch(E(E(_GI)[1])){case 9:var _GT=B(_Gq(_GM,_GN,(_GO+8|0)-B(_lF(_GO-1|0,8))|0,_GJ)),_GU=[0,_GT[1],E(_GT[2]),E(_GT[3])];break;case 10:var _GV=B(_Gq(_GM,_GN+1|0,1,_GJ)),_GU=[0,_GV[1],E(_GV[2]),E(_GV[3])];break;default:var _GW=B(_Gq(_GM,_GN,_GO+1|0,_GJ)),_GU=[0,_GW[1],E(_GW[2]),E(_GW[3])];}var _GX=_GU;return _GX;}),_GY=function(_GZ,_H0){var _H1=E(_GZ);if(!_H1[0]){return new F(function(){return A(_GD,[_GH,new T(function(){return [0,_H0,E(E(_GS)),E(_GK[3])];}),new T(function(){return [0,E(E(_GS)),_h];})]);});}else{return new F(function(){return A(new T(function(){return B(_3s(_GP));}),[new T(function(){return B(A(new T(function(){return B(_Gm(_GA));}),[_H0]));}),function(_H2){var _H3=E(_H2);if(!_H3[0]){return E(new T(function(){return B(A(_GE,[_GQ]));}));}else{var _H4=E(_H3[1]),_H5=E(_H4[1]);return E(_H1[1])[1]!=_H5[1]?B(A(_GE,[[0,E(_GL),[1,_GR,[1,[0,E([1,_39,new T(function(){return B(_3b([1,_H5,_h],_7J));})])],_h]]]])):B(_GY(_H1[2],_H4[2]));}}]);});}};return new F(function(){return A(_3s,[_GP,new T(function(){return B(A(_Gm,[_GA,_GK[1]]));}),function(_H6){var _H7=E(_H6);if(!_H7[0]){return E(new T(function(){return B(A(_GG,[_GQ]));}));}else{var _H8=E(_H7[1]),_H9=E(_H8[1]);return E(_GI)[1]!=_H9[1]?B(A(_GG,[[0,E(_GL),[1,_GR,[1,[0,E([1,_39,new T(function(){return B(_3b([1,_H9,_h],_7J));})])],_h]]]])):B(_GY(_GJ,_H8[2]));}}]);});}},_Ha=function(_Hb,_Hc,_Hd,_He,_Hf){return new F(function(){return _Gz(_Bo,_DC,_Hb,function(_Hg,_Hh,_Hi){var _Hj=new T(function(){var _Hk=E(_Hi),_Hl=B(_3P(_Hk[1],_Hk[2],E(_Hh)[2],_h));return [0,E(_Hl[1]),_Hl[2]];});return new F(function(){return _FZ(_Hh,_Hc,_Hd,function(_Hm,_Hn,_Ho){return new F(function(){return A(_Hc,[_Hm,_Hn,new T(function(){return B(_4a(_Hj,_Ho));})]);});},function(_Hp){return new F(function(){return A(_Hd,[new T(function(){return B(_4a(_Hj,_Hp));})]);});});});},_Hf,function(_Hq,_Hr,_Hs){var _Ht=new T(function(){var _Hu=E(_Hs),_Hv=B(_3P(_Hu[1],_Hu[2],E(_Hr)[2],_h));return [0,E(_Hv[1]),_Hv[2]];});return new F(function(){return _FZ(_Hr,_Hc,_Hd,function(_Hw,_Hx,_Hy){return new F(function(){return A(_He,[_Hw,_Hx,new T(function(){return B(_4a(_Ht,_Hy));})]);});},function(_Hz){return new F(function(){return A(_Hf,[new T(function(){return B(_4a(_Ht,_Hz));})]);});});});},_Hf);});},_HA=new T(function(){return B(unCStr("&#x"));}),_HB=function(_HC){var _HD=new T(function(){return _HC<65?_HC<97?false:_HC<=102:_HC>70?_HC<97?false:_HC<=102:true;});return _HC<48?E(_HD):_HC>57?E(_HD):true;},_HE=function(_HF){return new F(function(){return _HB(E(_HF)[1]);});},_HG=function(_HH,_HI,_HJ,_HK,_HL){var _HM=E(_HH),_HN=E(_HM[2]);return new F(function(){return _lM(_1r,_za,_HE,_HM[1],_HN[1],_HN[2],_HN[3],_HM[3],_HI,_HL);});},_HO=new T(function(){return B(unCStr("hexadecimal digit"));}),_HP=[1,_HO,_h],_HQ=function(_HR,_HS,_HT,_HU,_HV){return new F(function(){return _5L(_HG,_HP,_HR,_HS,_HT,_HU,_HV);});},_HW=[0,16],_HX=function(_HY){return [0,B(_Fi(_HW,_HY))];},_HZ=function(_I0){var _I1=new T(function(){return B(_Ew(B(_HX(_I0))));});return function(_I2,_I3,_I4,_I5,_I6){return new F(function(){return A(_zq,[_Bo,_Bn,_I2,function(_I7,_I8,_I9){var _Ia=new T(function(){var _Ib=E(_I9),_Ic=B(_3P(_Ib[1],_Ib[2],E(_I8)[2],_h));return [0,E(_Ic[1]),_Ic[2]];});return new F(function(){return A(_I1,[_I8,_I3,_I4,function(_Id,_Ie,_If){return new F(function(){return A(_I3,[_Id,_Ie,new T(function(){return B(_4a(_Ia,_If));})]);});},function(_Ig){return new F(function(){return A(_I4,[new T(function(){return B(_4a(_Ia,_Ig));})]);});}]);});},_I4,function(_Ih,_Ii,_Ij){var _Ik=new T(function(){var _Il=E(_Ij),_Im=B(_3P(_Il[1],_Il[2],E(_Ii)[2],_h));return [0,E(_Im[1]),_Im[2]];});return new F(function(){return A(_I1,[_Ii,_I3,_I4,function(_In,_Io,_Ip){return new F(function(){return A(_I5,[_In,_Io,new T(function(){return B(_4a(_Ik,_Ip));})]);});},function(_Iq){return new F(function(){return A(_I6,[new T(function(){return B(_4a(_Ik,_Iq));})]);});}]);});},_I6]);});};},_Ir=function(_Is,_It,_Iu,_Iv,_Iw){return new F(function(){return _yy(_HQ,_Is,function(_Ix,_Iy,_Iz){return new F(function(){return A(_HZ,[_Ix,_Iy,_It,_Iu,function(_IA,_IB,_IC){return new F(function(){return A(_It,[_IA,_IB,new T(function(){return B(_4a(_Iz,_IC));})]);});},function(_ID){return new F(function(){return A(_Iu,[new T(function(){return B(_4a(_Iz,_ID));})]);});}]);});},_Iu,function(_IE,_IF,_IG){return new F(function(){return A(_HZ,[_IE,_IF,_It,_Iu,function(_IH,_II,_IJ){return new F(function(){return A(_Iv,[_IH,_II,new T(function(){return B(_4a(_IG,_IJ));})]);});},function(_IK){return new F(function(){return A(_Iw,[new T(function(){return B(_4a(_IG,_IK));})]);});}]);});},_Iw);});},_IL=function(_IM,_IN,_IO,_IP,_IQ){return new F(function(){return _Gz(_Bo,_HA,_IM,function(_IR,_IS,_IT){var _IU=new T(function(){var _IV=E(_IT),_IW=B(_3P(_IV[1],_IV[2],E(_IS)[2],_h));return [0,E(_IW[1]),_IW[2]];});return new F(function(){return _Ir(_IS,_IN,_IO,function(_IX,_IY,_IZ){return new F(function(){return A(_IN,[_IX,_IY,new T(function(){return B(_4a(_IU,_IZ));})]);});},function(_J0){return new F(function(){return A(_IO,[new T(function(){return B(_4a(_IU,_J0));})]);});});});},_IQ,function(_J1,_J2,_J3){var _J4=new T(function(){var _J5=E(_J3),_J6=B(_3P(_J5[1],_J5[2],E(_J2)[2],_h));return [0,E(_J6[1]),_J6[2]];});return new F(function(){return _Ir(_J2,_IN,_IO,function(_J7,_J8,_J9){return new F(function(){return A(_IP,[_J7,_J8,new T(function(){return B(_4a(_J4,_J9));})]);});},function(_Ja){return new F(function(){return A(_IQ,[new T(function(){return B(_4a(_J4,_Ja));})]);});});});},_IQ);});},_Jb=function(_Jc,_Jd,_Je,_Jf,_Jg){var _Jh=function(_Ji,_Jj,_Jk){var _Jl=new T(function(){var _Jm=E(_Jk),_Jn=E(_Jm[2]);if(!_Jn[0]){var _Jo=E(_Jm);}else{var _Jp=B(_5q(_Jm[1],_Jn,_DB)),_Jo=[0,E(_Jp[1]),_Jp[2]];}var _Jq=_Jo;return _Jq;});return new F(function(){return A(_Dr,[_Ji,_Jj,_Jd,_Je,function(_Jr,_Js,_Jt){return new F(function(){return A(_Jf,[_Jr,_Js,new T(function(){return B(_4a(_Jl,_Jt));})]);});},function(_Ju){return new F(function(){return A(_Jg,[new T(function(){return B(_4a(_Jl,_Ju));})]);});}]);});},_Jv=function(_Jw,_Jx,_Jy){return new F(function(){return A(_Dr,[_Jw,_Jx,_Jd,_Je,function(_Jz,_JA,_JB){return new F(function(){return A(_Jd,[_Jz,_JA,new T(function(){return B(_4a(_Jy,_JB));})]);});},function(_JC){return new F(function(){return A(_Je,[new T(function(){return B(_4a(_Jy,_JC));})]);});}]);});};return new F(function(){return _IL(_Jc,_Jv,_Je,_Jh,function(_JD){return new F(function(){return _Ha(_Jc,_Jv,_Je,function(_JE,_JF,_JG){return new F(function(){return _Jh(_JE,_JF,new T(function(){var _JH=E(_JD),_JI=E(_JG),_JJ=B(_3P(_JH[1],_JH[2],_JI[1],_JI[2]));return [0,E(_JJ[1]),_JJ[2]];}));});},function(_JK){return new F(function(){return A(_Jg,[new T(function(){var _JL=E(_JD),_JM=E(_JK),_JN=B(_3P(_JL[1],_JL[2],_JM[1],_JM[2])),_JO=B(_5q(_JN[1],_JN[2],_DB));return [0,E(_JO[1]),_JO[2]];})]);});});});});});},_JP=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_Jb,_Dq,_B3,_B4,_B5,_B6,_zb);});},_JQ=function(_JR){var _JS=E(_JR)[1];if(_JS>>>0>1114111){return new F(function(){return _DW(_JS);});}else{return [0,_JS];}},_JT=function(_JU,_JV,_JW,_){while(1){var _JX=(function(_JY,_JZ,_K0,_){if(!addrEq(_JZ,_JY)){var _K1=readOffAddr("w8",1,_JZ,0),_K2=_K1,_K3=_JY,_K4=plusAddr(_JZ,-1),_K5=[1,new T(function(){return [0,_K2&4294967295];}),_K0];_JU=_K3;_JV=_K4;_JW=_K5;return null;}else{return _K0;}})(_JU,_JV,_JW,_);if(_JX!=null){return _JX;}}},_K6=function(_K7,_K8,_K9,_Ka,_Kb){if(_Ka>100){var _Kc=B(_JT(plusAddr(_K7,_K9-1|0),plusAddr(_K7,(_K9-1|0)+100|0),new T(function(){return B(_K6(_K7,_K8,_K9+100|0,_Ka-100|0,_Kb));}),_)),_Kd=_Kc,_=0;return E(_Kd);}else{var _Ke=B(_JT(plusAddr(_K7,_K9-1|0),plusAddr(_K7,(_K9-1|0)+_Ka|0),_Kb,_)),_Kf=_Ke,_=0;return E(_Kf);}},_Kg=function(_Kh){var _Ki=E(_Kh);if(!_Ki[0]){return [0];}else{return new F(function(){return _K6(_Ki[1],_Ki[2],_Ki[3],_Ki[4],new T(function(){return B(_Kg(_Ki[5]));}));});}},_Kj=function(_Kk,_Kl){var _Km=E(_Kk),_Kn=_Km[1],_Ko=new T(function(){var _Kp=E(_Kn);switch(_Kp[0]){case 0:var _Kq=B(_0(_Kp[1],new T(function(){var _Kr=E(E(_Kl)[1]);switch(_Kr[0]){case 0:var _Ks=E(_Kr[1]);break;case 1:var _Ks=B(_Kg(_Kr[1]));break;default:var _Ks=E(_vE);}var _Kt=_Ks;return _Kt;})));break;case 1:var _Kq=B(_0(B(_Kg(_Kp[1])),new T(function(){var _Ku=E(E(_Kl)[1]);switch(_Ku[0]){case 0:var _Kv=E(_Ku[1]);break;case 1:var _Kv=B(_Kg(_Ku[1]));break;default:var _Kv=E(_vE);}var _Kw=_Kv;return _Kw;})));break;default:var _Kq=E(_vE);}return _Kq;}),_Kx=new T(function(){var _Ky=E(_Kl);switch(E(_Ky[1])[0]){case 0:var _Kz=[1,[0,[0,_Ko],_h],_h];break;case 1:var _Kz=[1,[0,[0,_Ko],_h],_h];break;default:var _Kz=[1,_Km,[1,_Ky,_h]];}var _KA=_Kz;return _KA;});switch(E(_Kn)[0]){case 0:return E(_Kx);case 1:return E(_Kx);default:return [1,_Km,[1,_Kl,_h]];}},_KB=function(_KC,_KD){var _KE=E(_KC),_KF=new T(function(){var _KG=E(_KD),_KH=E(_KG[1]);switch(_KH[0]){case 2:var _KI=[0,[0,[1,new T(function(){return B(_JQ(_KH[1]));}),_h]],_h];break;case 5:var _KI=[0,[0,_KH[1]],_h];break;default:var _KI=E(_KG);}var _KJ=_KI;return _KJ;}),_KK=E(_KE[1]);switch(_KK[0]){case 2:return new F(function(){return _Kj([0,[0,[1,new T(function(){return B(_JQ(_KK[1]));}),_h]],_h],_KF);});break;case 5:return new F(function(){return _Kj([0,[0,_KK[1]],_h],_KF);});break;default:return new F(function(){return _Kj(_KE,_KF);});}},_KL=function(_KM){var _KN=E(_KM);if(!_KN[0]){return [0];}else{var _KO=_KN[1],_KP=B(_KL(_KN[2]));if(!_KP[0]){return [1,_KO,_h];}else{return new F(function(){return _0(B(_KB(_KO,_KP[1])),_KP[2]);});}}},_KQ=function(_KR){var _KS=E(_KR);if(!_KS[0]){return [0];}else{var _KT=_KS[1],_KU=B(_KQ(_KS[2]));if(!_KU[0]){return [1,_KT,_h];}else{return new F(function(){return _0(B(_KB(_KT,_KU[1])),_KU[2]);});}}},_KV=function(_KW,_KX,_KY,_KZ,_L0,_L1){return new F(function(){return _lc(function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_JP,function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _AW(_KW,_B3,_B4,_B5,_B6,_zb);});},_B3,_B4,_B5,_B6,_zb);});},_KX,function(_L2){return new F(function(){return A(_KY,[new T(function(){return B(_KQ(_L2));})]);});},_KZ,function(_L3){return new F(function(){return A(_L0,[new T(function(){return B(_KL(_L3));})]);});},_L1);});},_L4=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _KV(_yx,_B3,_B4,_B5,_B6,_zb);});},_L5=new T(function(){return B(unCStr("<&\'"));}),_L6=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _KV(_L5,_B3,_B4,_B5,_B6,_zb);});},_L7=[0,34],_L8=new T(function(){return B(_zq(_Bo,_L7));}),_L9=function(_La,_Lb,_Lc,_Ld,_Le){return new F(function(){return A(_L8,[_La,function(_Lf,_Lg,_Lh){return new F(function(){return A(_Lb,[_G,_Lg,new T(function(){var _Li=E(_Lh),_Lj=B(_3P(_Li[1],_Li[2],E(_Lg)[2],_h));return [0,E(_Lj[1]),_Lj[2]];})]);});},_Lc,function(_Lk,_Ll,_Lm){return new F(function(){return A(_Ld,[_G,_Ll,new T(function(){var _Ln=E(_Lm),_Lo=B(_3P(_Ln[1],_Ln[2],E(_Ll)[2],_h));return [0,E(_Lo[1]),_Lo[2]];})]);});},_Le]);});},_Lp=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _L9(_B3,_B4,_B5,_B6,_zb);});},_Lq=function(_Lr){var _Ls=E(E(_Lr)[1]);return _Ls[0]==9?E(_Ls[1]):E(_vE);},_Lt=[1,_39,_h],_Lu=new T(function(){return B(unCStr(" occurs twice in attribute list"));}),_Lv=[0,61],_Lw=function(_Lx,_Ly){var _Lz=E(_Ly);return _Lz[0]==0?[0]:[1,new T(function(){return B(A(_Lx,[_Lz[1]]));}),new T(function(){return B(_Lw(_Lx,_Lz[2]));})];},_LA=new T(function(){return B(_wN("src/Text/XML/HXT/DOM/QualifiedName.hs:329:5-36|(px, (_ : lp))"));}),_LB=function(_LC){return E(E(_LC)[1])==58?false:true;},_LD=function(_6X){return new F(function(){return _LB(_6X);});},_LE=function(_LF,_LG){while(1){var _LH=E(_LF),_LI=E(_LG);if(!_LI[0]){switch(B(_3H(_LH,_LI[2]))){case 0:_LF=_LH;_LG=_LI[4];continue;case 1:return [1,_LI[3]];default:_LF=_LH;_LG=_LI[5];continue;}}else{return [0];}}},_LJ=[1],_LK=new T(function(){return B(unCStr("Failure in Data.Map.balanceL"));}),_LL=new T(function(){return B(err(_LK));}),_LM=function(_LN,_LO,_LP,_LQ){var _LR=E(_LQ);if(!_LR[0]){var _LS=_LR[1],_LT=E(_LP);if(!_LT[0]){var _LU=_LT[1],_LV=_LT[2],_LW=_LT[3];if(_LU<=(imul(3,_LS)|0)){return [0,(1+_LU|0)+_LS|0,E(E(_LN)),_LO,E(_LT),E(_LR)];}else{var _LX=E(_LT[4]);if(!_LX[0]){var _LY=_LX[1],_LZ=E(_LT[5]);if(!_LZ[0]){var _M0=_LZ[1],_M1=_LZ[2],_M2=_LZ[3],_M3=_LZ[4];if(_M0>=(imul(2,_LY)|0)){var _M4=function(_M5){var _M6=E(_LZ[5]);return _M6[0]==0?[0,(1+_LU|0)+_LS|0,E(_M1),_M2,E([0,(1+_LY|0)+_M5|0,E(_LV),_LW,E(_LX),E(_M3)]),E([0,(1+_LS|0)+_M6[1]|0,E(E(_LN)),_LO,E(_M6),E(_LR)])]:[0,(1+_LU|0)+_LS|0,E(_M1),_M2,E([0,(1+_LY|0)+_M5|0,E(_LV),_LW,E(_LX),E(_M3)]),E([0,1+_LS|0,E(E(_LN)),_LO,E(_LJ),E(_LR)])];},_M7=E(_M3);return _M7[0]==0?B(_M4(_M7[1])):B(_M4(0));}else{return [0,(1+_LU|0)+_LS|0,E(_LV),_LW,E(_LX),E([0,(1+_LS|0)+_M0|0,E(E(_LN)),_LO,E(_LZ),E(_LR)])];}}else{return E(_LL);}}else{return E(_LL);}}}else{return [0,1+_LS|0,E(E(_LN)),_LO,E(_LJ),E(_LR)];}}else{var _M8=E(_LP);if(!_M8[0]){var _M9=_M8[1],_Ma=_M8[2],_Mb=_M8[3],_Mc=_M8[5],_Md=E(_M8[4]);if(!_Md[0]){var _Me=_Md[1],_Mf=E(_Mc);if(!_Mf[0]){var _Mg=_Mf[1],_Mh=_Mf[2],_Mi=_Mf[3],_Mj=_Mf[4];if(_Mg>=(imul(2,_Me)|0)){var _Mk=function(_Ml){var _Mm=E(_Mf[5]);return _Mm[0]==0?[0,1+_M9|0,E(_Mh),_Mi,E([0,(1+_Me|0)+_Ml|0,E(_Ma),_Mb,E(_Md),E(_Mj)]),E([0,1+_Mm[1]|0,E(E(_LN)),_LO,E(_Mm),E(_LJ)])]:[0,1+_M9|0,E(_Mh),_Mi,E([0,(1+_Me|0)+_Ml|0,E(_Ma),_Mb,E(_Md),E(_Mj)]),E([0,1,E(E(_LN)),_LO,E(_LJ),E(_LJ)])];},_Mn=E(_Mj);return _Mn[0]==0?B(_Mk(_Mn[1])):B(_Mk(0));}else{return [0,1+_M9|0,E(_Ma),_Mb,E(_Md),E([0,1+_Mg|0,E(E(_LN)),_LO,E(_Mf),E(_LJ)])];}}else{return [0,3,E(_Ma),_Mb,E(_Md),E([0,1,E(E(_LN)),_LO,E(_LJ),E(_LJ)])];}}else{var _Mo=E(_Mc);return _Mo[0]==0?[0,3,E(_Mo[2]),_Mo[3],E([0,1,E(_Ma),_Mb,E(_LJ),E(_LJ)]),E([0,1,E(E(_LN)),_LO,E(_LJ),E(_LJ)])]:[0,2,E(E(_LN)),_LO,E(_M8),E(_LJ)];}}else{return [0,1,E(E(_LN)),_LO,E(_LJ),E(_LJ)];}}},_Mp=new T(function(){return B(unCStr("Failure in Data.Map.balanceR"));}),_Mq=new T(function(){return B(err(_Mp));}),_Mr=function(_Ms,_Mt,_Mu,_Mv){var _Mw=E(_Mu);if(!_Mw[0]){var _Mx=_Mw[1],_My=E(_Mv);if(!_My[0]){var _Mz=_My[1],_MA=_My[2],_MB=_My[3];if(_Mz<=(imul(3,_Mx)|0)){return [0,(1+_Mx|0)+_Mz|0,E(E(_Ms)),_Mt,E(_Mw),E(_My)];}else{var _MC=E(_My[4]);if(!_MC[0]){var _MD=_MC[1],_ME=_MC[2],_MF=_MC[3],_MG=_MC[4],_MH=E(_My[5]);if(!_MH[0]){var _MI=_MH[1];if(_MD>=(imul(2,_MI)|0)){var _MJ=function(_MK){var _ML=E(_Ms),_MM=E(_MC[5]);return _MM[0]==0?[0,(1+_Mx|0)+_Mz|0,E(_ME),_MF,E([0,(1+_Mx|0)+_MK|0,E(_ML),_Mt,E(_Mw),E(_MG)]),E([0,(1+_MI|0)+_MM[1]|0,E(_MA),_MB,E(_MM),E(_MH)])]:[0,(1+_Mx|0)+_Mz|0,E(_ME),_MF,E([0,(1+_Mx|0)+_MK|0,E(_ML),_Mt,E(_Mw),E(_MG)]),E([0,1+_MI|0,E(_MA),_MB,E(_LJ),E(_MH)])];},_MN=E(_MG);return _MN[0]==0?B(_MJ(_MN[1])):B(_MJ(0));}else{return [0,(1+_Mx|0)+_Mz|0,E(_MA),_MB,E([0,(1+_Mx|0)+_MD|0,E(E(_Ms)),_Mt,E(_Mw),E(_MC)]),E(_MH)];}}else{return E(_Mq);}}else{return E(_Mq);}}}else{return [0,1+_Mx|0,E(E(_Ms)),_Mt,E(_Mw),E(_LJ)];}}else{var _MO=E(_Mv);if(!_MO[0]){var _MP=_MO[1],_MQ=_MO[2],_MR=_MO[3],_MS=_MO[5],_MT=E(_MO[4]);if(!_MT[0]){var _MU=_MT[1],_MV=_MT[2],_MW=_MT[3],_MX=_MT[4],_MY=E(_MS);if(!_MY[0]){var _MZ=_MY[1];if(_MU>=(imul(2,_MZ)|0)){var _N0=function(_N1){var _N2=E(_Ms),_N3=E(_MT[5]);return _N3[0]==0?[0,1+_MP|0,E(_MV),_MW,E([0,1+_N1|0,E(_N2),_Mt,E(_LJ),E(_MX)]),E([0,(1+_MZ|0)+_N3[1]|0,E(_MQ),_MR,E(_N3),E(_MY)])]:[0,1+_MP|0,E(_MV),_MW,E([0,1+_N1|0,E(_N2),_Mt,E(_LJ),E(_MX)]),E([0,1+_MZ|0,E(_MQ),_MR,E(_LJ),E(_MY)])];},_N4=E(_MX);return _N4[0]==0?B(_N0(_N4[1])):B(_N0(0));}else{return [0,1+_MP|0,E(_MQ),_MR,E([0,1+_MU|0,E(E(_Ms)),_Mt,E(_LJ),E(_MT)]),E(_MY)];}}else{return [0,3,E(_MV),_MW,E([0,1,E(E(_Ms)),_Mt,E(_LJ),E(_LJ)]),E([0,1,E(_MQ),_MR,E(_LJ),E(_LJ)])];}}else{var _N5=E(_MS);return _N5[0]==0?[0,3,E(_MQ),_MR,E([0,1,E(E(_Ms)),_Mt,E(_LJ),E(_LJ)]),E(_N5)]:[0,2,E(E(_Ms)),_Mt,E(_LJ),E(_MO)];}}else{return [0,1,E(E(_Ms)),_Mt,E(_LJ),E(_LJ)];}}},_N6=function(_N7,_N8,_N9){var _Na=E(_N7),_Nb=E(_N9);if(!_Nb[0]){var _Nc=_Nb[2],_Nd=_Nb[3],_Ne=_Nb[4],_Nf=_Nb[5];switch(B(_3H(_Na,_Nc))){case 0:return new F(function(){return _LM(_Nc,_Nd,B(_N6(_Na,_N8,_Ne)),_Nf);});break;case 1:return [0,_Nb[1],E(_Na),_N8,E(_Ne),E(_Nf)];default:return new F(function(){return _Mr(_Nc,_Nd,_Ne,B(_N6(_Na,_N8,_Nf)));});}}else{return [0,1,E(_Na),_N8,E(_LJ),E(_LJ)];}},_Ng=function(_Nh,_Ni){while(1){var _Nj=E(_Ni);if(!_Nj[0]){return 0;}else{var _Nk=B(A(_Nh,[_Nj[1]]));_Ni=_Nj[2];continue;}}},_Nl=function(_Nm){var _Nn=E(_Nm);return 0;},_No=function(_Np,_Nq){while(1){var _Nr=E(_Nq);if(!_Nr[0]){return E(_Np);}else{var _Ns=E(_Nr[1]),_Nt=B(_N6(_Ns[1],_Ns[2],_Np));_Nq=_Nr[2];_Np=_Nt;continue;}}},_Nu=function(_Nv,_Nw){return [0,1,E(E(_Nv)),_Nw,E(_LJ),E(_LJ)];},_Nx=function(_Ny,_Nz,_NA){var _NB=E(_NA);if(!_NB[0]){return new F(function(){return _Mr(_NB[2],_NB[3],_NB[4],B(_Nx(_Ny,_Nz,_NB[5])));});}else{return new F(function(){return _Nu(_Ny,_Nz);});}},_NC=function(_ND,_NE,_NF){var _NG=E(_NF);if(!_NG[0]){return new F(function(){return _LM(_NG[2],_NG[3],B(_NC(_ND,_NE,_NG[4])),_NG[5]);});}else{return new F(function(){return _Nu(_ND,_NE);});}},_NH=function(_NI,_NJ,_NK,_NL,_NM,_NN,_NO,_NP){var _NQ=E(_NK);if(!_NQ[0]){var _NR=_NQ[1],_NS=_NQ[2],_NT=_NQ[3],_NU=_NQ[4],_NV=_NQ[5];if((imul(3,_NR)|0)>=_NL){if((imul(3,_NL)|0)>=_NR){return [0,(_NR+_NL|0)+1|0,E(E(_NI)),_NJ,E(_NQ),E([0,_NL,E(_NM),_NN,E(_NO),E(_NP)])];}else{return new F(function(){return _Mr(_NS,_NT,_NU,B(_NH(_NI,_NJ,_NV,_NL,_NM,_NN,_NO,_NP)));});}}else{return new F(function(){return _LM(_NM,_NN,B(_NW(_NI,_NJ,_NR,_NS,_NT,_NU,_NV,_NO)),_NP);});}}else{return new F(function(){return _NC(_NI,_NJ,[0,_NL,E(_NM),_NN,E(_NO),E(_NP)]);});}},_NW=function(_NX,_NY,_NZ,_O0,_O1,_O2,_O3,_O4){var _O5=E(_O4);if(!_O5[0]){var _O6=_O5[1],_O7=_O5[2],_O8=_O5[3],_O9=_O5[4],_Oa=_O5[5];if((imul(3,_NZ)|0)>=_O6){if((imul(3,_O6)|0)>=_NZ){return [0,(_NZ+_O6|0)+1|0,E(E(_NX)),_NY,E([0,_NZ,E(_O0),_O1,E(_O2),E(_O3)]),E(_O5)];}else{return new F(function(){return _Mr(_O0,_O1,_O2,B(_NH(_NX,_NY,_O3,_O6,_O7,_O8,_O9,_Oa)));});}}else{return new F(function(){return _LM(_O7,_O8,B(_NW(_NX,_NY,_NZ,_O0,_O1,_O2,_O3,_O9)),_Oa);});}}else{return new F(function(){return _Nx(_NX,_NY,[0,_NZ,E(_O0),_O1,E(_O2),E(_O3)]);});}},_Ob=function(_Oc,_Od,_Oe,_Of){var _Og=E(_Oe);if(!_Og[0]){var _Oh=_Og[1],_Oi=_Og[2],_Oj=_Og[3],_Ok=_Og[4],_Ol=_Og[5],_Om=E(_Of);if(!_Om[0]){var _On=_Om[1],_Oo=_Om[2],_Op=_Om[3],_Oq=_Om[4],_Or=_Om[5];if((imul(3,_Oh)|0)>=_On){if((imul(3,_On)|0)>=_Oh){return [0,(_Oh+_On|0)+1|0,E(E(_Oc)),_Od,E(_Og),E(_Om)];}else{return new F(function(){return _Mr(_Oi,_Oj,_Ok,B(_NH(_Oc,_Od,_Ol,_On,_Oo,_Op,_Oq,_Or)));});}}else{return new F(function(){return _LM(_Oo,_Op,B(_NW(_Oc,_Od,_Oh,_Oi,_Oj,_Ok,_Ol,_Oq)),_Or);});}}else{return new F(function(){return _Nx(_Oc,_Od,_Og);});}}else{return new F(function(){return _NC(_Oc,_Od,_Of);});}},_Os=function(_Ot,_Ou){var _Ov=E(_Ou);if(!_Ov[0]){return [0,_LJ,_h,_h];}else{var _Ow=E(_Ot);if(_Ow==1){var _Ox=E(_Ov[1]),_Oy=_Ox[1],_Oz=_Ox[2],_OA=E(_Ov[2]);return _OA[0]==0?[0,new T(function(){return [0,1,E(E(_Oy)),_Oz,E(_LJ),E(_LJ)];}),_h,_h]:B(_3H(_Oy,E(_OA[1])[1]))==0?[0,new T(function(){return [0,1,E(E(_Oy)),_Oz,E(_LJ),E(_LJ)];}),_OA,_h]:[0,new T(function(){return [0,1,E(E(_Oy)),_Oz,E(_LJ),E(_LJ)];}),_h,_OA];}else{var _OB=B(_Os(_Ow>>1,_Ov)),_OC=_OB[1],_OD=_OB[3],_OE=E(_OB[2]);if(!_OE[0]){return [0,_OC,_h,_OD];}else{var _OF=E(_OE[1]),_OG=_OF[1],_OH=_OF[2],_OI=E(_OE[2]);if(!_OI[0]){return [0,new T(function(){return B(_Nx(_OG,_OH,_OC));}),_h,_OD];}else{if(!B(_3H(_OG,E(_OI[1])[1]))){var _OJ=B(_Os(_Ow>>1,_OI));return [0,new T(function(){return B(_Ob(_OG,_OH,_OC,_OJ[1]));}),_OJ[2],_OJ[3]];}else{return [0,_OC,_h,_OE];}}}}}},_OK=function(_OL,_OM,_ON){while(1){var _OO=E(_ON);if(!_OO[0]){return E(_OM);}else{var _OP=E(_OO[1]),_OQ=_OP[1],_OR=_OP[2],_OS=E(_OO[2]);if(!_OS[0]){return new F(function(){return _Nx(_OQ,_OR,_OM);});}else{if(!B(_3H(_OQ,E(_OS[1])[1]))){var _OT=B(_Os(_OL,_OS)),_OU=_OT[1],_OV=E(_OT[3]);if(!_OV[0]){var _OW=_OL<<1,_OX=B(_Ob(_OQ,_OR,_OM,_OU));_ON=_OT[2];_OL=_OW;_OM=_OX;continue;}else{return new F(function(){return _No(B(_Ob(_OQ,_OR,_OM,_OU)),_OV);});}}else{return new F(function(){return _No(_OM,_OO);});}}}}},_OY=function(_OZ){var _P0=E(_OZ);if(!_P0[0]){return [1];}else{var _P1=E(_P0[1]),_P2=_P1[1],_P3=_P1[2],_P4=E(_P0[2]);if(!_P4[0]){return [0,1,E(E(_P2)),_P3,E(_LJ),E(_LJ)];}else{if(!B(_3H(_P2,E(_P4[1])[1]))){return new F(function(){return _OK(1,[0,1,E(E(_P2)),_P3,E(_LJ),E(_LJ)],_P4);});}else{return new F(function(){return _No([0,1,E(E(_P2)),_P3,E(_LJ),E(_LJ)],_P4);});}}}},_P5=function(_P6,_P7,_P8){var _P9=E(_P6),_Pa=E(_P8);if(!_Pa[0]){var _Pb=_Pa[3],_Pc=_Pa[4],_Pd=_Pa[5],_Pe=E(_Pa[2]);switch(B(_3H(E(_P9[1])[2],E(_Pe[1])[2]))){case 0:return new F(function(){return _LM(_Pe,_Pb,B(_P5(_P9,_P7,_Pc)),_Pd);});break;case 1:switch(B(_3H(E(_P9[2])[2],E(_Pe[2])[2]))){case 0:return new F(function(){return _LM(_Pe,_Pb,B(_P5(_P9,_P7,_Pc)),_Pd);});break;case 1:switch(B(_3H(E(_P9[3])[2],E(_Pe[3])[2]))){case 0:return new F(function(){return _LM(_Pe,_Pb,B(_P5(_P9,_P7,_Pc)),_Pd);});break;case 1:return [0,_Pa[1],E(_P9),_P7,E(_Pc),E(_Pd)];default:return new F(function(){return _Mr(_Pe,_Pb,_Pc,B(_P5(_P9,_P7,_Pd)));});}break;default:return new F(function(){return _Mr(_Pe,_Pb,_Pc,B(_P5(_P9,_P7,_Pd)));});}break;default:return new F(function(){return _Mr(_Pe,_Pb,_Pc,B(_P5(_P9,_P7,_Pd)));});}}else{return [0,1,E(_P9),_P7,E(_LJ),E(_LJ)];}},_Pf=function(_Pg,_Ph){while(1){var _Pi=E(_Ph);if(!_Pi[0]){return E(_Pg);}else{var _Pj=E(_Pi[1]),_Pk=B(_P5(_Pj[1],_Pj[2],_Pg));_Ph=_Pi[2];_Pg=_Pk;continue;}}},_Pl=function(_Pm,_Pn){var _Po=E(_Pn);if(!_Po[0]){return [0,_LJ,_h,_h];}else{var _Pp=E(_Pm);if(_Pp==1){var _Pq=E(_Po[1]),_Pr=_Pq[1],_Ps=_Pq[2],_Pt=E(_Po[2]);if(!_Pt[0]){return [0,new T(function(){return [0,1,E(E(_Pr)),_Ps,E(_LJ),E(_LJ)];}),_h,_h];}else{var _Pu=E(_Pr),_Pv=E(E(_Pt[1])[1]);switch(B(_3H(E(_Pu[1])[2],E(_Pv[1])[2]))){case 0:return [0,[0,1,E(_Pu),_Ps,E(_LJ),E(_LJ)],_Pt,_h];case 1:switch(B(_3H(E(_Pu[2])[2],E(_Pv[2])[2]))){case 0:return [0,[0,1,E(_Pu),_Ps,E(_LJ),E(_LJ)],_Pt,_h];case 1:return B(_3H(E(_Pu[3])[2],E(_Pv[3])[2]))==0?[0,[0,1,E(_Pu),_Ps,E(_LJ),E(_LJ)],_Pt,_h]:[0,[0,1,E(_Pu),_Ps,E(_LJ),E(_LJ)],_h,_Pt];default:return [0,[0,1,E(_Pu),_Ps,E(_LJ),E(_LJ)],_h,_Pt];}break;default:return [0,[0,1,E(_Pu),_Ps,E(_LJ),E(_LJ)],_h,_Pt];}}}else{var _Pw=B(_Pl(_Pp>>1,_Po)),_Px=_Pw[1],_Py=_Pw[3],_Pz=E(_Pw[2]);if(!_Pz[0]){return [0,_Px,_h,_Py];}else{var _PA=E(_Pz[1]),_PB=_PA[1],_PC=_PA[2],_PD=E(_Pz[2]);if(!_PD[0]){return [0,new T(function(){return B(_Nx(_PB,_PC,_Px));}),_h,_Py];}else{var _PE=E(_PB),_PF=E(E(_PD[1])[1]);switch(B(_3H(E(_PE[1])[2],E(_PF[1])[2]))){case 0:var _PG=B(_Pl(_Pp>>1,_PD));return [0,new T(function(){return B(_Ob(_PE,_PC,_Px,_PG[1]));}),_PG[2],_PG[3]];case 1:switch(B(_3H(E(_PE[2])[2],E(_PF[2])[2]))){case 0:var _PH=B(_Pl(_Pp>>1,_PD));return [0,new T(function(){return B(_Ob(_PE,_PC,_Px,_PH[1]));}),_PH[2],_PH[3]];case 1:if(!B(_3H(E(_PE[3])[2],E(_PF[3])[2]))){var _PI=B(_Pl(_Pp>>1,_PD));return [0,new T(function(){return B(_Ob(_PE,_PC,_Px,_PI[1]));}),_PI[2],_PI[3]];}else{return [0,_Px,_h,_Pz];}break;default:return [0,_Px,_h,_Pz];}break;default:return [0,_Px,_h,_Pz];}}}}}},_PJ=function(_PK,_PL,_PM){var _PN=E(_PM);if(!_PN[0]){return E(_PL);}else{var _PO=E(_PN[1]),_PP=_PO[1],_PQ=_PO[2],_PR=E(_PN[2]);if(!_PR[0]){return new F(function(){return _Nx(_PP,_PQ,_PL);});}else{var _PS=E(_PP),_PT=E(E(_PR[1])[1]),_PU=new T(function(){var _PV=B(_Pl(_PK,_PR)),_PW=_PV[1],_PX=E(_PV[3]);if(!_PX[0]){var _PY=B(_PJ(_PK<<1,B(_Ob(_PS,_PQ,_PL,_PW)),_PV[2]));}else{var _PY=B(_Pf(B(_Ob(_PS,_PQ,_PL,_PW)),_PX));}var _PZ=_PY;return _PZ;});switch(B(_3H(E(_PS[1])[2],E(_PT[1])[2]))){case 0:return E(_PU);case 1:switch(B(_3H(E(_PS[2])[2],E(_PT[2])[2]))){case 0:return E(_PU);case 1:return B(_3H(E(_PS[3])[2],E(_PT[3])[2]))==0?E(_PU):B(_Pf(_PL,_PN));default:return new F(function(){return _Pf(_PL,_PN);});}break;default:return new F(function(){return _Pf(_PL,_PN);});}}}},_Q0=function(_Q1){var _Q2=E(_Q1);if(!_Q2[0]){return [1];}else{var _Q3=E(_Q2[1]),_Q4=_Q3[1],_Q5=_Q3[2],_Q6=E(_Q2[2]);if(!_Q6[0]){return [0,1,E(E(_Q4)),_Q5,E(_LJ),E(_LJ)];}else{var _Q7=E(_Q4),_Q8=E(E(_Q6[1])[1]);switch(B(_3H(E(_Q7[1])[2],E(_Q8[1])[2]))){case 0:return new F(function(){return _PJ(1,[0,1,E(_Q7),_Q5,E(_LJ),E(_LJ)],_Q6);});break;case 1:switch(B(_3H(E(_Q7[2])[2],E(_Q8[2])[2]))){case 0:return new F(function(){return _PJ(1,[0,1,E(_Q7),_Q5,E(_LJ),E(_LJ)],_Q6);});break;case 1:return B(_3H(E(_Q7[3])[2],E(_Q8[3])[2]))==0?B(_PJ(1,[0,1,E(_Q7),_Q5,E(_LJ),E(_LJ)],_Q6)):B(_Pf([0,1,E(_Q7),_Q5,E(_LJ),E(_LJ)],_Q6));default:return new F(function(){return _Pf([0,1,E(_Q7),_Q5,E(_LJ),E(_LJ)],_Q6);});}break;default:return new F(function(){return _Pf([0,1,E(_Q7),_Q5,E(_LJ),E(_LJ)],_Q6);});}}}},_Q9=new T(function(){return E(E(_x5)[1]);}),_Qa=new T(function(){return E(E(_x5)[3]);}),_Qb=new T(function(){return E(E(_x5)[4]);}),_Qc=new T(function(){return [0,E(E(_Qb)),E(E(_xf)),E(E(_Qa))];}),_Qd=new T(function(){var _Qe=E(_Qc);return [0,[0,_Qe[1],_Qe[2],_Qe[3]],_Qe];}),_Qf=[1,_Qd,_h],_Qg=function(_Qh){return [0,new T(function(){return E(E(_Qh)[2]);}),_Qh];},_Qi=new T(function(){return B(_Lw(_Qg,_Q9));}),_Qj=new T(function(){return [0,E([0,B(_E2(_Q9,0))]),E(B(_OY(_Qi))),E(B(_Q0(_Qf)))];}),_Qk=function(_){var _=0,_Ql=nMV(_Qj),_Qm=_Ql;return [0,_Qm];},_Qn=new T(function(){return B(_t(_Qk));}),_Qo=function(_Qp,_Qq){return new F(function(){return _t(function(_){var _=0;return new F(function(){return mMV(E(_Qn)[1],function(_Qr){var _Qs=B(A(_Qq,[_Qr])),_Qt=E(_Qs[1]);return E(_Qs);});});});});},_Qu=function(_Qv,_Qw,_Qx,_Qy){while(1){var _Qz=E(_Qy);if(!_Qz[0]){var _QA=_Qz[4],_QB=_Qz[5],_QC=E(_Qz[2]),_QD=E(_Qv);switch(B(_3H(_QD[2],E(_QC[1])[2]))){case 0:_Qv=_QD;_Qy=_QA;continue;case 1:var _QE=E(_Qw);switch(B(_3H(_QE[2],E(_QC[2])[2]))){case 0:_Qv=_QD;_Qw=_QE;_Qy=_QA;continue;case 1:var _QF=E(_Qx);switch(B(_3H(_QF[2],E(_QC[3])[2]))){case 0:_Qv=_QD;_Qw=_QE;_Qx=_QF;_Qy=_QA;continue;case 1:return [1,_Qz[3]];default:_Qv=_QD;_Qw=_QE;_Qx=_QF;_Qy=_QB;continue;}break;default:_Qv=_QD;_Qw=_QE;_Qy=_QB;continue;}break;default:_Qv=_QD;_Qy=_QB;continue;}}else{return [0];}}},_QG=function(_QH,_QI){var _QJ=E(_QH),_QK=_QJ[3],_QL=B(_Qu(_QI,_xf,_xf,_QK));if(!_QL[0]){var _QM=E(_QI),_QN=E(_xf),_QO=[0,E(_QM),E(_QN),E(_QN)];return [0,new T(function(){return [0,E(_QJ[1]),E(_QJ[2]),E(B(_P5([0,_QM,_QN,_QN],_QO,_QK)))];}),_QO];}else{return [0,_QJ,_QL[1]];}},_QP=function(_QQ){return new F(function(){return _Qo(_6T,function(_QR){var _QS=E(_QR),_QT=_QS[2],_QU=E(_QS[1]),_QV=B(_LE(_QQ,_QT));if(!_QV[0]){var _QW=E(new T(function(){return B(_Ng(_Nl,_QQ));})),_QX=[0,E(_QU),_QQ];return new F(function(){return _QG(new T(function(){return [0,E([0,_QU[1]+1|0]),E(B(_N6(_QQ,_QX,_QT))),E(_QS[3])];}),_QX);});}else{return new F(function(){return _QG(_QS,_QV[1]);});}});});},_QY=function(_QZ,_R0){var _R1=function(_R2,_R3){var _R4=E(_R2),_R5=_R4[2],_R6=E(_R4[1]),_R7=function(_R8,_R9){var _Ra=E(_R8),_Rb=_Ra[3],_Rc=B(_Qu(_R3,_R9,_xf,_Rb));if(!_Rc[0]){var _Rd=E(_R3),_Re=E(_R9),_Rf=E(_xf),_Rg=[0,E(_Rd),E(_Re),E(_Rf)];return [0,new T(function(){return [0,E(_Ra[1]),E(_Ra[2]),E(B(_P5([0,_Rd,_Re,_Rf],_Rg,_Rb)))];}),_Rg];}else{return [0,_Ra,_Rc[1]];}},_Rh=B(_LE(_R0,_R5));if(!_Rh[0]){var _Ri=B(_Ng(_Nl,_R0)),_Rj=[0,E(_R6),_R0];return new F(function(){return _R7(new T(function(){return [0,E([0,_R6[1]+1|0]),E(B(_N6(_R0,_Rj,_R5))),E(_R4[3])];}),_Rj);});}else{return new F(function(){return _R7(_R4,_Rh[1]);});}};return new F(function(){return _Qo(_6T,function(_Rk){var _Rl=E(_Rk),_Rm=_Rl[2],_Rn=E(_Rl[1]),_Ro=B(_LE(_QZ,_Rm));if(!_Ro[0]){var _Rp=E(new T(function(){return B(_Ng(_Nl,_QZ));})),_Rq=[0,E(_Rn),_QZ];return new F(function(){return _R1(new T(function(){return [0,E([0,_Rn[1]+1|0]),E(B(_N6(_QZ,_Rq,_Rm))),E(_Rl[3])];}),_Rq);});}else{return new F(function(){return _R1(_Rl,_Ro[1]);});}});});},_Rr=function(_Rs){if(!B(_kO(_7I,_7d,_Rs))){return new F(function(){return _QP(_Rs);});}else{var _Rt=B(_ws(_LD,_Rs)),_Ru=E(_Rt[2]);if(!_Ru[0]){return E(_LA);}else{var _Rv=E(_Rt[1]);return _Rv[0]==0?B(_QP(_Rs)):B(_QY(_Ru[2],_Rv));}}},_Rw=function(_Rx){switch(E(E(_Rx)[1])){case 9:return true;case 10:return true;case 32:return true;default:return false;}},_Ry=function(_Rz,_RA,_RB,_RC,_RD){var _RE=E(_Rz),_RF=E(_RE[2]);return new F(function(){return _lM(_1r,_tX,_Rw,_RE[1],_RF[1],_RF[2],_RF[3],_RE[3],_RA,_RD);});},_RG=function(_ug,_uh,_ui,_uj,_tY){return new F(function(){return _4g(_Ry,_Ap,_ug,_uh,_ui,_uj,_tY);});},_RH=new T(function(){return B(unCStr("white space"));}),_RI=[1,_RH,_h],_RJ=function(_RK,_RL,_RM,_RN,_RO){return new F(function(){return _5L(_RG,_RI,_RK,_RL,_RM,_RN,_RO);});},_RP=function(_ug,_uh,_ui,_uj,_tY){return new F(function(){return _RJ(_ug,_uh,_ui,_uj,_tY);});},_RQ=function(_RR){var _RS=new T(function(){return B(_zq(_Bo,_RR));});return function(_RT,_RU,_RV,_RW,_RX){return new F(function(){return _5L(function(_RY,_RZ,_S0,_S1,_S2){var _S3=function(_S4,_S5){return new F(function(){return _7g(_RP,_S4,_RZ,_S0,function(_S6,_S7,_S8){return new F(function(){return A(_RZ,[_S6,_S7,new T(function(){return B(_4a(_S5,_S8));})]);});});});},_S9=function(_Sa,_Sb,_Sc){return new F(function(){return _S3(_Sb,_Sc);});};return new F(function(){return _7g(_RP,_RY,function(_Sd,_Se,_Sf){return new F(function(){return A(_RS,[_Se,_S9,_S2,function(_Sg,_Sh,_Si){return new F(function(){return _S3(_Sh,new T(function(){return B(_4a(_Sf,_Si));}));});},function(_Sj){return new F(function(){return A(_S2,[new T(function(){return B(_4a(_Sf,_Sj));})]);});}]);});},_S2,function(_Sk,_Sl,_Sm){return new F(function(){return A(_RS,[_Sl,_S9,_S2,function(_Sn,_So,_Sp){return new F(function(){return _7g(_RP,_So,_RZ,_S0,function(_Sq,_Sr,_Ss){return new F(function(){return A(_S1,[_Sq,_Sr,new T(function(){return B(_4a(new T(function(){var _St=E(_Sm),_Su=E(_Sp),_Sv=B(_3P(_St[1],_St[2],_Su[1],_Su[2]));return [0,E(_Sv[1]),_Sv[2]];}),_Ss));})]);});});});},function(_Sw){return new F(function(){return A(_S2,[new T(function(){return B(_4a(_Sm,_Sw));})]);});}]);});});});},[1,[1,_RR,_h],_h],_RT,_RU,_RV,_RW,_RX);});};},_Sx=[0,39],_Sy=new T(function(){return B(_zq(_Bo,_Sx));}),_Sz=function(_SA,_SB,_SC,_SD,_SE){return new F(function(){return A(_Sy,[_SA,function(_SF,_SG,_SH){return new F(function(){return A(_SB,[_G,_SG,new T(function(){var _SI=E(_SH),_SJ=B(_3P(_SI[1],_SI[2],E(_SG)[2],_h));return [0,E(_SJ[1]),_SJ[2]];})]);});},_SC,function(_SK,_SL,_SM){return new F(function(){return A(_SD,[_G,_SL,new T(function(){var _SN=E(_SM),_SO=B(_3P(_SN[1],_SN[2],E(_SL)[2],_h));return [0,E(_SO[1]),_SO[2]];})]);});},_SE]);});},_SP=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _Sz(_B3,_B4,_B5,_B6,_zb);});},_SQ=function(_SR,_SS,_ST,_SU,_SV,_SW,_SX,_SY){var _SZ=function(_T0,_T1,_T2){var _T3=new T(function(){var _T4=E(E(_T0)[1]);return _T4[0]==9?E(_T4[1]):E(_vE);}),_T5=function(_T6){return !B(_kO(_xw,_T3,B(_Lw(_Lq,_T6))))?function(_T7,_T8,_T9,_Ta,_Tb){return new F(function(){return A(_Ta,[[1,_T0,_T6],_T7,new T(function(){return [0,E(E(_T7)[2]),_h];})]);});}:function(_Tc,_Td,_Te,_Tf,_Tg){return new F(function(){return A(_Tg,[new T(function(){return [0,E(E(_Tc)[2]),[1,new T(function(){return [1,E(E(new T(function(){return B(unAppCStr("attribute name ",new T(function(){return B(_0([1,_39,new T(function(){var _Th=E(_T3),_Ti=E(_Th[1])[2],_Tj=E(_Th[2]);if(E(_Tj[1])[1]!=E(E(_xf)[1])[1]){var _Tk=B(_3b(B(_0(_Tj[2],[1,_7d,_Ti])),_Lt));}else{var _Tk=B(_3b(_Ti,_Lt));}var _Tl=_Tk,_Tm=_Tl,_Tn=_Tm,_To=_Tn,_Tp=_To,_Tq=_Tp;return _Tq;})],_Lu));})));})))];}),_h]];})]);});};},_Tr=function(_Ts,_Tt,_Tu){return new F(function(){return A(_T5,[_Ts,_Tt,_SW,_SX,function(_Tv,_Tw,_Tx){return new F(function(){return A(_SW,[_Tv,_Tw,new T(function(){var _Ty=E(_T2),_Tz=E(_Tu),_TA=E(_Tx),_TB=B(_3P(_Tz[1],_Tz[2],_TA[1],_TA[2])),_TC=B(_3P(_Ty[1],_Ty[2],_TB[1],_TB[2]));return [0,E(_TC[1]),_TC[2]];})]);});},function(_TD){return new F(function(){return A(_SX,[new T(function(){var _TE=E(_T2),_TF=E(_Tu),_TG=E(_TD),_TH=B(_3P(_TF[1],_TF[2],_TG[1],_TG[2])),_TI=B(_3P(_TE[1],_TE[2],_TH[1],_TH[2]));return [0,E(_TI[1]),_TI[2]];})]);});}]);});};return new F(function(){return _TJ(_T1,function(_TK,_TL,_TM){return new F(function(){return A(_T5,[_TK,_TL,_SW,_SX,function(_TN,_TO,_TP){return new F(function(){return A(_SW,[_TN,_TO,new T(function(){return B(_4a(_TM,_TP));})]);});},function(_TQ){return new F(function(){return A(_SX,[new T(function(){return B(_4a(_TM,_TQ));})]);});}]);});},_SX,_Tr,function(_TR){return new F(function(){return _Tr(_h,_T1,new T(function(){var _TS=E(_TR),_TT=B(_3P(_TS[1],_TS[2],E(_T1)[2],_h));return [0,E(_TT[1]),_TT[2]];}));});});});};return new F(function(){return _uk(_SR,_SS,_ST,_SU,_SV,function(_TU,_TV,_TW){var _TX=function(_TY){return function(_TZ,_U0,_U1,_U2,_U3){return new F(function(){return A(_U2,[new T(function(){var _U4=[0,[9,new T(function(){return B(_Rr(_TU));})],_TY],_U5=B(_6v(_6O,_U4));return E(_U4);}),_TZ,new T(function(){return [0,E(E(_TZ)[2]),_h];})]);});};},_U6=function(_U7,_U8,_U9,_Ua,_Ub){var _Uc=function(_Ud,_Ue,_Uf){var _Ug=new T(function(){var _Uh=E(_Uf),_Ui=E(_Uh[2]);if(!_Ui[0]){var _Uj=E(_Uh);}else{var _Uk=B(_5q(_Uh[1],_Ui,_yw)),_Uj=[0,E(_Uk[1]),_Uk[2]];}var _Ul=_Uj;return _Ul;});return new F(function(){return A(_TX,[_Ud,_Ue,_U8,_U9,function(_Um,_Un,_Uo){return new F(function(){return A(_Ua,[_Um,_Un,new T(function(){return B(_4a(_Ug,_Uo));})]);});},function(_Up){return new F(function(){return A(_Ub,[new T(function(){return B(_4a(_Ug,_Up));})]);});}]);});},_Uq=function(_Ur,_Us,_Ut){return new F(function(){return A(_TX,[_Ur,_Us,_U8,_U9,function(_Uu,_Uv,_Uw){return new F(function(){return A(_U8,[_Uu,_Uv,new T(function(){return B(_4a(_Ut,_Uw));})]);});},function(_Ux){return new F(function(){return A(_U9,[new T(function(){return B(_4a(_Ut,_Ux));})]);});}]);});};return new F(function(){return _xx(_Lp,_Lp,_L4,_U7,_Uq,_U9,_Uc,function(_Uy){return new F(function(){return _xx(_SP,_SP,_L6,_U7,_Uq,_U9,function(_Uz,_UA,_UB){return new F(function(){return _Uc(_Uz,_UA,new T(function(){var _UC=E(_Uy),_UD=E(_UB),_UE=B(_3P(_UC[1],_UC[2],_UD[1],_UD[2]));return [0,E(_UE[1]),_UE[2]];}));});},function(_UF){return new F(function(){return A(_Ub,[new T(function(){var _UG=E(_Uy),_UH=E(_UF),_UI=B(_3P(_UG[1],_UG[2],_UH[1],_UH[2])),_UJ=B(_5q(_UI[1],_UI[2],_yw));return [0,E(_UJ[1]),_UJ[2]];})]);});});});});});};return new F(function(){return A(_RQ,[_Lv,_TV,function(_UK,_UL,_UM){return new F(function(){return _U6(_UL,_SZ,_SX,function(_UN,_UO,_UP){return new F(function(){return _SZ(_UN,_UO,new T(function(){return B(_4a(_UM,_UP));}));});},function(_UQ){return new F(function(){return A(_SX,[new T(function(){return B(_4a(_UM,_UQ));})]);});});});},_SX,function(_UR,_US,_UT){return new F(function(){return _U6(_US,_SZ,_SX,function(_UU,_UV,_UW){return new F(function(){return _SZ(_UU,_UV,new T(function(){var _UX=E(_TW),_UY=E(_UT),_UZ=E(_UW),_V0=B(_3P(_UY[1],_UY[2],_UZ[1],_UZ[2])),_V1=B(_3P(_UX[1],_UX[2],_V0[1],_V0[2]));return [0,E(_V1[1]),_V1[2]];}));});},function(_V2){return new F(function(){return A(_SX,[new T(function(){var _V3=E(_TW),_V4=E(_UT),_V5=E(_V2),_V6=B(_3P(_V4[1],_V4[2],_V5[1],_V5[2])),_V7=B(_3P(_V3[1],_V3[2],_V6[1],_V6[2]));return [0,E(_V7[1]),_V7[2]];})]);});});});},function(_V8){return new F(function(){return A(_SX,[new T(function(){return B(_4a(_TW,_V8));})]);});}]);});},_SX,function(_V9){return new F(function(){return A(_SY,[new T(function(){var _Va=E(_V9),_Vb=B(_5q(_Va[1],_Va[2],_Bq));return [0,E(_Vb[1]),_Vb[2]];})]);});});});},_TJ=function(_Vc,_Vd,_Ve,_Vf,_Vg){return new F(function(){return _v8(_RP,_Vc,function(_Vh,_Vi,_Vj){var _Vk=E(_Vi),_Vl=E(_Vk[2]);return new F(function(){return _SQ(_Vk[1],_Vl[1],_Vl[2],_Vl[3],_Vk[3],_Vd,_Ve,function(_Vm){return new F(function(){return A(_Vd,[_h,_Vk,new T(function(){var _Vn=E(_Vj),_Vo=E(_Vm),_Vp=B(_3P(_Vo[1],_Vo[2],_Vl,_h)),_Vq=B(_3P(_Vn[1],_Vn[2],_Vp[1],_Vp[2]));return [0,E(_Vq[1]),_Vq[2]];})]);});});});},_Ve,function(_Vr,_Vs,_Vt){var _Vu=E(_Vs),_Vv=E(_Vu[2]);return new F(function(){return _SQ(_Vu[1],_Vv[1],_Vv[2],_Vv[3],_Vu[3],_Vd,_Ve,function(_Vw){return new F(function(){return A(_Vf,[_h,_Vu,new T(function(){var _Vx=E(_Vt),_Vy=E(_Vw),_Vz=B(_3P(_Vy[1],_Vy[2],_Vv,_h)),_VA=B(_3P(_Vx[1],_Vx[2],_Vz[1],_Vz[2]));return [0,E(_VA[1]),_VA[2]];})]);});});});},_Vg);});},_VB=new T(function(){return B(_zH(_1r));}),_VC=function(_VD){return function(_VE,_VF,_VG,_VH,_VI){return new F(function(){return A(_VH,[new T(function(){var _VJ=[0,[0,_VD],_h],_VK=B(_6v(_6O,_VJ));return E(_VJ);}),_VE,new T(function(){return [0,E(E(_VE)[2]),_h];})]);});};},_VL=[1,_39,_h],_VM=new T(function(){return B(unCStr(" is undefined"));}),_VN=function(_VO){return new F(function(){return err(B(unAppCStr("allBut1 _ _ ",new T(function(){return B(_0([1,_39,new T(function(){return B(_3b(_VO,_VL));})],_VM));}))));});},_VP=new T(function(){return B(_VN(_h));}),_VQ=[1,_zc,_h],_VR=[1,_ze,_VQ],_VS=function(_VT,_VU,_VV){var _VW=E(_VV);if(!_VW[0]){return E(_VP);}else{var _VX=_VW[1],_VY=function(_VZ,_W0,_W1,_W2,_W3){return new F(function(){return (function(_W4,_W5,_W6,_W7){return new F(function(){return _Gz(_Bo,_VW[2],_W4,function(_W8,_W9,_Wa){return new F(function(){return A(_W5,[_VX,_W9,new T(function(){var _Wb=E(_Wa),_Wc=B(_3P(_Wb[1],_Wb[2],E(_W9)[2],_h));return [0,E(_Wc[1]),_Wc[2]];})]);});},_W7,function(_Wd,_We,_Wf){return new F(function(){return A(_W6,[_VX,_We,new T(function(){var _Wg=E(_Wf),_Wh=B(_3P(_Wg[1],_Wg[2],E(_We)[2],_h));return [0,E(_Wh[1]),_Wh[2]];})]);});},_W7);});})(_VZ,_W0,_W2,_W3);});};return new F(function(){return A(_VT,[function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(function(_Wi,_Wj,_Wk,_Wl,_Wm){var _Wn=E(_Wi),_Wo=E(_Wn[2]);return new F(function(){return _lM(_1r,_za,function(_Wp){var _Wq=E(_Wp),_Wr=_Wq[1],_Ws=new T(function(){if(!B(_kO(_7I,_Wq,_VR))){if(_Wr<57344){var _Wt=false;}else{if(_Wr>65533){if(_Wr<65536){var _Wu=false;}else{if(!B(A(_VU,[_Wq]))){var _Wv=false;}else{var _Wv=_Wr!=E(_VX)[1]?true:false;}var _Wu=_Wv;}var _Ww=_Wu;}else{if(!B(A(_VU,[_Wq]))){var _Wx=false;}else{var _Wx=_Wr!=E(_VX)[1]?true:false;}var _Ww=_Wx;}var _Wt=_Ww;}var _Wy=_Wt;}else{if(!B(A(_VU,[_Wq]))){var _Wz=false;}else{var _Wz=_Wr!=E(_VX)[1]?true:false;}var _Wy=_Wz;}return _Wy;});return _Wr<32?E(_Ws):_Wr>55295?E(_Ws):!B(A(_VU,[_Wq]))?false:_Wr!=E(_VX)[1]?true:false;},_Wn[1],_Wo[1],_Wo[2],_Wo[3],_Wn[3],_Wj,_Wm);});},function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_Ap,function(_WA,_WB,_WC,_WD,_WE){var _WF=function(_WG,_WH,_WI){return new F(function(){return (function(_WJ,_WK){return new F(function(){return A(_WB,[_VX,_WJ,new T(function(){var _WL=E(_WK),_WM=B(_3P(_WL[1],_WL[2],E(_WJ)[2],_h));return [0,E(_WM[1]),_WM[2]];})]);});})(_WH,_WI);});};return new F(function(){return A(new T(function(){return B(_zq(_Bo,_VX));}),[_WA,function(_WN,_WO,_WP){return new F(function(){return _4B(_3r,_VY,_WO,_WF,function(_WQ,_WR,_WS){return new F(function(){return A(_WB,[_VX,_WR,new T(function(){var _WT=E(_WP),_WU=E(_WS),_WV=B(_3P(_WT[1],_WT[2],_WU[1],_WU[2])),_WW=B(_3P(_WV[1],_WV[2],E(_WR)[2],_h));return [0,E(_WW[1]),_WW[2]];})]);});},function(_WX){return new F(function(){return A(_WE,[new T(function(){return B(_4a(_WP,_WX));})]);});});});},_WE,function(_WY,_WZ,_X0){return new F(function(){return _4B(_3r,_VY,_WZ,_WF,function(_X1,_X2,_X3){return new F(function(){return A(_WD,[_VX,_X2,new T(function(){var _X4=E(_X0),_X5=E(_X3),_X6=B(_3P(_X4[1],_X4[2],_X5[1],_X5[2])),_X7=B(_3P(_X6[1],_X6[2],E(_X2)[2],_h));return [0,E(_X7[1]),_X7[2]];})]);});},function(_X8){return new F(function(){return A(_WE,[new T(function(){return B(_4a(_X0,_X8));})]);});});});},_WE]);});},_B3,_B4,_B5,_B6,_zb);});},_B3,_B4,_B5,_B6,_zb);});}]);});}},_X9=new T(function(){return B(unCStr("]]>"));}),_Xa=new T(function(){return B(unCStr("<&"));}),_Xb=function(_Xc){return !B(_kO(_7I,_Xc,_Xa))?true:false;},_Xd=new T(function(){return B(_VS(_yy,_Xb,_X9));}),_Xe=function(_Xf,_Xg,_Xh,_Xi,_Xj){return new F(function(){return A(_Xd,[_Xf,function(_Xk,_Xl,_Xm){return new F(function(){return A(_VC,[_Xk,_Xl,_Xg,_Xh,function(_Xn,_Xo,_Xp){return new F(function(){return A(_Xg,[_Xn,_Xo,new T(function(){return B(_4a(_Xm,_Xp));})]);});},function(_Xq){return new F(function(){return A(_Xh,[new T(function(){return B(_4a(_Xm,_Xq));})]);});}]);});},_Xh,function(_Xr,_Xs,_Xt){return new F(function(){return A(_VC,[_Xr,_Xs,_Xg,_Xh,function(_Xu,_Xv,_Xw){return new F(function(){return A(_Xi,[_Xu,_Xv,new T(function(){return B(_4a(_Xt,_Xw));})]);});},function(_Xx){return new F(function(){return A(_Xj,[new T(function(){return B(_4a(_Xt,_Xx));})]);});}]);});},_Xj]);});},_Xy=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_Xe,_JP,_Xz,_XA,_XB,_XC,_XD);});},_XE=new T(function(){return B(unCStr("[CDATA["));}),_XF=function(_XG,_XH,_XI,_XJ,_XK){return new F(function(){return _Gz(_VB,_XE,_XG,function(_XL,_XM,_XN){return new F(function(){return A(_XH,[_G,_XM,new T(function(){var _XO=E(_XN),_XP=B(_3P(_XO[1],_XO[2],E(_XM)[2],_h));return [0,E(_XP[1]),_XP[2]];})]);});},_XI,function(_XQ,_XR,_XS){return new F(function(){return A(_XJ,[_G,_XR,new T(function(){var _XT=E(_XS),_XU=B(_3P(_XT[1],_XT[2],E(_XR)[2],_h));return [0,E(_XU[1]),_XU[2]];})]);});},_XK);});},_XV=new T(function(){return B(unCStr("--"));}),_XW=function(_XX,_XY,_XZ,_Y0,_Y1){return new F(function(){return _Gz(_VB,_XV,_XX,function(_Y2,_Y3,_Y4){return new F(function(){return A(_XY,[_G,_Y3,new T(function(){var _Y5=E(_Y4),_Y6=B(_3P(_Y5[1],_Y5[2],E(_Y3)[2],_h));return [0,E(_Y6[1]),_Y6[2]];})]);});},_XZ,function(_Y7,_Y8,_Y9){return new F(function(){return A(_Y0,[_G,_Y8,new T(function(){var _Ya=E(_Y9),_Yb=B(_3P(_Ya[1],_Ya[2],E(_Y8)[2],_h));return [0,E(_Yb[1]),_Yb[2]];})]);});},_Y1);});},_Yc=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _Gz(_VB,_X9,_Xz,_XA,_XB,_XC,_XD);});},_Yd=new T(function(){return B(unCStr("CDATA section"));}),_Ye=[1,_Yd,_h],_Yf=function(_Yg){return function(_Yh,_Yi,_Yj,_Yk,_Yl){return new F(function(){return A(_Yk,[new T(function(){var _Ym=[0,[5,_Yg],_h],_Yn=B(_6v(_6O,_Ym));return E(_Ym);}),_Yh,new T(function(){return [0,E(E(_Yh)[2]),_h];})]);});};},_Yo=function(_Yp){return true;},_Yq=new T(function(){return B(_VS(_lc,_Yo,_X9));}),_Yr=function(_Ys,_Yt,_Yu,_Yv,_Yw,_Yx){return new F(function(){return _5L(function(_Yy,_Yz,_YA,_YB,_YC){return new F(function(){return _xx(_Ys,_Yc,_Yq,_Yy,function(_YD,_YE,_YF){return new F(function(){return A(_Yf,[_YD,_YE,_Yz,_YA,function(_YG,_YH,_YI){return new F(function(){return A(_Yz,[_YG,_YH,new T(function(){return B(_4a(_YF,_YI));})]);});},function(_YJ){return new F(function(){return A(_YA,[new T(function(){return B(_4a(_YF,_YJ));})]);});}]);});},_YA,function(_YK,_YL,_YM){return new F(function(){return A(_Yf,[_YK,_YL,_Yz,_YA,function(_YN,_YO,_YP){return new F(function(){return A(_YB,[_YN,_YO,new T(function(){return B(_4a(_YM,_YP));})]);});},function(_YQ){return new F(function(){return A(_YC,[new T(function(){return B(_4a(_YM,_YQ));})]);});}]);});},_YC);});},_Ye,_Yt,_Yu,_Yv,_Yw,_Yx);});},_YR=new T(function(){return B(unCStr("-->"));}),_YS=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _Gz(_VB,_YR,_Xz,_XA,_XB,_XC,_XD);});},_YT=new T(function(){return B(unCStr("comment"));}),_YU=[1,_YT,_h],_YV=function(_YW){return function(_YX,_YY,_YZ,_Z0,_Z1){return new F(function(){return A(_Z0,[new T(function(){var _Z2=[0,[4,_YW],_h],_Z3=B(_6v(_6O,_Z2));return E(_Z2);}),_YX,new T(function(){return [0,E(E(_YX)[2]),_h];})]);});};},_Z4=new T(function(){return B(_VS(_lc,_Yo,_XV));}),_Z5=function(_Z6,_Z7,_Z8,_Z9,_Za,_Zb){return new F(function(){return _5L(function(_Zc,_Zd,_Ze,_Zf,_Zg){return new F(function(){return _xx(_Z6,_YS,_Z4,_Zc,function(_Zh,_Zi,_Zj){return new F(function(){return A(_YV,[_Zh,_Zi,_Zd,_Ze,function(_Zk,_Zl,_Zm){return new F(function(){return A(_Zd,[_Zk,_Zl,new T(function(){return B(_4a(_Zj,_Zm));})]);});},function(_Zn){return new F(function(){return A(_Ze,[new T(function(){return B(_4a(_Zj,_Zn));})]);});}]);});},_Ze,function(_Zo,_Zp,_Zq){return new F(function(){return A(_YV,[_Zo,_Zp,_Zd,_Ze,function(_Zr,_Zs,_Zt){return new F(function(){return A(_Zf,[_Zr,_Zs,new T(function(){return B(_4a(_Zq,_Zt));})]);});},function(_Zu){return new F(function(){return A(_Zg,[new T(function(){return B(_4a(_Zq,_Zu));})]);});}]);});},_Zg);});},_YU,_Z7,_Z8,_Z9,_Za,_Zb);});},_Zv=[0,33],_Zw=new T(function(){return B(_zq(_VB,_Zv));}),_Zx=function(_Zy,_Zz,_ZA,_ZB,_ZC){return new F(function(){return A(_Zw,[_Zy,function(_ZD,_ZE,_ZF){return new F(function(){return _Z5(_XW,_ZE,_Zz,_ZA,function(_ZG,_ZH,_ZI){return new F(function(){return A(_Zz,[_ZG,_ZH,new T(function(){return B(_4a(_ZF,_ZI));})]);});},function(_ZJ){return new F(function(){return _Yr(_XF,_ZE,_Zz,_ZA,function(_ZK,_ZL,_ZM){return new F(function(){return A(_Zz,[_ZK,_ZL,new T(function(){var _ZN=E(_ZF),_ZO=E(_ZJ),_ZP=E(_ZM),_ZQ=B(_3P(_ZO[1],_ZO[2],_ZP[1],_ZP[2])),_ZR=B(_3P(_ZN[1],_ZN[2],_ZQ[1],_ZQ[2]));return [0,E(_ZR[1]),_ZR[2]];})]);});},function(_ZS){return new F(function(){return A(_ZA,[new T(function(){var _ZT=E(_ZF),_ZU=E(_ZJ),_ZV=E(_ZS),_ZW=B(_3P(_ZU[1],_ZU[2],_ZV[1],_ZV[2])),_ZX=B(_3P(_ZT[1],_ZT[2],_ZW[1],_ZW[2]));return [0,E(_ZX[1]),_ZX[2]];})]);});});});});});},_ZA,function(_ZY,_ZZ,_100){return new F(function(){return _Z5(_XW,_ZZ,_Zz,_ZA,function(_101,_102,_103){return new F(function(){return A(_ZB,[_101,_102,new T(function(){return B(_4a(_100,_103));})]);});},function(_104){return new F(function(){return _Yr(_XF,_ZZ,_Zz,_ZA,function(_105,_106,_107){return new F(function(){return A(_ZB,[_105,_106,new T(function(){var _108=E(_100),_109=E(_104),_10a=E(_107),_10b=B(_3P(_109[1],_109[2],_10a[1],_10a[2])),_10c=B(_3P(_108[1],_108[2],_10b[1],_10b[2]));return [0,E(_10c[1]),_10c[2]];})]);});},function(_10d){return new F(function(){return A(_ZC,[new T(function(){var _10e=E(_100),_10f=E(_104),_10g=E(_10d),_10h=B(_3P(_10f[1],_10f[2],_10g[1],_10g[2])),_10i=B(_3P(_10e[1],_10e[2],_10h[1],_10h[2]));return [0,E(_10i[1]),_10i[2]];})]);});});});});});},_ZC]);});},_10j=[0,63],_10k=new T(function(){return B(_zq(_VB,_10j));}),_10l=function(_10m,_10n,_10o,_10p,_10q){return new F(function(){return A(_10k,[_10m,function(_10r,_10s,_10t){return new F(function(){return A(_10n,[_G,_10s,new T(function(){var _10u=E(_10t),_10v=B(_3P(_10u[1],_10u[2],E(_10s)[2],_h));return [0,E(_10v[1]),_10v[2]];})]);});},_10o,function(_10w,_10x,_10y){return new F(function(){return A(_10p,[_G,_10x,new T(function(){var _10z=E(_10y),_10A=B(_3P(_10z[1],_10z[2],E(_10x)[2],_h));return [0,E(_10A[1]),_10A[2]];})]);});},_10q]);});},_10B=[0,60],_10C=new T(function(){return B(unCStr("element"));}),_10D=[1,_10C,_h],_10E=[0,47],_10F=new T(function(){return B(_zq(_VB,_10E));}),_10G=new T(function(){return B(unCStr("value"));}),_10H=new T(function(){return B(_Rr(_10G));}),_10I=[9,_10H],_10J=function(_10K,_10L){while(1){var _10M=E(_10K);if(!_10M[0]){return E(_10L)[0]==0?true:false;}else{var _10N=E(_10L);if(!_10N[0]){return false;}else{if(E(_10M[1])[1]!=E(_10N[1])[1]){return false;}else{_10K=_10M[2];_10L=_10N[2];continue;}}}}},_10O=new T(function(){return B(unCStr("?>"));}),_10P=new T(function(){return B(_VS(_lc,_Yo,_10O));}),_10Q=function(_10R){var _10S=u_towlower(_10R),_10T=_10S;if(_10T>>>0>1114111){return new F(function(){return _DW(_10T);});}else{return [0,_10T];}},_10U=function(_10V){return new F(function(){return _10Q(E(_10V)[1]);});},_10W=function(_10X,_10Y,_10Z,_110,_111,_112,_113,_114){return new F(function(){return _uk(_10X,_10Y,_10Z,_110,_111,function(_115,_116,_117){if(!B(_10J(B(_Lw(_10U,_115)),_wR))){var _118=function(_119){return function(_11a,_11b,_11c,_11d,_11e){return new F(function(){return A(_11d,[new T(function(){var _11f=[0,[6,new T(function(){return B(_Rr(_115));}),[1,new T(function(){var _11g=[0,_10I,[1,new T(function(){var _11h=[0,[0,_119],_h],_11i=B(_6v(_6O,_11h));return E(_11h);}),_h]],_11j=B(_6v(_6O,_11g));return E(_11g);}),_h]],_h],_11k=B(_6v(_6O,_11f));return E(_11f);}),_11a,new T(function(){return [0,E(E(_11a)[2]),_h];})]);});};},_11l=new T(function(){var _11m=E(_117),_11n=B(_3P(_11m[1],_11m[2],E(_116)[2],_h));return [0,E(_11n[1]),_11n[2]];}),_11o=function(_11p,_11q,_11r){return new F(function(){return A(_118,[_11p,_11q,_112,_113,function(_11s,_11t,_11u){return new F(function(){return A(_112,[_11s,_11t,new T(function(){var _11v=E(_11l),_11w=E(_11r),_11x=E(_11u),_11y=B(_3P(_11w[1],_11w[2],_11x[1],_11x[2])),_11z=B(_3P(_11v[1],_11v[2],_11y[1],_11y[2]));return [0,E(_11z[1]),_11z[2]];})]);});},function(_11A){return new F(function(){return A(_113,[new T(function(){var _11B=E(_11l),_11C=E(_11r),_11D=E(_11A),_11E=B(_3P(_11C[1],_11C[2],_11D[1],_11D[2])),_11F=B(_3P(_11B[1],_11B[2],_11E[1],_11E[2]));return [0,E(_11F[1]),_11F[2]];})]);});}]);});},_11G=function(_11H,_11I,_11J){return new F(function(){return A(_118,[_11H,_11I,_112,_113,function(_11K,_11L,_11M){return new F(function(){return A(_112,[_11K,_11L,new T(function(){return B(_4a(_11J,_11M));})]);});},function(_11N){return new F(function(){return A(_113,[new T(function(){return B(_4a(_11J,_11N));})]);});}]);});};return new F(function(){return _yy(_RP,_116,function(_11O,_11P,_11Q){return new F(function(){return A(_10P,[_11P,_11G,_113,function(_11R,_11S,_11T){return new F(function(){return _11G(_11R,_11S,new T(function(){return B(_4a(_11Q,_11T));}));});},function(_11U){return new F(function(){return A(_113,[new T(function(){return B(_4a(_11Q,_11U));})]);});}]);});},_113,function(_11V,_11W,_11X){return new F(function(){return A(_10P,[_11W,_11G,_113,function(_11Y,_11Z,_120){return new F(function(){return _11o(_11Y,_11Z,new T(function(){var _121=E(_11X),_122=E(_120),_123=B(_3P(_121[1],_121[2],_122[1],_122[2]));return [0,E(_123[1]),_123[2]];}));});},function(_124){return new F(function(){return _11o(_h,_116,new T(function(){var _125=E(_11X),_126=E(_124),_127=B(_3P(_125[1],_125[2],_126[1],_126[2])),_128=B(_3P(_127[1],_127[2],E(_116)[2],_h));return [0,E(_128[1]),_128[2]];}));});}]);});},function(_129){return new F(function(){return _11o(_h,_116,new T(function(){var _12a=E(_129),_12b=B(_3P(_12a[1],_12a[2],E(_116)[2],_h));return [0,E(_12b[1]),_12b[2]];}));});});});}else{return new F(function(){return A(_113,[new T(function(){var _12c=E(_117),_12d=B(_3P(_12c[1],_12c[2],E(_116)[2],[1,new T(function(){return [1,E(E(_115))];}),_h]));return [0,E(_12d[1]),_12d[2]];})]);});}},_113,function(_12e){return new F(function(){return A(_114,[new T(function(){var _12f=E(_12e),_12g=B(_5q(_12f[1],_12f[2],_Bq));return [0,E(_12g[1]),_12g[2]];})]);});});});},_12h=function(_12i,_12j,_12k,_12l,_12m){var _12n=E(_12i),_12o=E(_12n[2]);return new F(function(){return _10W(_12n[1],_12o[1],_12o[2],_12o[3],_12n[3],_12j,_12k,_12m);});},_12p=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _Gz(_VB,_10O,_Xz,_XA,_XB,_XC,_XD);});},_12q=new T(function(){return B(unCStr("processing instruction"));}),_12r=[1,_12q,_h],_12s=function(_12t,_12u,_12v,_12w,_12x,_12y){return new F(function(){return _5L(function(_12z,_12A,_12B,_12C,_12D){return new F(function(){return _xx(_12t,_12p,_12h,_12z,_12A,_12B,_12C,_12D);});},_12r,_12u,_12v,_12w,_12x,_12y);});},_12E=function(_12F,_12G,_12H,_12I,_12J){var _12K=function(_12L,_12M){var _12N=E(_12L),_12O=E(_12N[2]),_12P=new T(function(){var _12Q=E(_12M),_12R=B(_3P(_12Q[1],_12Q[2],_12O,_h));return [0,E(_12R[1]),_12R[2]];});return new F(function(){return _12S(_12N[1],_12O[1],_12O[2],_12O[3],_12N[3],_12G,_12H,function(_12T){var _12U=new T(function(){var _12V=E(_12T),_12W=B(_5q(_12V[1],_12V[2],_10D));return [0,E(_12W[1]),_12W[2]];});return new F(function(){return _12s(_10l,_12N,_12G,_12H,function(_12X,_12Y,_12Z){return new F(function(){return A(_12G,[_12X,_12Y,new T(function(){var _130=E(_12P),_131=E(_12U),_132=E(_12Z),_133=B(_3P(_131[1],_131[2],_132[1],_132[2])),_134=B(_3P(_130[1],_130[2],_133[1],_133[2]));return [0,E(_134[1]),_134[2]];})]);});},function(_135){return new F(function(){return _Zx(_12N,_12G,_12H,function(_136,_137,_138){return new F(function(){return A(_12G,[_136,_137,new T(function(){var _139=E(_12P),_13a=E(_12U),_13b=E(_135),_13c=E(_138),_13d=B(_3P(_13b[1],_13b[2],_13c[1],_13c[2])),_13e=B(_3P(_13a[1],_13a[2],_13d[1],_13d[2])),_13f=B(_3P(_139[1],_139[2],_13e[1],_13e[2]));return [0,E(_13f[1]),_13f[2]];})]);});},function(_13g){return new F(function(){return A(_12H,[new T(function(){var _13h=E(_12P),_13i=E(_12U),_13j=E(_135),_13k=E(_13g),_13l=B(_3P(_13j[1],_13j[2],_13k[1],_13k[2])),_13m=B(_3P(_13i[1],_13i[2],_13l[1],_13l[2])),_13n=B(_3P(_13h[1],_13h[2],_13m[1],_13m[2]));return [0,E(_13n[1]),_13n[2]];})]);});});});});});});});},_13o=function(_XB,_XC,_XD){return new F(function(){return (function(_13p,_13q,_13r){return new F(function(){return _12K(_13q,_13r);});})(_XB,_XC,_XD);});};return new F(function(){return A(_zq,[_VB,_10B,_12F,function(_13s,_13t,_13u){var _13v=new T(function(){var _13w=E(_13u),_13x=B(_3P(_13w[1],_13w[2],E(_13t)[2],_h));return [0,E(_13x[1]),_13x[2]];});return new F(function(){return _4B(_3r,_10F,_13t,_13o,function(_13y,_13z,_13A){return new F(function(){return _12K(_13z,new T(function(){return B(_4a(_13v,_13A));}));});},function(_13B){return new F(function(){return A(_12J,[new T(function(){return B(_4a(_13v,_13B));})]);});});});},_12J,function(_13C,_13D,_13E){var _13F=new T(function(){var _13G=E(_13E),_13H=B(_3P(_13G[1],_13G[2],E(_13D)[2],_h));return [0,E(_13H[1]),_13H[2]];});return new F(function(){return _4B(_3r,_10F,_13D,_13o,function(_13I,_13J,_13K){var _13L=E(_13J),_13M=E(_13L[2]),_13N=new T(function(){var _13O=E(_13F),_13P=E(_13K),_13Q=B(_3P(_13O[1],_13O[2],_13P[1],_13P[2])),_13R=B(_3P(_13Q[1],_13Q[2],_13M,_h));return [0,E(_13R[1]),_13R[2]];});return new F(function(){return _12S(_13L[1],_13M[1],_13M[2],_13M[3],_13L[3],_12G,_12H,function(_13S){var _13T=new T(function(){var _13U=E(_13S),_13V=B(_5q(_13U[1],_13U[2],_10D));return [0,E(_13V[1]),_13V[2]];});return new F(function(){return _12s(_10l,_13L,_12G,_12H,function(_13W,_13X,_13Y){return new F(function(){return A(_12I,[_13W,_13X,new T(function(){var _13Z=E(_13N),_140=E(_13T),_141=E(_13Y),_142=B(_3P(_140[1],_140[2],_141[1],_141[2])),_143=B(_3P(_13Z[1],_13Z[2],_142[1],_142[2]));return [0,E(_143[1]),_143[2]];})]);});},function(_144){return new F(function(){return _Zx(_13L,_12G,_12H,function(_145,_146,_147){return new F(function(){return A(_12I,[_145,_146,new T(function(){var _148=E(_13N),_149=E(_13T),_14a=E(_144),_14b=E(_147),_14c=B(_3P(_14a[1],_14a[2],_14b[1],_14b[2])),_14d=B(_3P(_149[1],_149[2],_14c[1],_14c[2])),_14e=B(_3P(_148[1],_148[2],_14d[1],_14d[2]));return [0,E(_14e[1]),_14e[2]];})]);});},function(_14f){return new F(function(){return A(_12J,[new T(function(){var _14g=E(_13N),_14h=E(_13T),_14i=E(_144),_14j=E(_14f),_14k=B(_3P(_14i[1],_14i[2],_14j[1],_14j[2])),_14l=B(_3P(_14h[1],_14h[2],_14k[1],_14k[2])),_14m=B(_3P(_14g[1],_14g[2],_14l[1],_14l[2]));return [0,E(_14m[1]),_14m[2]];})]);});});});});});});});},function(_14n){return new F(function(){return A(_12J,[new T(function(){return B(_4a(_13F,_14n));})]);});});});},_12J]);});},_14o=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_12E,_Xy,_Xz,_XA,_XB,_XC,_XD);});},_14p=[0,62],_14q=new T(function(){return B(unCStr("</"));}),_14r=new T(function(){return B(unCStr("/>"));}),_14s=new T(function(){return B(unCStr("> expected"));}),_14t=function(_14u){var _14v=E(_14u);if(!_14v[0]){return [0];}else{var _14w=_14v[1],_14x=B(_14t(_14v[2]));if(!_14x[0]){return [1,_14w,_h];}else{return new F(function(){return _0(B(_KB(_14w,_14x[1])),_14x[2]);});}}},_14y=[1,_h,_h],_14z=new T(function(){return B(unCStr("proper attribute list followed by \"/>\" or \">\""));}),_14A=[1,_14z,_h],_12S=function(_14B,_14C,_14D,_14E,_14F,_14G,_14H,_14I){var _14J=function(_14K,_14L,_14M,_14N){var _14O=E(_14K),_14P=E(_14O[1])[2],_14Q=E(_14O[2]),_14R=_14Q[2],_14S=E(_14Q[1])[1],_14T=B(_6o(_7c,_14L)),_14U=function(_14V,_14W,_14X){return new F(function(){return A(_14G,[_14V,_14W,new T(function(){var _14Y=E(_14N),_14Z=_14Y[1],_150=_14Y[2],_151=E(_14X),_152=_151[1],_153=E(_151[2]);if(!_153[0]){var _154=B(_3P(_14Z,_150,_152,_h)),_155=[0,E(_154[1]),_154[2]];}else{var _156=B(_5q(_152,_153,_14A)),_157=B(_3P(_14Z,_150,_156[1],_156[2])),_155=[0,E(_157[1]),_157[2]];}var _158=_155,_159=_158;return _159;})]);});},_15a=[7,_14O,_14L],_15b=function(_15c,_15d,_15e,_15f,_15g,_15h,_15i,_15j){return new F(function(){return _uk(_15c,_15d,_15e,_15f,_15g,function(_15k,_15l,_15m){var _15n=new T(function(){if(!B(_10J(_15k,new T(function(){return _14S!=E(E(_xf)[1])[1]?B(_0(_14R,[1,_7d,_14P])):E(_14P);})))){var _15o=function(_15p,_15q,_15r,_15s,_15t){return new F(function(){return A(_15t,[new T(function(){return [0,E(E(_15p)[2]),[1,new T(function(){return [1,E(B(unAppCStr("illegal end tag </",new T(function(){return B(_0(_15k,new T(function(){return B(unAppCStr("> found, </",new T(function(){if(_14S!=E(E(_xf)[1])[1]){var _15u=B(_0(B(_0(_14R,[1,_7d,_14P])),_14s));}else{var _15u=B(_0(_14P,_14s));}var _15v=_15u,_15w=_15v;return _15w;})));})));}))))];}),_h]];})]);});};}else{var _15o=E(_v2);}return _15o;}),_15x=function(_15y,_15z,_15A,_15B,_15C){return new F(function(){return A(_zq,[_VB,_14p,_15y,function(_15D,_15E,_15F){var _15G=new T(function(){var _15H=E(_15F),_15I=B(_3P(_15H[1],_15H[2],E(_15E)[2],_h));return [0,E(_15I[1]),_15I[2]];});return new F(function(){return A(_15n,[_15E,_15z,_15A,function(_15J,_15K,_15L){return new F(function(){return A(_15z,[_15J,_15K,new T(function(){return B(_4a(_15G,_15L));})]);});},function(_15M){return new F(function(){return A(_15A,[new T(function(){return B(_4a(_15G,_15M));})]);});}]);});},_15A,function(_15N,_15O,_15P){var _15Q=new T(function(){var _15R=E(_15P),_15S=B(_3P(_15R[1],_15R[2],E(_15O)[2],_h));return [0,E(_15S[1]),_15S[2]];});return new F(function(){return A(_15n,[_15O,_15z,_15A,function(_15T,_15U,_15V){return new F(function(){return A(_15B,[_15T,_15U,new T(function(){return B(_4a(_15Q,_15V));})]);});},function(_15W){return new F(function(){return A(_15C,[new T(function(){return B(_4a(_15Q,_15W));})]);});}]);});},_15C]);});};return new F(function(){return _7g(_RP,_15l,function(_15X,_15Y,_15Z){return new F(function(){return _15x(_15Y,_15h,_15i,function(_160,_161,_162){return new F(function(){return A(_15h,[_160,_161,new T(function(){return B(_4a(_15Z,_162));})]);});},function(_163){return new F(function(){return A(_15i,[new T(function(){return B(_4a(_15Z,_163));})]);});});});},_15i,function(_164,_165,_166){return new F(function(){return _15x(_165,_15h,_15i,function(_167,_168,_169){return new F(function(){return A(_15h,[_167,_168,new T(function(){var _16a=E(_15m),_16b=E(_166),_16c=E(_169),_16d=B(_3P(_16b[1],_16b[2],_16c[1],_16c[2])),_16e=B(_3P(_16a[1],_16a[2],_16d[1],_16d[2]));return [0,E(_16e[1]),_16e[2]];})]);});},function(_16f){return new F(function(){return A(_15i,[new T(function(){var _16g=E(_15m),_16h=E(_166),_16i=E(_16f),_16j=B(_3P(_16h[1],_16h[2],_16i[1],_16i[2])),_16k=B(_3P(_16g[1],_16g[2],_16j[1],_16j[2]));return [0,E(_16k[1]),_16k[2]];})]);});});});});});},_15i,function(_16l){return new F(function(){return A(_15j,[new T(function(){var _16m=E(_16l),_16n=B(_5q(_16m[1],_16m[2],_Bq));return [0,E(_16n[1]),_16n[2]];})]);});});});},_16o=function(_16p){return function(_16q,_16r,_16s,_16t,_16u){var _16v=function(_16w){return new F(function(){return A(_16u,[new T(function(){var _16x=E(_16w),_16y=B(_5q(_16x[1],_16x[2],_14y));return [0,E(_16y[1]),_16y[2]];})]);});},_16z=function(_16A,_16B,_16C){return new F(function(){return (function(_16D,_16E){return new F(function(){return A(_16r,[new T(function(){var _16F=[0,_15a,_16p],_16G=B(_6v(_6O,_16F));return E(_16F);}),_16D,new T(function(){var _16H=E(_16E),_16I=B(_3P(_16H[1],_16H[2],E(_16D)[2],_h));return [0,E(_16I[1]),_16I[2]];})]);});})(_16B,_16C);});};return new F(function(){return _Gz(_VB,_14q,_16q,function(_16J,_16K,_16L){var _16M=E(_16K),_16N=E(_16M[2]);return new F(function(){return _15b(_16M[1],_16N[1],_16N[2],_16N[3],_16M[3],_16z,_16s,function(_16O){return new F(function(){return A(_16s,[new T(function(){return B(_4a(new T(function(){var _16P=E(_16L),_16Q=B(_3P(_16P[1],_16P[2],_16N,_h));return [0,E(_16Q[1]),_16Q[2]];}),_16O));})]);});});});},_16v,function(_16R,_16S,_16T){var _16U=E(_16S),_16V=E(_16U[2]);return new F(function(){return _15b(_16U[1],_16V[1],_16V[2],_16V[3],_16U[3],_16z,_16s,function(_16W){return new F(function(){return A(_16u,[new T(function(){return B(_4a(new T(function(){var _16X=E(_16T),_16Y=B(_3P(_16X[1],_16X[2],_16V,_h)),_16Z=_16Y[1],_170=E(_16Y[2]);if(!_170[0]){var _171=[0,E(_16Z),_h];}else{var _172=B(_5q(_16Z,_170,_14y)),_171=[0,E(_172[1]),_172[2]];}var _173=_171,_174=_173;return _174;}),_16W));})]);});});});},_16v);});};},_175=function(_176,_177,_178,_179,_17a){return new F(function(){return _lc(_14o,_176,function(_17b){return function(_17c,_17d){return new F(function(){return A(_16o,[new T(function(){return B(_14t(_17b));}),_17c,_177,_178,function(_17e,_17f,_17g){return new F(function(){return A(_177,[_17e,_17f,new T(function(){return B(_4a(_17d,_17g));})]);});},function(_17h){return new F(function(){return A(_178,[new T(function(){return B(_4a(_17d,_17h));})]);});}]);});};},_178,function(_17i){return function(_17j,_17k){return new F(function(){return A(_16o,[new T(function(){return B(_14t(_17i));}),_17j,_177,_178,function(_17l,_17m,_17n){return new F(function(){return A(_179,[_17l,_17m,new T(function(){return B(_4a(_17k,_17n));})]);});},function(_17o){return new F(function(){return A(_17a,[new T(function(){return B(_4a(_17k,_17o));})]);});}]);});};},_17a);});},_17p=function(_17q){var _17r=function(_17s){return new F(function(){return A(_14H,[new T(function(){var _17t=E(_14N),_17u=E(_17q),_17v=E(_17s),_17w=B(_3P(_17u[1],_17u[2],_17v[1],_17v[2])),_17x=B(_5q(_17w[1],_17w[2],_14A)),_17y=B(_3P(_17t[1],_17t[2],_17x[1],_17x[2]));return [0,E(_17y[1]),_17y[2]];})]);});};return new F(function(){return A(_zq,[_VB,_14p,_14M,function(_17z,_17A,_17B){var _17C=new T(function(){var _17D=E(_17B),_17E=B(_3P(_17D[1],_17D[2],E(_17A)[2],_h));return [0,E(_17E[1]),_17E[2]];});return new F(function(){return _175(_17A,_14G,_14H,function(_17F,_17G,_17H){return new F(function(){return A(_14G,[_17F,_17G,new T(function(){return B(_4a(_17C,_17H));})]);});},function(_17I){return new F(function(){return A(_14H,[new T(function(){return B(_4a(_17C,_17I));})]);});});});},_14H,function(_17J,_17K,_17L){var _17M=new T(function(){var _17N=E(_17L),_17O=B(_3P(_17N[1],_17N[2],E(_17K)[2],_h));return [0,E(_17O[1]),_17O[2]];});return new F(function(){return _175(_17K,_14G,_14H,function(_17P,_17Q,_17R){return new F(function(){return _14U(_17P,_17Q,new T(function(){var _17S=E(_17q),_17T=E(_17M),_17U=E(_17R),_17V=B(_3P(_17T[1],_17T[2],_17U[1],_17U[2])),_17W=B(_3P(_17S[1],_17S[2],_17V[1],_17V[2]));return [0,E(_17W[1]),_17W[2]];}));});},function(_17X){return new F(function(){return _17r(new T(function(){var _17Y=E(_17M),_17Z=E(_17X),_180=B(_3P(_17Y[1],_17Y[2],_17Z[1],_17Z[2]));return [0,E(_180[1]),_180[2]];}));});});});},_17r]);});},_181=new T(function(){var _182=[0,_15a,_h],_183=B(_6v(_6O,_182));return E(_182);});return new F(function(){return _Gz(_VB,_14r,_14M,function(_184,_185,_186){return new F(function(){return A(_14G,[_181,_185,new T(function(){var _187=E(_185)[2],_188=E(_186),_189=B(_3P(_188[1],_188[2],_187,_h)),_18a=B(_3P(_189[1],_189[2],_187,_h));return [0,E(_18a[1]),_18a[2]];})]);});},_17p,function(_18b,_18c,_18d){return new F(function(){return _14U(_181,_18c,new T(function(){var _18e=E(_18c)[2],_18f=E(_18d),_18g=B(_3P(_18f[1],_18f[2],_18e,_h)),_18h=B(_3P(_18g[1],_18g[2],_18e,_h));return [0,E(_18h[1]),_18h[2]];}));});},_17p);});},_18i=function(_18j,_18k,_18l){var _18m=E(_18j);return new F(function(){return _14J(_18m[1],_18m[2],_18k,_18l);});};return new F(function(){return _uk(_14B,_14C,_14D,_14E,_14F,function(_18n,_18o,_18p){var _18q=function(_18r,_18s,_18t,_18u,_18v){var _18w=[0,new T(function(){return B(_Rr(_18n));}),_18r];return new F(function(){return _7g(_RP,_18s,function(_18x,_18y,_18z){return new F(function(){return A(_18t,[_18w,_18y,new T(function(){var _18A=E(_18z),_18B=B(_3P(_18A[1],_18A[2],E(_18y)[2],_h));return [0,E(_18B[1]),_18B[2]];})]);});},_18u,function(_18C,_18D,_18E){return new F(function(){return A(_18v,[_18w,_18D,new T(function(){var _18F=E(_18E),_18G=B(_3P(_18F[1],_18F[2],E(_18D)[2],_h));return [0,E(_18G[1]),_18G[2]];})]);});});});},_18H=function(_18I,_18J,_18K){return new F(function(){return _18q(_18I,_18J,_18i,_14H,function(_18L,_18M,_18N){var _18O=E(_18L);return new F(function(){return _14J(_18O[1],_18O[2],_18M,new T(function(){var _18P=E(_18p),_18Q=E(_18K),_18R=E(_18N),_18S=B(_3P(_18Q[1],_18Q[2],_18R[1],_18R[2])),_18T=B(_3P(_18P[1],_18P[2],_18S[1],_18S[2]));return [0,E(_18T[1]),_18T[2]];}));});});});};return new F(function(){return _TJ(_18o,function(_18U,_18V,_18W){return new F(function(){return _18q(_18U,_18V,_18i,_14H,function(_18X,_18Y,_18Z){var _190=E(_18X);return new F(function(){return _14J(_190[1],_190[2],_18Y,new T(function(){return B(_4a(_18W,_18Z));}));});});});},_14H,_18H,function(_191){return new F(function(){return _18H(_h,_18o,new T(function(){var _192=E(_191),_193=B(_3P(_192[1],_192[2],E(_18o)[2],_h));return [0,E(_193[1]),_193[2]];}));});});});},_14H,function(_194){return new F(function(){return A(_14I,[new T(function(){var _195=E(_194),_196=B(_5q(_195[1],_195[2],_Bq));return [0,E(_196[1]),_196[2]];})]);});});});},_197=new T(function(){return B(_zq(_VB,_10B));}),_198=function(_199,_19a,_19b,_19c){return new F(function(){return A(_197,[_199,function(_19d,_19e,_19f){var _19g=E(_19e),_19h=E(_19g[2]);return new F(function(){return _12S(_19g[1],_19h[1],_19h[2],_19h[3],_19g[3],_19a,_19b,function(_19i){return new F(function(){return A(_19b,[new T(function(){var _19j=E(_19f),_19k=E(_19i),_19l=B(_5q(_19k[1],_19k[2],_10D)),_19m=B(_3P(_19j[1],_19j[2],_19l[1],_19l[2]));return [0,E(_19m[1]),_19m[2]];})]);});});});},_19b,function(_19n,_19o,_19p){var _19q=E(_19o),_19r=E(_19q[2]);return new F(function(){return _12S(_19q[1],_19r[1],_19r[2],_19r[3],_19q[3],_19a,_19b,function(_19s){return new F(function(){return A(_19c,[new T(function(){var _19t=E(_19p),_19u=E(_19s),_19v=B(_5q(_19u[1],_19u[2],_10D)),_19w=B(_3P(_19t[1],_19t[2],_19v[1],_19v[2]));return [0,E(_19w[1]),_19w[2]];})]);});});});},_19c]);});},_19x=function(_XD){return new F(function(){return _tT(_1r,_XD);});},_19y=new T(function(){return B(unCStr("<!--"));}),_19z=function(_19A,_19B,_19C,_19D){return new F(function(){return _Gz(_VB,_19y,_19A,function(_19E,_19F,_19G){return new F(function(){return A(_19B,[_G,_19F,new T(function(){var _19H=E(_19G),_19I=B(_3P(_19H[1],_19H[2],E(_19F)[2],_h));return [0,E(_19I[1]),_19I[2]];})]);});},_19D,function(_19J,_19K,_19L){return new F(function(){return A(_19C,[_G,_19K,new T(function(){var _19M=E(_19L),_19N=B(_3P(_19M[1],_19M[2],E(_19K)[2],_h));return [0,E(_19N[1]),_19N[2]];})]);});},_19D);});},_19O=function(_19P,_19Q,_19R,_19S,_19T){return new F(function(){return _19z(_19P,_19Q,_19S,_19T);});},_19U=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _Z5(_19O,_Xz,_XA,_XB,_XC,_XD);});},_19V=new T(function(){return B(unCStr("<?"));}),_19W=function(_19X,_19Y,_19Z,_1a0){return new F(function(){return _Gz(_VB,_19V,_19X,function(_1a1,_1a2,_1a3){return new F(function(){return A(_19Y,[_G,_1a2,new T(function(){var _1a4=E(_1a3),_1a5=B(_3P(_1a4[1],_1a4[2],E(_1a2)[2],_h));return [0,E(_1a5[1]),_1a5[2]];})]);});},_1a0,function(_1a6,_1a7,_1a8){return new F(function(){return A(_19Z,[_G,_1a7,new T(function(){var _1a9=E(_1a8),_1aa=B(_3P(_1a9[1],_1a9[2],E(_1a7)[2],_h));return [0,E(_1aa[1]),_1aa[2]];})]);});},_1a0);});},_1ab=function(_1ac,_1ad,_1ae,_1af,_1ag){return new F(function(){return _19W(_1ac,_1ad,_1af,_1ag);});},_1ah=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _12s(_1ab,_Xz,_XA,_XB,_XC,_XD);});},_1ai=function(_1aj){return function(_1ak,_1al,_1am,_1an,_1ao){return new F(function(){return A(_1an,[new T(function(){var _1ap=[0,[0,_1aj],_h],_1aq=B(_6v(_6O,_1ap));return E(_1ap);}),_1ak,new T(function(){return [0,E(E(_1ak)[2]),_h];})]);});};},_1ar=function(_1as,_1at,_1au,_1av,_1aw){return new F(function(){return _yy(_RP,_1as,function(_1ax,_1ay,_1az){return new F(function(){return A(_1ai,[_1ax,_1ay,_1at,_1au,function(_1aA,_1aB,_1aC){return new F(function(){return A(_1at,[_1aA,_1aB,new T(function(){return B(_4a(_1az,_1aC));})]);});},function(_1aD){return new F(function(){return A(_1au,[new T(function(){return B(_4a(_1az,_1aD));})]);});}]);});},_1au,function(_1aE,_1aF,_1aG){return new F(function(){return A(_1ai,[_1aE,_1aF,_1at,_1au,function(_1aH,_1aI,_1aJ){return new F(function(){return A(_1av,[_1aH,_1aI,new T(function(){return B(_4a(_1aG,_1aJ));})]);});},function(_1aK){return new F(function(){return A(_1aw,[new T(function(){return B(_4a(_1aG,_1aK));})]);});}]);});},_1aw);});},_1aL=function(_1aM,_1aN,_1aO,_1aP,_1aQ){return new F(function(){return _5L(_1ar,_14y,_1aM,_1aN,_1aO,_1aP,_1aQ);});},_1aR=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_1ah,_1aL,_Xz,_XA,_XB,_XC,_XD);});},_1aS=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_19U,_1aR,_Xz,_XA,_XB,_XC,_XD);});},_1aT=function(_1aU,_1aV,_1aW,_1aX,_1aY){return new F(function(){return _198(_1aV,function(_1aZ,_1b0,_1b1){var _1b2=function(_1b3){var _1b4=new T(function(){return B(_0(_1aU,[1,_1aZ,_1b3]));});return function(_1b5,_1b6,_1b7,_1b8,_1b9){return new F(function(){return _64(_1r,_19x,_3r,_1b5,function(_1ba,_1bb,_1bc){return new F(function(){return A(_1b6,[_1b4,_1bb,new T(function(){var _1bd=E(_1bc),_1be=B(_3P(_1bd[1],_1bd[2],E(_1bb)[2],_h));return [0,E(_1be[1]),_1be[2]];})]);});},_1b7,function(_1bf,_1bg,_1bh){return new F(function(){return A(_1b8,[_1b4,_1bg,new T(function(){var _1bi=E(_1bh),_1bj=B(_3P(_1bi[1],_1bi[2],E(_1bg)[2],_h));return [0,E(_1bj[1]),_1bj[2]];})]);});},_1b9);});};};return new F(function(){return _lc(_1aS,_1b0,function(_1bk,_1bl,_1bm){return new F(function(){return A(_1b2,[_1bk,_1bl,_1aW,_1aX,function(_1bn,_1bo,_1bp){return new F(function(){return A(_1aW,[_1bn,_1bo,new T(function(){return B(_4a(_1bm,_1bp));})]);});},function(_1bq){return new F(function(){return A(_1aX,[new T(function(){return B(_4a(_1bm,_1bq));})]);});}]);});},_1aX,function(_1br,_1bs,_1bt){return new F(function(){return A(_1b2,[_1br,_1bs,_1aW,_1aX,function(_1bu,_1bv,_1bw){return new F(function(){return A(_1aW,[_1bu,_1bv,new T(function(){var _1bx=E(_1b1),_1by=E(_1bt),_1bz=E(_1bw),_1bA=B(_3P(_1by[1],_1by[2],_1bz[1],_1bz[2])),_1bB=B(_3P(_1bx[1],_1bx[2],_1bA[1],_1bA[2]));return [0,E(_1bB[1]),_1bB[2]];})]);});},function(_1bC){return new F(function(){return A(_1aX,[new T(function(){var _1bD=E(_1b1),_1bE=E(_1bt),_1bF=E(_1bC),_1bG=B(_3P(_1bE[1],_1bE[2],_1bF[1],_1bF[2])),_1bH=B(_3P(_1bD[1],_1bD[2],_1bG[1],_1bG[2]));return [0,E(_1bH[1]),_1bH[2]];})]);});}]);});},function(_1bI){return new F(function(){return A(_1aX,[new T(function(){return B(_4a(_1b1,_1bI));})]);});});});},_1aX,_1aY);});},_1bJ=0,_1bK=new T(function(){return B(unCStr("SYSTEM"));}),_1bL=function(_1bM,_1bN,_1bO,_1bP,_1bQ,_1bR,_1bS,_1bT,_1bU,_1bV){return new F(function(){return _lM(_1bM,_1bN,function(_1bW){return !B(_kO(_7I,_1bW,_1bO))?true:false;},_1bP,_1bQ,_1bR,_1bS,_1bT,_1bU,_1bV);});},_1bX=[1,_L7,_h],_1bY=function(_1bZ,_1c0,_1c1,_1c2,_1c3){var _1c4=E(_1bZ),_1c5=E(_1c4[2]);return new F(function(){return _1bL(_1r,_za,_1bX,_1c4[1],_1c5[1],_1c5[2],_1c5[3],_1c4[3],_1c0,_1c3);});},_1c6=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _lc(_1bY,_B3,_B4,_B5,_B6,_zb);});},_1c7=new T(function(){return B(unCStr("system literal (in quotes)"));}),_1c8=[1,_1c7,_h],_1c9=[1,_Sx,_h],_1ca=function(_1cb,_1cc,_1cd,_1ce,_1cf){var _1cg=E(_1cb),_1ch=E(_1cg[2]);return new F(function(){return _1bL(_1r,_za,_1c9,_1cg[1],_1ch[1],_1ch[2],_1ch[3],_1cg[3],_1cc,_1cf);});},_1ci=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _lc(_1ca,_B3,_B4,_B5,_B6,_zb);});},_1cj=function(_1ck,_1cl,_1cm,_1cn,_1co){var _1cp=function(_1cq,_1cr,_1cs){return new F(function(){return A(_1cn,[[1,[0,_1bK,_1cq],_h],_1cr,new T(function(){var _1ct=E(E(_1cr)[2]),_1cu=E(_1ct[2])[1],_1cv=E(_1ct[3])[1],_1cw=E(_1cs),_1cx=E(_1cw[1]),_1cy=E(_1cx[2])[1],_1cz=E(_1cx[3])[1],_1cA=E(_1cw[2]);if(!_1cA[0]){switch(B(_3H(_1cx[1],_1ct[1]))){case 0:var _1cB=[0,E(_1ct),_h];break;case 1:var _1cB=_1cy>=_1cu?_1cy!=_1cu?[0,E(_1cx),_h]:_1cz>=_1cv?_1cz!=_1cv?[0,E(_1cx),_h]:[0,E(_1cx),_3O]:[0,E(_1ct),_h]:[0,E(_1ct),_h];break;default:var _1cB=[0,E(_1cx),_h];}var _1cC=_1cB;}else{var _1cD=B(_5q(_1cx,_1cA,_1c8)),_1cE=B(_3P(_1cD[1],_1cD[2],_1ct,_h)),_1cC=[0,E(_1cE[1]),_1cE[2]];}var _1cF=_1cC,_1cG=_1cF,_1cH=_1cG,_1cI=_1cH,_1cJ=_1cI,_1cK=_1cJ,_1cL=_1cK,_1cM=_1cL;return _1cM;})]);});},_1cN=function(_1cO,_1cP,_1cQ){return new F(function(){return A(_1cl,[[1,[0,_1bK,_1cO],_h],_1cP,new T(function(){var _1cR=E(_1cQ),_1cS=B(_3P(_1cR[1],_1cR[2],E(_1cP)[2],_h));return [0,E(_1cS[1]),_1cS[2]];})]);});};return new F(function(){return _xx(_Lp,_Lp,_1c6,_1ck,_1cN,_1cm,_1cp,function(_1cT){return new F(function(){return _xx(_SP,_SP,_1ci,_1ck,_1cN,_1cm,function(_1cU,_1cV,_1cW){return new F(function(){return _1cp(_1cU,_1cV,new T(function(){var _1cX=E(_1cT),_1cY=E(_1cW),_1cZ=B(_3P(_1cX[1],_1cX[2],_1cY[1],_1cY[2]));return [0,E(_1cZ[1]),_1cZ[2]];}));});},function(_1d0){return new F(function(){return A(_1co,[new T(function(){var _1d1=E(_1cT),_1d2=E(_1d0),_1d3=B(_3P(_1d1[1],_1d1[2],_1d2[1],_1d2[2])),_1d4=B(_5q(_1d3[1],_1d3[2],_1c8));return [0,E(_1d4[1]),_1d4[2]];})]);});});});});});},_1d5=function(_1d6,_1d7,_1d8,_1d9,_1da){return new F(function(){return _v8(_RP,_1d6,function(_1db,_1dc,_1dd){return new F(function(){return _1cj(_1dc,_1d7,_1d8,function(_1de,_1df,_1dg){return new F(function(){return A(_1d7,[_1de,_1df,new T(function(){return B(_4a(_1dd,_1dg));})]);});},function(_1dh){return new F(function(){return A(_1d8,[new T(function(){return B(_4a(_1dd,_1dh));})]);});});});},_1d8,function(_1di,_1dj,_1dk){return new F(function(){return _1cj(_1dj,_1d7,_1d8,function(_1dl,_1dm,_1dn){return new F(function(){return A(_1d9,[_1dl,_1dm,new T(function(){return B(_4a(_1dk,_1dn));})]);});},function(_1do){return new F(function(){return A(_1da,[new T(function(){return B(_4a(_1dk,_1do));})]);});});});},_1da);});},_1dp=function(_1dq,_1dr,_1ds,_1dt,_1du,_1dv){return new F(function(){return _5L(function(_1dw,_1dx,_1dy,_1dz,_1dA){var _1dB=E(_1dw),_1dC=E(_1dB[2]);return new F(function(){return _uk(_1dB[1],_1dC[1],_1dC[2],_1dC[3],_1dB[3],function(_1dD,_1dE,_1dF){return !B(_10J(_1dD,_1dq))?B(A(_1dA,[new T(function(){var _1dG=E(_1dF),_1dH=B(_3P(_1dG[1],_1dG[2],E(_1dE)[2],[1,new T(function(){return [1,E(E(_1dD))];}),_h]));return [0,E(_1dH[1]),_1dH[2]];})])):B(A(_1dx,[_1dD,_1dE,new T(function(){var _1dI=E(_1dF),_1dJ=B(_3P(_1dI[1],_1dI[2],E(_1dE)[2],_h));return [0,E(_1dJ[1]),_1dJ[2]];})]));},_1dA,function(_1dK){return new F(function(){return A(_1dA,[new T(function(){var _1dL=E(_1dK),_1dM=B(_5q(_1dL[1],_1dL[2],_Bq));return [0,E(_1dM[1]),_1dM[2]];})]);});});});},[1,_1dq,_h],_1dr,_1ds,_1dt,_1du,_1dv);});},_1dN=function(_1dO,_1dP,_1dQ,_1dR,_1dS){return new F(function(){return _1dp(_1bK,_1dO,function(_1dT,_1dU,_1dV){return new F(function(){return _1d5(_1dU,_1dP,_1dQ,function(_1dW,_1dX,_1dY){return new F(function(){return A(_1dP,[_1dW,_1dX,new T(function(){return B(_4a(_1dV,_1dY));})]);});},function(_1dZ){return new F(function(){return A(_1dQ,[new T(function(){return B(_4a(_1dV,_1dZ));})]);});});});},_1dQ,function(_1e0,_1e1,_1e2){return new F(function(){return _1d5(_1e1,_1dP,_1dQ,function(_1e3,_1e4,_1e5){return new F(function(){return A(_1dR,[_1e3,_1e4,new T(function(){return B(_4a(_1e2,_1e5));})]);});},function(_1e6){return new F(function(){return A(_1dS,[new T(function(){return B(_4a(_1e2,_1e6));})]);});});});},_1dS);});},_1e7=new T(function(){return B(unCStr("PUBLIC"));}),_1e8=function(_1e9,_1ea,_1eb,_1ec,_1ed,_1ee){var _1ef=[1,[0,_1e7,_1e9],_h],_1eg=function(_1eh,_1ei,_1ej,_1ek,_1el){var _1em=function(_1en,_1eo,_1ep){return new F(function(){return A(_1ek,[[1,[0,_1bK,_1en],_1ef],_1eo,new T(function(){var _1eq=E(E(_1eo)[2]),_1er=E(_1eq[2])[1],_1es=E(_1eq[3])[1],_1et=E(_1ep),_1eu=E(_1et[1]),_1ev=E(_1eu[2])[1],_1ew=E(_1eu[3])[1],_1ex=E(_1et[2]);if(!_1ex[0]){switch(B(_3H(_1eu[1],_1eq[1]))){case 0:var _1ey=[0,E(_1eq),_h];break;case 1:var _1ey=_1ev>=_1er?_1ev!=_1er?[0,E(_1eu),_h]:_1ew>=_1es?_1ew!=_1es?[0,E(_1eu),_h]:[0,E(_1eu),_3O]:[0,E(_1eq),_h]:[0,E(_1eq),_h];break;default:var _1ey=[0,E(_1eu),_h];}var _1ez=_1ey;}else{var _1eA=B(_5q(_1eu,_1ex,_1c8)),_1eB=B(_3P(_1eA[1],_1eA[2],_1eq,_h)),_1ez=[0,E(_1eB[1]),_1eB[2]];}var _1eC=_1ez,_1eD=_1eC,_1eE=_1eD,_1eF=_1eE,_1eG=_1eF,_1eH=_1eG,_1eI=_1eH,_1eJ=_1eI;return _1eJ;})]);});},_1eK=function(_1eL,_1eM,_1eN){return new F(function(){return A(_1ei,[[1,[0,_1bK,_1eL],_1ef],_1eM,new T(function(){var _1eO=E(_1eN),_1eP=B(_3P(_1eO[1],_1eO[2],E(_1eM)[2],_h));return [0,E(_1eP[1]),_1eP[2]];})]);});};return new F(function(){return _xx(_Lp,_Lp,_1c6,_1eh,_1eK,_1ej,_1em,function(_1eQ){return new F(function(){return _xx(_SP,_SP,_1ci,_1eh,_1eK,_1ej,function(_1eR,_1eS,_1eT){return new F(function(){return _1em(_1eR,_1eS,new T(function(){var _1eU=E(_1eQ),_1eV=E(_1eT),_1eW=B(_3P(_1eU[1],_1eU[2],_1eV[1],_1eV[2]));return [0,E(_1eW[1]),_1eW[2]];}));});},function(_1eX){return new F(function(){return A(_1el,[new T(function(){var _1eY=E(_1eQ),_1eZ=E(_1eX),_1f0=B(_3P(_1eY[1],_1eY[2],_1eZ[1],_1eZ[2])),_1f1=B(_5q(_1f0[1],_1f0[2],_1c8));return [0,E(_1f1[1]),_1f1[2]];})]);});});});});});};return new F(function(){return _v8(_RP,_1ea,function(_1f2,_1f3,_1f4){return new F(function(){return _1eg(_1f3,_1eb,_1ec,function(_1f5,_1f6,_1f7){return new F(function(){return A(_1eb,[_1f5,_1f6,new T(function(){return B(_4a(_1f4,_1f7));})]);});},function(_1f8){return new F(function(){return A(_1ec,[new T(function(){return B(_4a(_1f4,_1f8));})]);});});});},_1ec,function(_1f9,_1fa,_1fb){return new F(function(){return _1eg(_1fa,_1eb,_1ec,function(_1fc,_1fd,_1fe){return new F(function(){return A(_1ed,[_1fc,_1fd,new T(function(){return B(_4a(_1fb,_1fe));})]);});},function(_1ff){return new F(function(){return A(_1ee,[new T(function(){return B(_4a(_1fb,_1ff));})]);});});});},_1ee);});},_1fg=[1,_Sx,_h],_1fh=new T(function(){return B(unCStr("ASCII letter"));}),_1fi=[1,_1fh,_h],_1fj=function(_1fk){return _1fk<65?_1fk<97?false:_1fk<=122:_1fk>90?_1fk<97?false:_1fk<=122:true;},_1fl=function(_1fm){return new F(function(){return _1fj(E(_1fm)[1]);});},_1fn=function(_1fo,_1fp,_1fq,_1fr,_1fs){var _1ft=E(_1fo),_1fu=E(_1ft[2]);return new F(function(){return _lM(_1r,_za,_1fl,_1ft[1],_1fu[1],_1fu[2],_1fu[3],_1ft[3],_1fp,_1fs);});},_1fv=function(_1fw,_1fx,_1fy,_1fz,_1fA){return new F(function(){return _5L(_1fn,_1fi,_1fw,_1fx,_1fy,_1fz,_1fA);});},_1fB=new T(function(){return B(unCStr(" \r\n"));}),_1fC=function(_1fD){return new F(function(){return _kO(_7I,_1fD,_1fB);});},_1fE=function(_1fF,_1fG,_1fH,_1fI,_1fJ){var _1fK=E(_1fF),_1fL=E(_1fK[2]);return new F(function(){return _lM(_1r,_za,_1fC,_1fK[1],_1fL[1],_1fL[2],_1fL[3],_1fK[3],_1fG,_1fJ);});},_1fM=function(_1fN,_1fO,_1fP,_1fQ,_1fR){var _1fS=E(_1fN),_1fT=E(_1fS[2]);return new F(function(){return _lM(_1r,_za,_DD,_1fS[1],_1fT[1],_1fT[2],_1fT[3],_1fS[3],_1fO,_1fR);});},_1fU=function(_1fV,_1fW,_1fX,_1fY,_1fZ){return new F(function(){return _5L(_1fM,_DP,_1fV,_1fW,_1fX,_1fY,_1fZ);});},_1g0=new T(function(){return B(unCStr("-()+,./:=?;!*#@$_%"));}),_1g1=function(_1g2){return new F(function(){return _kO(_7I,_1g2,_1g0);});},_1g3=function(_1g4,_1g5,_1g6,_1g7,_1g8){var _1g9=E(_1g4),_1ga=E(_1g9[2]);return new F(function(){return _lM(_1r,_za,_1g1,_1g9[1],_1ga[1],_1ga[2],_1ga[3],_1g9[3],_1g5,_1g8);});},_1gb=function(_1gc,_1gd,_1ge,_1gf,_1gg,_1gh){return new F(function(){return _4g(_1fv,function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_1fU,function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_1fE,function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_1g3,function(_1gi,_1gj,_1gk,_1gl,_1gm){var _1gn=E(_1gi),_1go=E(_1gn[2]);return new F(function(){return _lM(_1r,_za,function(_1gp){return new F(function(){return _kO(_7I,_1gp,_1gc);});},_1gn[1],_1go[1],_1go[2],_1go[3],_1gn[3],_1gj,_1gm);});},_B3,_B4,_B5,_B6,_zb);});},_B3,_B4,_B5,_B6,_zb);});},_B3,_B4,_B5,_B6,_zb);});},_1gd,_1ge,_1gf,_1gg,_1gh);});},_1gq=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _1gb(_1fg,_B3,_B4,_B5,_B6,_zb);});},_1gr=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _lc(_1gq,_B3,_B4,_B5,_B6,_zb);});},_1gs=new T(function(){return B(unCStr("pubid literal (in quotes)"));}),_1gt=[1,_1gs,_h],_1gu=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _1gb(_h,_B3,_B4,_B5,_B6,_zb);});},_1gv=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _lc(_1gu,_B3,_B4,_B5,_B6,_zb);});},_1gw=function(_1gx,_1gy,_1gz,_1gA,_1gB){var _1gC=function(_1gD,_1gE,_1gF){var _1gG=new T(function(){var _1gH=E(_1gF),_1gI=E(_1gH[2]);if(!_1gI[0]){var _1gJ=E(_1gH);}else{var _1gK=B(_5q(_1gH[1],_1gI,_1gt)),_1gJ=[0,E(_1gK[1]),_1gK[2]];}var _1gL=_1gJ;return _1gL;});return new F(function(){return _1e8(_1gD,_1gE,_1gy,_1gz,function(_1gM,_1gN,_1gO){return new F(function(){return A(_1gA,[_1gM,_1gN,new T(function(){return B(_4a(_1gG,_1gO));})]);});},function(_1gP){return new F(function(){return A(_1gB,[new T(function(){return B(_4a(_1gG,_1gP));})]);});});});},_1gQ=function(_1gR,_1gS,_1gT){return new F(function(){return _1e8(_1gR,_1gS,_1gy,_1gz,function(_1gU,_1gV,_1gW){return new F(function(){return A(_1gy,[_1gU,_1gV,new T(function(){return B(_4a(_1gT,_1gW));})]);});},function(_1gX){return new F(function(){return A(_1gz,[new T(function(){return B(_4a(_1gT,_1gX));})]);});});});};return new F(function(){return _xx(_Lp,_Lp,_1gr,_1gx,_1gQ,_1gz,_1gC,function(_1gY){return new F(function(){return _xx(_SP,_SP,_1gv,_1gx,_1gQ,_1gz,function(_1gZ,_1h0,_1h1){return new F(function(){return _1gC(_1gZ,_1h0,new T(function(){var _1h2=E(_1gY),_1h3=E(_1h1),_1h4=B(_3P(_1h2[1],_1h2[2],_1h3[1],_1h3[2]));return [0,E(_1h4[1]),_1h4[2]];}));});},function(_1h5){return new F(function(){return A(_1gB,[new T(function(){var _1h6=E(_1gY),_1h7=E(_1h5),_1h8=B(_3P(_1h6[1],_1h6[2],_1h7[1],_1h7[2])),_1h9=B(_5q(_1h8[1],_1h8[2],_1gt));return [0,E(_1h9[1]),_1h9[2]];})]);});});});});});},_1ha=function(_1hb,_1hc,_1hd,_1he,_1hf){return new F(function(){return _v8(_RP,_1hb,function(_1hg,_1hh,_1hi){return new F(function(){return _1gw(_1hh,_1hc,_1hd,function(_1hj,_1hk,_1hl){return new F(function(){return A(_1hc,[_1hj,_1hk,new T(function(){return B(_4a(_1hi,_1hl));})]);});},function(_1hm){return new F(function(){return A(_1hd,[new T(function(){return B(_4a(_1hi,_1hm));})]);});});});},_1hd,function(_1hn,_1ho,_1hp){return new F(function(){return _1gw(_1ho,_1hc,_1hd,function(_1hq,_1hr,_1hs){return new F(function(){return A(_1he,[_1hq,_1hr,new T(function(){return B(_4a(_1hp,_1hs));})]);});},function(_1ht){return new F(function(){return A(_1hf,[new T(function(){return B(_4a(_1hp,_1ht));})]);});});});},_1hf);});},_1hu=function(_1hv,_1hw,_1hx,_1hy,_1hz){return new F(function(){return _1dp(_1e7,_1hv,function(_1hA,_1hB,_1hC){return new F(function(){return _1ha(_1hB,_1hw,_1hx,function(_1hD,_1hE,_1hF){return new F(function(){return A(_1hw,[_1hD,_1hE,new T(function(){return B(_4a(_1hC,_1hF));})]);});},function(_1hG){return new F(function(){return A(_1hx,[new T(function(){return B(_4a(_1hC,_1hG));})]);});});});},_1hx,function(_1hH,_1hI,_1hJ){return new F(function(){return _1ha(_1hI,_1hw,_1hx,function(_1hK,_1hL,_1hM){return new F(function(){return A(_1hy,[_1hK,_1hL,new T(function(){return B(_4a(_1hJ,_1hM));})]);});},function(_1hN){return new F(function(){return A(_1hz,[new T(function(){return B(_4a(_1hJ,_1hN));})]);});});});},_1hz);});},_1hO=new T(function(){return B(unCStr("name"));}),_1hP=[0,91],_1hQ=new T(function(){return B(_zq(_VB,_1hP));}),_1hR=[0,93],_1hS=new T(function(){return B(_zq(_VB,_1hR));}),_1hT=new T(function(){return B(unCStr("SYSTEM or PUBLIC declaration"));}),_1hU=[1,_1hT,_h],_1hV=function(_1hW,_1hX,_1hY,_1hZ,_1i0,_1i1){return new F(function(){return A(_1hW,[_1hX,function(_1i2,_1i3,_1i4){return new F(function(){return A(_1hY,[[1,_1i2,_h],_1i3,new T(function(){var _1i5=E(_1i4),_1i6=B(_3P(_1i5[1],_1i5[2],E(_1i3)[2],_h));return [0,E(_1i6[1]),_1i6[2]];})]);});},_1hZ,function(_1i7,_1i8,_1i9){return new F(function(){return A(_1i0,[[1,_1i7,_h],_1i8,new T(function(){var _1ia=E(_1i9),_1ib=B(_3P(_1ia[1],_1ia[2],E(_1i8)[2],_h));return [0,E(_1ib[1]),_1ib[2]];})]);});},_1i1]);});},_1ic=[0,62],_1id=new T(function(){return B(_zH(_1r));}),_1ie=function(_1if,_1ig,_1ih,_1ii,_1ij){return new F(function(){return A(_zq,[_1id,_1ic,_1if,function(_1ik,_1il,_1im){return new F(function(){return A(_1ig,[_G,_1il,new T(function(){var _1in=E(_1il)[2],_1io=E(_1im),_1ip=B(_3P(_1io[1],_1io[2],_1in,_h)),_1iq=B(_3P(_1ip[1],_1ip[2],_1in,_h));return [0,E(_1iq[1]),_1iq[2]];})]);});},_1ih,function(_1ir,_1is,_1it){return new F(function(){return A(_1ii,[_G,_1is,new T(function(){var _1iu=E(_1is)[2],_1iv=E(_1it),_1iw=B(_3P(_1iv[1],_1iv[2],_1iu,_h)),_1ix=B(_3P(_1iw[1],_1iw[2],_1iu,_h));return [0,E(_1ix[1]),_1ix[2]];})]);});},_1ij]);});},_1iy=new T(function(){return B(unCStr("DTD token"));}),_1iz=[1,_1iy,_h],_1iA=function(_1iB){return function(_1iC,_1iD,_1iE,_1iF,_1iG){return new F(function(){return A(_1iF,[new T(function(){var _1iH=[0,[0,_1iB],_h],_1iI=B(_6v(_6O,_1iH));return E(_1iH);}),_1iC,new T(function(){return [0,E(E(_1iC)[2]),_h];})]);});};},_1iJ=new T(function(){return B(unCStr("%\"\'<>[]"));}),_1iK=function(_1iL,_1iM,_1iN,_1iO,_1iP){return new F(function(){return _Av(_1iJ,_1iL,_1iM,_1iN,_1iO,_1iP);});},_1iQ=function(_1iR,_1iS,_1iT,_1iU,_1iV){return new F(function(){return _yy(_1iK,_1iR,function(_1iW,_1iX,_1iY){return new F(function(){return A(_1iA,[_1iW,_1iX,_1iS,_1iT,function(_1iZ,_1j0,_1j1){return new F(function(){return A(_1iS,[_1iZ,_1j0,new T(function(){return B(_4a(_1iY,_1j1));})]);});},function(_1j2){return new F(function(){return A(_1iT,[new T(function(){return B(_4a(_1iY,_1j2));})]);});}]);});},_1iT,function(_1j3,_1j4,_1j5){return new F(function(){return A(_1iA,[_1j3,_1j4,_1iS,_1iT,function(_1j6,_1j7,_1j8){return new F(function(){return A(_1iU,[_1j6,_1j7,new T(function(){return B(_4a(_1j5,_1j8));})]);});},function(_1j9){return new F(function(){return A(_1iV,[new T(function(){return B(_4a(_1j5,_1j9));})]);});}]);});},_1iV);});},_1ja=function(_1jb){return function(_1jc,_1jd,_1je,_1jf,_1jg){return new F(function(){return A(_1jf,[new T(function(){var _1jh=[0,[0,_1jb],_h],_1ji=B(_6v(_6O,_1jh));return E(_1jh);}),_1jc,new T(function(){return [0,E(E(_1jc)[2]),_h];})]);});};},_1jj=[1,_L7,_h],_1jk=function(_1jl){return function(_1jm,_1jn,_1jo,_1jp,_1jq){return new F(function(){return A(_1jp,[[1,_L7,new T(function(){return B(_0(_1jl,_1jj));})],_1jm,new T(function(){return [0,E(E(_1jm)[2]),_h];})]);});};},_1jr=function(_1js){var _1jt=E(_1js);if(!_1jt[0]){return [0];}else{return new F(function(){return _0(_1jt[1],new T(function(){return B(_1jr(_1jt[2]));}));});}},_1ju=function(_1jv,_1jw,_1jx,_1jy,_1jz,_1jA){return new F(function(){return A(_1jv,[_1jw,function(_1jB,_1jC,_1jD){return new F(function(){return A(_1jx,[new T(function(){return B(_1jr(_1jB));}),_1jC,new T(function(){var _1jE=E(_1jD),_1jF=B(_3P(_1jE[1],_1jE[2],E(_1jC)[2],_h));return [0,E(_1jF[1]),_1jF[2]];})]);});},_1jy,function(_1jG,_1jH,_1jI){return new F(function(){return A(_1jz,[new T(function(){return B(_1jr(_1jG));}),_1jH,new T(function(){var _1jJ=E(_1jI),_1jK=B(_3P(_1jJ[1],_1jJ[2],E(_1jH)[2],_h));return [0,E(_1jK[1]),_1jK[2]];})]);});},_1jA]);});},_1jL=[1,_Bn,_h],_1jM=function(_1jN){return function(_1jO,_1jP,_1jQ,_1jR,_1jS){return new F(function(){return A(_1jR,[new T(function(){return B(unAppCStr("&#",new T(function(){return B(_0(B(_b(0,E(_1jN)[1],_h)),_1jL));})));}),_1jO,new T(function(){return [0,E(E(_1jO)[2]),_h];})]);});};},_1jT=function(_1jU,_1jV,_1jW,_1jX,_1jY){var _1jZ=function(_1k0,_1k1,_1k2){var _1k3=new T(function(){var _1k4=E(_1k2),_1k5=E(_1k4[2]);if(!_1k5[0]){var _1k6=E(_1k4);}else{var _1k7=B(_5q(_1k4[1],_1k5,_DB)),_1k6=[0,E(_1k7[1]),_1k7[2]];}var _1k8=_1k6;return _1k8;});return new F(function(){return A(_1jM,[_1k0,_1k1,_1jV,_1jW,function(_1k9,_1ka,_1kb){return new F(function(){return A(_1jX,[_1k9,_1ka,new T(function(){return B(_4a(_1k3,_1kb));})]);});},function(_1kc){return new F(function(){return A(_1jY,[new T(function(){return B(_4a(_1k3,_1kc));})]);});}]);});},_1kd=function(_1ke,_1kf,_1kg){return new F(function(){return A(_1jM,[_1ke,_1kf,_1jV,_1jW,function(_1kh,_1ki,_1kj){return new F(function(){return A(_1jV,[_1kh,_1ki,new T(function(){return B(_4a(_1kg,_1kj));})]);});},function(_1kk){return new F(function(){return A(_1jW,[new T(function(){return B(_4a(_1kg,_1kk));})]);});}]);});};return new F(function(){return _IL(_1jU,_1kd,_1jW,_1jZ,function(_1kl){return new F(function(){return _Ha(_1jU,_1kd,_1jW,function(_1km,_1kn,_1ko){return new F(function(){return _1jZ(_1km,_1kn,new T(function(){var _1kp=E(_1kl),_1kq=E(_1ko),_1kr=B(_3P(_1kp[1],_1kp[2],_1kq[1],_1kq[2]));return [0,E(_1kr[1]),_1kr[2]];}));});},function(_1ks){return new F(function(){return A(_1jY,[new T(function(){var _1kt=E(_1kl),_1ku=E(_1ks),_1kv=B(_3P(_1kt[1],_1kt[2],_1ku[1],_1ku[2])),_1kw=B(_5q(_1kv[1],_1kv[2],_DB));return [0,E(_1kw[1]),_1kw[2]];})]);});});});});});},_1kx=[1,_Bn,_h],_1ky=function(_1kz,_1kA,_1kB,_1kC){return new F(function(){return _BY(_1kz,function(_1kD,_1kE,_1kF){return new F(function(){return A(_1kA,[[1,_BX,new T(function(){return B(_0(_1kD,_1kx));})],_1kE,new T(function(){var _1kG=E(_1kF),_1kH=B(_3P(_1kG[1],_1kG[2],E(_1kE)[2],_h));return [0,E(_1kH[1]),_1kH[2]];})]);});},_1kB,function(_1kI){return new F(function(){return A(_1kC,[new T(function(){var _1kJ=E(_1kI),_1kK=B(_5q(_1kJ[1],_1kJ[2],_Bm));return [0,E(_1kK[1]),_1kK[2]];})]);});});});},_1kL=function(_1kM,_1kN,_1kO,_1kP,_1kQ){return new F(function(){return _1ky(_1kM,_1kN,_1kO,_1kQ);});},_1kR=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _1kL(_B3,_B4,_B5,_B6,_zb);});},_1kS=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_1jT,_1kR,_B3,_B4,_B5,_B6,_zb);});},_1kT=new T(function(){return B(unCStr(" )"));}),_1kU=function(_1kV){return function(_1kW,_1kX,_1kY,_1kZ,_1l0){return new F(function(){return _5L(function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_1kS,function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _1hV(function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _Av(_1kV,_B3,_B4,_B5,_B6,_zb);});},_B3,_B4,_B5,_B6,_zb);});},_B3,_B4,_B5,_B6,_zb);});},[1,new T(function(){return B(unAppCStr("legal attribute or entity character or reference (not allowed: ",new T(function(){return B(_0([1,_39,new T(function(){return B(_3b(_1kV,_VL));})],_1kT));})));}),_h],_1kW,_1kX,_1kY,_1kZ,_1l0);});};},_1l1=new T(function(){return B(unCStr("&\""));}),_1l2=new T(function(){return B(_1kU(_1l1));}),_1l3=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _lc(_1l2,_B3,_B4,_B5,_B6,_zb);});},_1l4=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _1ju(_1l3,_B3,_B4,_B5,_B6,_zb);});},_1l5=function(_1l6,_1l7,_1l8,_1l9,_1la){return new F(function(){return _xx(_Lp,_Lp,_1l4,_1l6,function(_1lb,_1lc,_1ld){return new F(function(){return A(_1jk,[_1lb,_1lc,_1l7,_1l8,function(_1le,_1lf,_1lg){return new F(function(){return A(_1l7,[_1le,_1lf,new T(function(){return B(_4a(_1ld,_1lg));})]);});},function(_1lh){return new F(function(){return A(_1l8,[new T(function(){return B(_4a(_1ld,_1lh));})]);});}]);});},_1l8,function(_1li,_1lj,_1lk){return new F(function(){return A(_1jk,[_1li,_1lj,_1l7,_1l8,function(_1ll,_1lm,_1ln){return new F(function(){return A(_1l9,[_1ll,_1lm,new T(function(){return B(_4a(_1lk,_1ln));})]);});},function(_1lo){return new F(function(){return A(_1la,[new T(function(){return B(_4a(_1lk,_1lo));})]);});}]);});},_1la);});},_1lp=new T(function(){return B(unCStr("entity value (in quotes)"));}),_1lq=[1,_1lp,_h],_1lr=[1,_Sx,_h],_1ls=function(_1lt){return function(_1lu,_1lv,_1lw,_1lx,_1ly){return new F(function(){return A(_1lx,[[1,_Sx,new T(function(){return B(_0(_1lt,_1lr));})],_1lu,new T(function(){return [0,E(E(_1lu)[2]),_h];})]);});};},_1lz=new T(function(){return B(unCStr("&\'"));}),_1lA=new T(function(){return B(_1kU(_1lz));}),_1lB=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _lc(_1lA,_B3,_B4,_B5,_B6,_zb);});},_1lC=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _1ju(_1lB,_B3,_B4,_B5,_B6,_zb);});},_1lD=function(_1lE,_1lF,_1lG,_1lH,_1lI){return new F(function(){return _xx(_SP,_SP,_1lC,_1lE,function(_1lJ,_1lK,_1lL){return new F(function(){return A(_1ls,[_1lJ,_1lK,_1lF,_1lG,function(_1lM,_1lN,_1lO){return new F(function(){return A(_1lF,[_1lM,_1lN,new T(function(){return B(_4a(_1lL,_1lO));})]);});},function(_1lP){return new F(function(){return A(_1lG,[new T(function(){return B(_4a(_1lL,_1lP));})]);});}]);});},_1lG,function(_1lQ,_1lR,_1lS){return new F(function(){return A(_1ls,[_1lQ,_1lR,_1lF,_1lG,function(_1lT,_1lU,_1lV){return new F(function(){return A(_1lH,[_1lT,_1lU,new T(function(){return B(_4a(_1lS,_1lV));})]);});},function(_1lW){return new F(function(){return A(_1lI,[new T(function(){return B(_4a(_1lS,_1lW));})]);});}]);});},_1lI);});},_1lX=function(_1lY,_1lZ,_1m0,_1m1,_1m2){var _1m3=function(_1m4,_1m5,_1m6){var _1m7=new T(function(){var _1m8=E(_1m6),_1m9=E(_1m8[2]);if(!_1m9[0]){var _1ma=E(_1m8);}else{var _1mb=B(_5q(_1m8[1],_1m9,_1lq)),_1ma=[0,E(_1mb[1]),_1mb[2]];}var _1mc=_1ma;return _1mc;});return new F(function(){return A(_1ja,[_1m4,_1m5,_1lZ,_1m0,function(_1md,_1me,_1mf){return new F(function(){return A(_1m1,[_1md,_1me,new T(function(){return B(_4a(_1m7,_1mf));})]);});},function(_1mg){return new F(function(){return A(_1m2,[new T(function(){return B(_4a(_1m7,_1mg));})]);});}]);});},_1mh=function(_1mi,_1mj,_1mk){return new F(function(){return A(_1ja,[_1mi,_1mj,_1lZ,_1m0,function(_1ml,_1mm,_1mn){return new F(function(){return A(_1lZ,[_1ml,_1mm,new T(function(){return B(_4a(_1mk,_1mn));})]);});},function(_1mo){return new F(function(){return A(_1m0,[new T(function(){return B(_4a(_1mk,_1mo));})]);});}]);});};return new F(function(){return _1l5(_1lY,_1mh,_1m0,_1m3,function(_1mp){return new F(function(){return _1lD(_1lY,_1mh,_1m0,function(_1mq,_1mr,_1ms){return new F(function(){return _1m3(_1mq,_1mr,new T(function(){var _1mt=E(_1mp),_1mu=E(_1ms),_1mv=B(_3P(_1mt[1],_1mt[2],_1mu[1],_1mu[2]));return [0,E(_1mv[1]),_1mv[2]];}));});},function(_1mw){return new F(function(){return A(_1m2,[new T(function(){var _1mx=E(_1mp),_1my=E(_1mw),_1mz=B(_3P(_1mx[1],_1mx[2],_1my[1],_1my[2])),_1mA=B(_5q(_1mz[1],_1mz[2],_1lq));return [0,E(_1mA[1]),_1mA[2]];})]);});});});});});},_1mB=9,_1mC=new T(function(){return B(unCStr("parameter-entity reference"));}),_1mD=[1,_1mC,_h],_1mE=function(_1mF,_1mG,_1mH,_1mI,_1mJ,_1mK,_1mL,_1mM){return new F(function(){return _uk(_1mF,_1mG,_1mH,_1mI,_1mJ,function(_1mN,_1mO,_1mP){return new F(function(){return A(_zq,[_Bo,_Bn,_1mO,function(_1mQ,_1mR,_1mS){return new F(function(){return A(_1mK,[_1mN,_1mR,new T(function(){var _1mT=E(_1mR)[2],_1mU=E(_1mS),_1mV=B(_3P(_1mU[1],_1mU[2],_1mT,_h)),_1mW=B(_3P(_1mV[1],_1mV[2],_1mT,_h));return [0,E(_1mW[1]),_1mW[2]];})]);});},_1mL,function(_1mX,_1mY,_1mZ){return new F(function(){return A(_1mK,[_1mN,_1mY,new T(function(){var _1n0=E(_1mP),_1n1=E(_1mY)[2],_1n2=E(_1mZ),_1n3=B(_3P(_1n2[1],_1n2[2],_1n1,_h)),_1n4=B(_3P(_1n3[1],_1n3[2],_1n1,_h)),_1n5=B(_3P(_1n0[1],_1n0[2],_1n4[1],_1n4[2]));return [0,E(_1n5[1]),_1n5[2]];})]);});},function(_1n6){return new F(function(){return A(_1mL,[new T(function(){return B(_4a(_1mP,_1n6));})]);});}]);});},_1mL,function(_1n7){return new F(function(){return A(_1mM,[new T(function(){var _1n8=E(_1n7),_1n9=B(_5q(_1n8[1],_1n8[2],_Bq));return [0,E(_1n9[1]),_1n9[2]];})]);});});});},_1na=[0,37],_1nb=new T(function(){return B(_zq(_Bo,_1na));}),_1nc=function(_1nd,_1ne,_1nf,_1ng){return new F(function(){return A(_1nb,[_1nd,function(_1nh,_1ni,_1nj){var _1nk=E(_1ni),_1nl=E(_1nk[2]);return new F(function(){return _1mE(_1nk[1],_1nl[1],_1nl[2],_1nl[3],_1nk[3],_1ne,_1nf,function(_1nm){return new F(function(){return A(_1nf,[new T(function(){return B(_4a(_1nj,_1nm));})]);});});});},_1nf,function(_1nn,_1no,_1np){var _1nq=E(_1no),_1nr=E(_1nq[2]);return new F(function(){return _1mE(_1nq[1],_1nr[1],_1nr[2],_1nr[3],_1nq[3],_1ne,_1nf,function(_1ns){return new F(function(){return A(_1ng,[new T(function(){return B(_4a(_1np,_1ns));})]);});});});},_1ng]);});},_1nt=new T(function(){return B(unCStr("PERef"));}),_1nu=function(_1nv,_1nw,_1nx){var _1ny=function(_1nz){return new F(function(){return A(_1nx,[new T(function(){var _1nA=E(_1nz),_1nB=B(_5q(_1nA[1],_1nA[2],_1mD));return [0,E(_1nB[1]),_1nB[2]];})]);});};return new F(function(){return _1nc(_1nv,function(_1nC,_1nD,_1nE){var _1nF=[0,[8,_1mB,[1,[0,_1nt,_1nC],_h]],_h],_1nG=B(_6v(_6O,_1nF));return new F(function(){return A(_1nw,[_1nF,_1nD,new T(function(){var _1nH=E(_1nE),_1nI=B(_3P(_1nH[1],_1nH[2],E(_1nD)[2],_h));return [0,E(_1nI[1]),_1nI[2]];})]);});},_1ny,_1ny);});},_1nJ=function(_1nK,_1nL,_1nM,_1nN,_1nO){return new F(function(){return _1nu(_1nK,_1nL,_1nO);});},_1nP=function(_1iL,_1iM,_1iN,_1iO,_1iP){return new F(function(){return _1nJ(_1iL,_1iM,_1iN,_1iO,_1iP);});},_1nQ=function(_1nR){return function(_1nS,_1nT,_1nU,_1nV,_1nW){return new F(function(){return A(_1nV,[new T(function(){var _1nX=[0,[0,[1,_1nR,_h]],_h],_1nY=B(_6v(_6O,_1nX));return E(_1nX);}),_1nS,new T(function(){return [0,E(E(_1nS)[2]),_h];})]);});};},_1nZ=[0,37],_1o0=new T(function(){return B(_zq(_1id,_1nZ));}),_1o1=function(_1o2,_1o3,_1o4,_1o5,_1o6){return new F(function(){return A(_1o0,[_1o2,function(_1o7,_1o8,_1o9){return new F(function(){return A(_1nQ,[_1o7,_1o8,_1o3,_1o4,function(_1oa,_1ob,_1oc){return new F(function(){return A(_1o3,[_1oa,_1ob,new T(function(){return B(_4a(_1o9,_1oc));})]);});},function(_1od){return new F(function(){return A(_1o4,[new T(function(){return B(_4a(_1o9,_1od));})]);});}]);});},_1o4,function(_1oe,_1of,_1og){return new F(function(){return A(_1nQ,[_1oe,_1of,_1o3,_1o4,function(_1oh,_1oi,_1oj){return new F(function(){return A(_1o5,[_1oh,_1oi,new T(function(){return B(_4a(_1og,_1oj));})]);});},function(_1ok){return new F(function(){return A(_1o6,[new T(function(){return B(_4a(_1og,_1ok));})]);});}]);});},_1o6]);});},_1ol=function(_1iL,_1iM,_1iN,_1iO,_1iP){return new F(function(){return _4g(_1nP,_1o1,_1iL,_1iM,_1iN,_1iO,_1iP);});},_1om=function(_1iL,_1iM,_1iN,_1iO,_1iP){return new F(function(){return _4g(_1lX,_1ol,_1iL,_1iM,_1iN,_1iO,_1iP);});},_1on=function(_1iL,_1iM,_1iN,_1iO,_1iP){return new F(function(){return _4g(_1iQ,_1om,_1iL,_1iM,_1iN,_1iO,_1iP);});},_1oo=function(_1op,_1oq,_1or,_1os,_1ot){return new F(function(){return _5L(_1on,_1iz,_1op,_1oq,_1or,_1os,_1ot);});},_1ou=function(_1ov,_1ow,_1ox,_1oy,_1oz,_1oA,_1oB){var _1oC=function(_1oD){var _1oE=new T(function(){var _1oF=[0,[8,_1ov,_1ow],_1oD],_1oG=B(_6v(_6O,_1oF));return E(_1oF);});return function(_1oH,_1oI,_1oJ,_1oK,_1oL){return new F(function(){return _1ie(_1oH,function(_1oM,_1oN,_1oO){return new F(function(){return A(_1oI,[_1oE,_1oN,new T(function(){var _1oP=E(_1oO),_1oQ=B(_3P(_1oP[1],_1oP[2],E(_1oN)[2],_h));return [0,E(_1oQ[1]),_1oQ[2]];})]);});},_1oJ,function(_1oR,_1oS,_1oT){return new F(function(){return A(_1oK,[_1oE,_1oS,new T(function(){var _1oU=E(_1oT),_1oV=B(_3P(_1oU[1],_1oU[2],E(_1oS)[2],_h));return [0,E(_1oV[1]),_1oV[2]];})]);});},_1oL);});};};return new F(function(){return _yy(_1oo,_1ox,function(_1oW,_1oX,_1oY){return new F(function(){return A(_1oC,[_1oW,_1oX,_1oy,_1oz,function(_1oZ,_1p0,_1p1){return new F(function(){return A(_1oy,[_1oZ,_1p0,new T(function(){return B(_4a(_1oY,_1p1));})]);});},function(_1p2){return new F(function(){return A(_1oz,[new T(function(){return B(_4a(_1oY,_1p2));})]);});}]);});},_1oz,function(_1p3,_1p4,_1p5){return new F(function(){return A(_1oC,[_1p3,_1p4,_1oy,_1oz,function(_1p6,_1p7,_1p8){return new F(function(){return A(_1oA,[_1p6,_1p7,new T(function(){return B(_4a(_1p5,_1p8));})]);});},function(_1p9){return new F(function(){return A(_1oB,[new T(function(){return B(_4a(_1p5,_1p9));})]);});}]);});},_1oB);});},_1pa=new T(function(){return B(unCStr("column"));}),_1pb=new T(function(){return B(unCStr("line"));}),_1pc=new T(function(){return B(unCStr("source"));}),_1pd=new T(function(){return B(unCStr("<!"));}),_1pe=function(_1pf,_1pg,_1ph,_1pi,_1pj,_1pk){var _1pl=function(_1pm){return function(_1pn,_1po,_1pp,_1pq,_1pr){return new F(function(){return A(_1pq,[[0,new T(function(){return E(E(_1pf)[2]);}),[1,[0,_1pc,new T(function(){return E(E(_1pm)[1]);})],[1,[0,_1pb,new T(function(){return B(_b(0,E(E(_1pm)[2])[1],_h));})],[1,[0,_1pa,new T(function(){return B(_b(0,E(E(_1pm)[3])[1],_h));})],_h]]]],_1pn,new T(function(){return [0,E(E(_1pn)[2]),_h];})]);});};},_1ps=function(_1pt,_1pu,_1pv,_1pw,_1px){return new F(function(){return _Gz(_1id,new T(function(){return E(E(_1pf)[1]);}),_1pt,function(_1py,_1pz,_1pA){var _1pB=new T(function(){var _1pC=E(E(_1pz)[2]),_1pD=_1pC[1];return B(_3H(_1pD,_1pD))==1?[0,E(_1pC),_3O]:[0,E(_1pC),_h];});return new F(function(){return A(_1pl,[new T(function(){return E(E(_1pz)[2]);}),_1pz,_1pu,_1pv,function(_1pE,_1pF,_1pG){return new F(function(){return A(_1pu,[_1pE,_1pF,new T(function(){var _1pH=E(_1pA),_1pI=E(_1pB),_1pJ=E(_1pG),_1pK=B(_3P(_1pI[1],_1pI[2],_1pJ[1],_1pJ[2])),_1pL=B(_3P(_1pH[1],_1pH[2],_1pK[1],_1pK[2]));return [0,E(_1pL[1]),_1pL[2]];})]);});},function(_1pM){return new F(function(){return A(_1pv,[new T(function(){var _1pN=E(_1pA),_1pO=E(_1pB),_1pP=E(_1pM),_1pQ=B(_3P(_1pO[1],_1pO[2],_1pP[1],_1pP[2])),_1pR=B(_3P(_1pN[1],_1pN[2],_1pQ[1],_1pQ[2]));return [0,E(_1pR[1]),_1pR[2]];})]);});}]);});},_1pv,function(_1pS,_1pT,_1pU){var _1pV=new T(function(){var _1pW=E(E(_1pT)[2]),_1pX=_1pW[1];return B(_3H(_1pX,_1pX))==1?[0,E(_1pW),_3O]:[0,E(_1pW),_h];});return new F(function(){return A(_1pl,[new T(function(){return E(E(_1pT)[2]);}),_1pT,_1pu,_1pv,function(_1pY,_1pZ,_1q0){return new F(function(){return A(_1pw,[_1pY,_1pZ,new T(function(){var _1q1=E(_1pU),_1q2=E(_1pV),_1q3=E(_1q0),_1q4=B(_3P(_1q2[1],_1q2[2],_1q3[1],_1q3[2])),_1q5=B(_3P(_1q1[1],_1q1[2],_1q4[1],_1q4[2]));return [0,E(_1q5[1]),_1q5[2]];})]);});},function(_1q6){return new F(function(){return A(_1px,[new T(function(){var _1q7=E(_1pU),_1q8=E(_1pV),_1q9=E(_1q6),_1qa=B(_3P(_1q8[1],_1q8[2],_1q9[1],_1q9[2])),_1qb=B(_3P(_1q7[1],_1q7[2],_1qa[1],_1qa[2]));return [0,E(_1qb[1]),_1qb[2]];})]);});}]);});},_1px);});};return new F(function(){return _Gz(_1id,_1pd,_1pg,function(_1qc,_1qd,_1qe){return new F(function(){return _1ps(_1qd,_1ph,_1pk,function(_1qf,_1qg,_1qh){return new F(function(){return A(_1ph,[_1qf,_1qg,new T(function(){return B(_4a(_1qe,_1qh));})]);});},function(_1qi){return new F(function(){return A(_1pk,[new T(function(){return B(_4a(_1qe,_1qi));})]);});});});},_1pk,function(_1qj,_1qk,_1ql){return new F(function(){return _1ps(_1qk,_1ph,_1pk,function(_1qm,_1qn,_1qo){return new F(function(){return A(_1pj,[_1qm,_1qn,new T(function(){return B(_4a(_1ql,_1qo));})]);});},function(_1qp){return new F(function(){return A(_1pk,[new T(function(){return B(_4a(_1ql,_1qp));})]);});});});},_1pk);});},_1qq=1,_1qr=new T(function(){return B(unCStr("ELEMENT"));}),_1qs=[0,_1qr,_1qq],_1qt=3,_1qu=new T(function(){return B(unCStr("ATTLIST"));}),_1qv=[0,_1qu,_1qt],_1qw=6,_1qx=new T(function(){return B(unCStr("NOTATION"));}),_1qy=[0,_1qx,_1qw],_1qz=[1,_1qy,_h],_1qA=4,_1qB=new T(function(){return B(unCStr("ENTITY"));}),_1qC=[0,_1qB,_1qA],_1qD=[1,_1qC,_1qz],_1qE=[1,_1qv,_1qD],_1qF=[1,_1qs,_1qE],_1qG=new T(function(){return B(_Lw(_1pe,_1qF));}),_1qH=new T(function(){return B(unCStr(": empty list"));}),_1qI=new T(function(){return B(unCStr("Prelude."));}),_1qJ=function(_1qK){return new F(function(){return err(B(_0(_1qI,new T(function(){return B(_0(_1qK,_1qH));}))));});},_1qL=new T(function(){return B(unCStr("foldr1"));}),_1qM=new T(function(){return B(_1qJ(_1qL));}),_1qN=function(_1qO,_1qP){var _1qQ=E(_1qP);if(!_1qQ[0]){return E(_1qM);}else{var _1qR=_1qQ[1],_1qS=E(_1qQ[2]);if(!_1qS[0]){return E(_1qR);}else{return new F(function(){return A(_1qO,[_1qR,new T(function(){return B(_1qN(_1qO,_1qS));})]);});}}},_1qT=new T(function(){return B(_1qN(_4g,_1qG));}),_1qU=function(_1qV,_1qW,_1qX,_1qY,_1qZ){return new F(function(){return A(_1qT,[_1qV,function(_1r0,_1r1,_1r2){var _1r3=E(_1r0);return new F(function(){return _1ou(_1r3[1],_1r3[2],_1r1,_1qW,_1qX,function(_1r4,_1r5,_1r6){return new F(function(){return A(_1qW,[_1r4,_1r5,new T(function(){return B(_4a(_1r2,_1r6));})]);});},function(_1r7){return new F(function(){return A(_1qX,[new T(function(){return B(_4a(_1r2,_1r7));})]);});});});},_1qX,function(_1r8,_1r9,_1ra){var _1rb=E(_1r8);return new F(function(){return _1ou(_1rb[1],_1rb[2],_1r9,_1qW,_1qX,function(_1rc,_1rd,_1re){return new F(function(){return A(_1qY,[_1rc,_1rd,new T(function(){return B(_4a(_1ra,_1re));})]);});},function(_1rf){return new F(function(){return A(_1qZ,[new T(function(){return B(_4a(_1ra,_1rf));})]);});});});},_1qZ]);});},_1rg=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_19U,_1qU,_Xz,_XA,_XB,_XC,_XD);});},_1rh=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_1ah,_1rg,_Xz,_XA,_XB,_XC,_XD);});},_1ri=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _1hV(_1rh,_Xz,_XA,_XB,_XC,_XD);});},_1rj=new T(function(){return B(unCStr("<!["));}),_1rk=7,_1rl=function(_1rm,_1rn,_1ro,_1rp){return new F(function(){return _Gz(_VB,_X9,_1rm,function(_1rq,_1rr,_1rs){return new F(function(){return A(_1rn,[_h,_1rr,new T(function(){var _1rt=E(_1rr)[2],_1ru=E(_1rs),_1rv=B(_3P(_1ru[1],_1ru[2],_1rt,_h)),_1rw=B(_3P(_1rv[1],_1rv[2],_1rt,_h));return [0,E(_1rw[1]),_1rw[2]];})]);});},_1rp,function(_1rx,_1ry,_1rz){return new F(function(){return A(_1ro,[_h,_1ry,new T(function(){var _1rA=E(_1ry)[2],_1rB=E(_1rz),_1rC=B(_3P(_1rB[1],_1rB[2],_1rA,_h)),_1rD=B(_3P(_1rC[1],_1rC[2],_1rA,_h));return [0,E(_1rD[1]),_1rD[2]];})]);});},_1rp);});},_1rE=function(_1rF,_1rG,_1rH,_1rI,_1rJ,_1rK){var _1rL=function(_1rM,_1rN,_1rO){return new F(function(){return A(_1rJ,[new T(function(){return B(unAppCStr("<![",new T(function(){return B(_0(_1rF,new T(function(){return B(unAppCStr("]]>",_1rM));})));})));}),_1rN,new T(function(){var _1rP=E(_1rO),_1rQ=B(_3P(_1rP[1],_1rP[2],E(_1rN)[2],_h));return [0,E(_1rQ[1]),_1rQ[2]];})]);});},_1rR=function(_1rS,_1rT,_1rU){return new F(function(){return A(_1rH,[new T(function(){return B(unAppCStr("<![",new T(function(){return B(_0(_1rF,new T(function(){return B(unAppCStr("]]>",_1rS));})));})));}),_1rT,new T(function(){var _1rV=E(_1rU),_1rW=B(_3P(_1rV[1],_1rV[2],E(_1rT)[2],_h));return [0,E(_1rW[1]),_1rW[2]];})]);});};return new F(function(){return _1rl(_1rG,_1rR,_1rL,function(_1rX){var _1rY=function(_1rZ,_1s0,_1s1){return new F(function(){return _1rL(_1rZ,_1s0,new T(function(){var _1s2=E(_1rX),_1s3=E(_1s1),_1s4=B(_3P(_1s2[1],_1s2[2],_1s3[1],_1s3[2]));return [0,E(_1s4[1]),_1s4[2]];}));});};return new F(function(){return _1s5(_1rG,_1rR,_1rI,_1rY,function(_1s6){return new F(function(){return _1s7(_1rG,_1rR,_1rI,function(_1s8,_1s9,_1sa){return new F(function(){return _1rY(_1s8,_1s9,new T(function(){var _1sb=E(_1s6),_1sc=E(_1sa),_1sd=B(_3P(_1sb[1],_1sb[2],_1sc[1],_1sc[2]));return [0,E(_1sd[1]),_1sd[2]];}));});},function(_1se){return new F(function(){return A(_1rK,[new T(function(){var _1sf=E(_1rX),_1sg=E(_1s6),_1sh=E(_1se),_1si=B(_3P(_1sg[1],_1sg[2],_1sh[1],_1sh[2])),_1sj=B(_3P(_1sf[1],_1sf[2],_1si[1],_1si[2]));return [0,E(_1sj[1]),_1sj[2]];})]);});});});});});});});},_1sk=function(_1sl,_1sm,_1sn,_1so,_1sp){var _1sq=function(_1sr,_1ss,_1st){return new F(function(){return _1rE(_1sr,_1ss,_1sm,_1sn,function(_1su,_1sv,_1sw){return new F(function(){return A(_1so,[_1su,_1sv,new T(function(){return B(_4a(_1st,_1sw));})]);});},function(_1sx){return new F(function(){return A(_1sp,[new T(function(){return B(_4a(_1st,_1sx));})]);});});});},_1sy=function(_1sz,_1sA,_1sB){return new F(function(){return _1rE(_1sz,_1sA,_1sm,_1sn,function(_1sC,_1sD,_1sE){return new F(function(){return A(_1sm,[_1sC,_1sD,new T(function(){return B(_4a(_1sB,_1sE));})]);});},function(_1sF){return new F(function(){return A(_1sn,[new T(function(){return B(_4a(_1sB,_1sF));})]);});});});};return new F(function(){return _1rl(_1sl,_1sy,_1sq,function(_1sG){return new F(function(){return _1s5(_1sl,_1sy,_1sn,function(_1sH,_1sI,_1sJ){return new F(function(){return _1sq(_1sH,_1sI,new T(function(){return B(_4a(_1sG,_1sJ));}));});},function(_1sK){return new F(function(){return _1s7(_1sl,_1sy,_1sn,function(_1sL,_1sM,_1sN){return new F(function(){return _1sq(_1sL,_1sM,new T(function(){var _1sO=E(_1sG),_1sP=E(_1sK),_1sQ=E(_1sN),_1sR=B(_3P(_1sP[1],_1sP[2],_1sQ[1],_1sQ[2])),_1sS=B(_3P(_1sO[1],_1sO[2],_1sR[1],_1sR[2]));return [0,E(_1sS[1]),_1sS[2]];}));});},function(_1sT){return new F(function(){return A(_1sp,[new T(function(){var _1sU=E(_1sG),_1sV=E(_1sK),_1sW=E(_1sT),_1sX=B(_3P(_1sV[1],_1sV[2],_1sW[1],_1sW[2])),_1sY=B(_3P(_1sU[1],_1sU[2],_1sX[1],_1sX[2]));return [0,E(_1sY[1]),_1sY[2]];})]);});});});});});});});},_1s5=function(_1sZ,_1t0,_1t1,_1t2,_1t3){return new F(function(){return _Gz(_VB,_1rj,_1sZ,function(_1t4,_1t5,_1t6){var _1t7=new T(function(){var _1t8=E(_1t6),_1t9=B(_3P(_1t8[1],_1t8[2],E(_1t5)[2],_h));return [0,E(_1t9[1]),_1t9[2]];});return new F(function(){return _1sk(_1t5,_1t0,_1t1,function(_1ta,_1tb,_1tc){return new F(function(){return A(_1t0,[_1ta,_1tb,new T(function(){return B(_4a(_1t7,_1tc));})]);});},function(_1td){return new F(function(){return A(_1t1,[new T(function(){return B(_4a(_1t7,_1td));})]);});});});},_1t3,function(_1te,_1tf,_1tg){var _1th=new T(function(){var _1ti=E(_1tg),_1tj=B(_3P(_1ti[1],_1ti[2],E(_1tf)[2],_h));return [0,E(_1tj[1]),_1tj[2]];});return new F(function(){return _1sk(_1tf,_1t0,_1t1,function(_1tk,_1tl,_1tm){return new F(function(){return A(_1t2,[_1tk,_1tl,new T(function(){return B(_4a(_1th,_1tm));})]);});},function(_1tn){return new F(function(){return A(_1t3,[new T(function(){return B(_4a(_1th,_1tn));})]);});});});},_1t3);});},_1to=function(_1tp,_1tq,_1tr,_1ts,_1tt,_1tu){var _1tv=function(_1tw,_1tx,_1ty){return new F(function(){return A(_1tt,[[1,_1tp,_1tw],_1tx,new T(function(){var _1tz=E(_1ty),_1tA=B(_3P(_1tz[1],_1tz[2],E(_1tx)[2],_h));return [0,E(_1tA[1]),_1tA[2]];})]);});},_1tB=function(_1tC,_1tD,_1tE){return new F(function(){return A(_1tr,[[1,_1tp,_1tC],_1tD,new T(function(){var _1tF=E(_1tE),_1tG=B(_3P(_1tF[1],_1tF[2],E(_1tD)[2],_h));return [0,E(_1tG[1]),_1tG[2]];})]);});};return new F(function(){return _1rl(_1tq,_1tB,_1tv,function(_1tH){var _1tI=function(_1tJ,_1tK,_1tL){return new F(function(){return _1tv(_1tJ,_1tK,new T(function(){var _1tM=E(_1tH),_1tN=E(_1tL),_1tO=B(_3P(_1tM[1],_1tM[2],_1tN[1],_1tN[2]));return [0,E(_1tO[1]),_1tO[2]];}));});};return new F(function(){return _1s5(_1tq,_1tB,_1ts,_1tI,function(_1tP){return new F(function(){return _1s7(_1tq,_1tB,_1ts,function(_1tQ,_1tR,_1tS){return new F(function(){return _1tI(_1tQ,_1tR,new T(function(){var _1tT=E(_1tP),_1tU=E(_1tS),_1tV=B(_3P(_1tT[1],_1tT[2],_1tU[1],_1tU[2]));return [0,E(_1tV[1]),_1tV[2]];}));});},function(_1tW){return new F(function(){return A(_1tu,[new T(function(){var _1tX=E(_1tH),_1tY=E(_1tP),_1tZ=E(_1tW),_1u0=B(_3P(_1tY[1],_1tY[2],_1tZ[1],_1tZ[2])),_1u1=B(_3P(_1tX[1],_1tX[2],_1u0[1],_1u0[2]));return [0,E(_1u1[1]),_1u1[2]];})]);});});});});});});});},_1u2=[0,9],_1u3=[1,_1u2,_h],_1u4=[0,10],_1u5=[1,_1u4,_1u3],_1u6=new T(function(){return B(unCStr("legal XML character"));}),_1u7=[1,_1u6,_h],_1s7=function(_1u8,_1u9,_1ua,_1ub,_1uc){var _1ud=E(_1u8),_1ue=_1ud[3],_1uf=E(_1ud[2]),_1ug=_1uf[1],_1uh=E(_1uf[2]),_1ui=E(_1uf[3])[1],_1uj=function(_1uk,_1ul,_1um){return new F(function(){return _1to(_1uk,_1ul,_1u9,_1ua,function(_1un,_1uo,_1up){return new F(function(){return A(_1u9,[_1un,_1uo,new T(function(){return B(_4a(_1um,_1up));})]);});},function(_1uq){return new F(function(){return A(_1ua,[new T(function(){return B(_4a(_1um,_1uq));})]);});});});},_1ur=function(_1us){return new F(function(){return _A7(_1ud,_1uj,_1ua,function(_1ut,_1uu,_1uv){var _1uw=new T(function(){var _1ux=E(_1uv),_1uy=_1ux[1],_1uz=E(_1ux[2]);if(!_1uz[0]){var _1uA=E(_1us),_1uB=B(_3P(_1uA[1],_1uA[2],_1uy,_h)),_1uC=_1uB[1],_1uD=E(_1uB[2]);if(!_1uD[0]){var _1uE=[0,E(_1uC),_h];}else{var _1uF=B(_5q(_1uC,_1uD,_1u7)),_1uE=[0,E(_1uF[1]),_1uF[2]];}var _1uG=_1uE,_1uH=_1uG,_1uI=_1uH;}else{var _1uJ=B(_5q(_1uy,_1uz,_zo)),_1uK=E(_1us),_1uL=B(_3P(_1uK[1],_1uK[2],_1uJ[1],_1uJ[2])),_1uM=_1uL[1],_1uN=E(_1uL[2]);if(!_1uN[0]){var _1uO=[0,E(_1uM),_h];}else{var _1uP=B(_5q(_1uM,_1uN,_1u7)),_1uO=[0,E(_1uP[1]),_1uP[2]];}var _1uQ=_1uO,_1uR=_1uQ,_1uS=_1uR,_1uI=_1uS;}var _1uT=_1uI;return _1uT;});return new F(function(){return _1to(_1ut,_1uu,_1u9,_1ua,function(_1uU,_1uV,_1uW){return new F(function(){return A(_1ub,[_1uU,_1uV,new T(function(){return B(_4a(_1uw,_1uW));})]);});},function(_1uX){return new F(function(){return A(_1uc,[new T(function(){return B(_4a(_1uw,_1uX));})]);});});});},function(_1uY){return new F(function(){return A(_1uc,[new T(function(){var _1uZ=E(_1us),_1v0=E(_1uY),_1v1=B(_5q(_1v0[1],_1v0[2],_zo)),_1v2=B(_3P(_1uZ[1],_1uZ[2],_1v1[1],_1v1[2])),_1v3=B(_5q(_1v2[1],_1v2[2],_1u7));return [0,E(_1v3[1]),_1v3[2]];})]);});});});},_1v4=E(_1ud[1]);if(!_1v4[0]){return new F(function(){return _1ur([0,E(_1uf),_7L]);});}else{var _1v5=_1v4[2],_1v6=E(_1v4[1]),_1v7=_1v6[1],_1v8=new T(function(){switch(E(_1v7)){case 9:var _1v9=[0,_1ug,E(_1uh),E([0,(_1ui+8|0)-B(_lF(_1ui-1|0,8))|0])],_1va=B(_1uj(_1v6,[0,_1v5,E(_1v9),E(_1ue)],[0,E(_1v9),_h]));break;case 10:var _1vb=[0,_1ug,E([0,_1uh[1]+1|0]),E(_kT)],_1va=B(_1uj(_1v6,[0,_1v5,E(_1vb),E(_1ue)],[0,E(_1vb),_h]));break;default:var _1vc=[0,_1ug,E(_1uh),E([0,_1ui+1|0])],_1va=B(_1uj(_1v6,[0,_1v5,E(_1vc),E(_1ue)],[0,E(_1vc),_h]));}return _1va;}),_1vd=new T(function(){if(!B(_kO(_7I,_1v6,_1u5))){var _1ve=new T(function(){return B(_1ur([0,E(_1uf),[1,[0,E([1,_39,new T(function(){return B(_3b([1,_1v6,_h],_7J));})])],_h]]));}),_1vf=_1v7<57344?E(_1ve):_1v7>65533?_1v7<65536?E(_1ve):E(_1v8):E(_1v8);}else{var _1vf=E(_1v8);}return _1vf;});return _1v7<32?E(_1vd):_1v7>55295?E(_1vd):E(_1v8);}},_1vg=new T(function(){return B(_zq(_VB,_1hP));}),_1vh=function(_1vi,_1vj,_1vk,_1vl,_1vm,_1vn){var _1vo=function(_1vp){return function(_1vq,_1vr,_1vs,_1vt,_1vu){return new F(function(){return A(_1vt,[new T(function(){var _1vv=[0,[8,_1rk,[1,[0,_10G,_1vp],_h]],_1vi],_1vw=B(_6v(_6O,_1vv));return E(_1vv);}),_1vq,new T(function(){return [0,E(E(_1vq)[2]),_h];})]);});};},_1vx=function(_1vy,_1vz,_1vA,_1vB,_1vC){var _1vD=function(_1vE,_1vF,_1vG){return new F(function(){return A(_1vo,[_1vE,_1vF,_1vz,_1vA,function(_1vH,_1vI,_1vJ){return new F(function(){return A(_1vB,[_1vH,_1vI,new T(function(){return B(_4a(_1vG,_1vJ));})]);});},function(_1vK){return new F(function(){return A(_1vC,[new T(function(){return B(_4a(_1vG,_1vK));})]);});}]);});},_1vL=function(_1vM,_1vN,_1vO){return new F(function(){return A(_1vo,[_1vM,_1vN,_1vz,_1vA,function(_1vP,_1vQ,_1vR){return new F(function(){return A(_1vz,[_1vP,_1vQ,new T(function(){return B(_4a(_1vO,_1vR));})]);});},function(_1vS){return new F(function(){return A(_1vA,[new T(function(){return B(_4a(_1vO,_1vS));})]);});}]);});};return new F(function(){return _1rl(_1vy,_1vL,_1vD,function(_1vT){return new F(function(){return _1s5(_1vy,_1vL,_1vA,function(_1vU,_1vV,_1vW){return new F(function(){return _1vD(_1vU,_1vV,new T(function(){return B(_4a(_1vT,_1vW));}));});},function(_1vX){return new F(function(){return _1s7(_1vy,_1vL,_1vA,function(_1vY,_1vZ,_1w0){return new F(function(){return _1vD(_1vY,_1vZ,new T(function(){var _1w1=E(_1vT),_1w2=E(_1vX),_1w3=E(_1w0),_1w4=B(_3P(_1w2[1],_1w2[2],_1w3[1],_1w3[2])),_1w5=B(_3P(_1w1[1],_1w1[2],_1w4[1],_1w4[2]));return [0,E(_1w5[1]),_1w5[2]];}));});},function(_1w6){return new F(function(){return A(_1vC,[new T(function(){var _1w7=E(_1vT),_1w8=E(_1vX),_1w9=E(_1w6),_1wa=B(_3P(_1w8[1],_1w8[2],_1w9[1],_1w9[2])),_1wb=B(_3P(_1w7[1],_1w7[2],_1wa[1],_1wa[2]));return [0,E(_1wb[1]),_1wb[2]];})]);});});});});});});});};return new F(function(){return A(_1vg,[_1vj,function(_1wc,_1wd,_1we){return new F(function(){return _1vx(_1wd,_1vk,_1vl,function(_1wf,_1wg,_1wh){return new F(function(){return A(_1vk,[_1wf,_1wg,new T(function(){return B(_4a(_1we,_1wh));})]);});},function(_1wi){return new F(function(){return A(_1vl,[new T(function(){return B(_4a(_1we,_1wi));})]);});});});},_1vl,function(_1wj,_1wk,_1wl){return new F(function(){return _1vx(_1wk,_1vk,_1vl,function(_1wm,_1wn,_1wo){return new F(function(){return A(_1vm,[_1wm,_1wn,new T(function(){return B(_4a(_1wl,_1wo));})]);});},function(_1wp){return new F(function(){return A(_1vn,[new T(function(){return B(_4a(_1wl,_1wp));})]);});});});},_1vn]);});},_1wq=function(_1wr,_1ws,_1wt,_1wu,_1wv){return new F(function(){return _lc(_1oo,_1wr,function(_1ww,_1wx,_1wy){return new F(function(){return _1vh(_1ww,_1wx,_1ws,_1wt,function(_1wz,_1wA,_1wB){return new F(function(){return A(_1ws,[_1wz,_1wA,new T(function(){return B(_4a(_1wy,_1wB));})]);});},function(_1wC){return new F(function(){return A(_1wt,[new T(function(){return B(_4a(_1wy,_1wC));})]);});});});},_1wt,function(_1wD,_1wE,_1wF){return new F(function(){return _1vh(_1wD,_1wE,_1ws,_1wt,function(_1wG,_1wH,_1wI){return new F(function(){return A(_1wu,[_1wG,_1wH,new T(function(){return B(_4a(_1wF,_1wI));})]);});},function(_1wJ){return new F(function(){return A(_1wv,[new T(function(){return B(_4a(_1wF,_1wJ));})]);});});});},_1wv);});},_1wK=function(_1wL,_1wM,_1wN,_1wO,_1wP){return new F(function(){return _Gz(_VB,_1rj,_1wL,function(_1wQ,_1wR,_1wS){var _1wT=new T(function(){var _1wU=E(_1wS),_1wV=B(_3P(_1wU[1],_1wU[2],E(_1wR)[2],_h));return [0,E(_1wV[1]),_1wV[2]];});return new F(function(){return _1wq(_1wR,_1wM,_1wN,function(_1wW,_1wX,_1wY){return new F(function(){return A(_1wM,[_1wW,_1wX,new T(function(){return B(_4a(_1wT,_1wY));})]);});},function(_1wZ){return new F(function(){return A(_1wN,[new T(function(){return B(_4a(_1wT,_1wZ));})]);});});});},_1wP,function(_1x0,_1x1,_1x2){var _1x3=new T(function(){var _1x4=E(_1x2),_1x5=B(_3P(_1x4[1],_1x4[2],E(_1x1)[2],_h));return [0,E(_1x5[1]),_1x5[2]];});return new F(function(){return _1wq(_1x1,_1wM,_1wN,function(_1x6,_1x7,_1x8){return new F(function(){return A(_1wO,[_1x6,_1x7,new T(function(){return B(_4a(_1x3,_1x8));})]);});},function(_1x9){return new F(function(){return A(_1wP,[new T(function(){return B(_4a(_1x3,_1x9));})]);});});});},_1wP);});},_1xa=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _1hV(_1wK,_Xz,_XA,_XB,_XC,_XD);});},_1xb=function(_1xc,_1xd,_1xe,_1xf,_1xg){return new F(function(){return _v8(_RP,_1xc,function(_1xh,_1xi,_1xj){return new F(function(){return A(_1xd,[_h,_1xi,new T(function(){var _1xk=E(_1xj),_1xl=B(_3P(_1xk[1],_1xk[2],E(_1xi)[2],_h));return [0,E(_1xl[1]),_1xl[2]];})]);});},_1xe,function(_1xm,_1xn,_1xo){return new F(function(){return A(_1xf,[_h,_1xn,new T(function(){var _1xp=E(_1xo),_1xq=B(_3P(_1xp[1],_1xp[2],E(_1xn)[2],_h));return [0,E(_1xq[1]),_1xq[2]];})]);});},_1xg);});},_1xr=function(_1xs,_1xt,_1xu,_1xv){var _1xw=function(_1xx){return new F(function(){return A(_1xv,[new T(function(){var _1xy=E(_1xx),_1xz=B(_5q(_1xy[1],_1xy[2],_1mD));return [0,E(_1xz[1]),_1xz[2]];})]);});};return new F(function(){return _1nc(_1xs,function(_1xA,_1xB,_1xC){var _1xD=[0,[8,_1mB,[1,[0,_1nt,_1xA],_h]],_h],_1xE=B(_6v(_6O,_1xD));return new F(function(){return A(_1xt,[_1xD,_1xB,new T(function(){var _1xF=E(_1xC),_1xG=B(_3P(_1xF[1],_1xF[2],E(_1xB)[2],_h));return [0,E(_1xG[1]),_1xG[2]];})]);});},_1xw,_1xw);});},_1xH=function(_1xI,_1xJ,_1xK,_1xL,_1xM){return new F(function(){return _1xr(_1xI,_1xJ,_1xK,_1xM);});},_1xN=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _1xH(_B3,_B4,_B5,_B6,_zb);});},_1xO=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _1hV(_1xN,_Xz,_XA,_XB,_XC,_XD);});},_1xP=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_1xO,_1xb,_Xz,_XA,_XB,_XC,_XD);});},_1xQ=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_1xP,_1xa,_Xz,_XA,_XB,_XC,_XD);});},_1xR=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _4g(_1ri,_1xQ,_Xz,_XA,_XB,_XC,_XD);});},_1xS=function(_1xT){var _1xU=E(_1xT);if(!_1xU[0]){return [0];}else{return new F(function(){return _0(_1xU[1],new T(function(){return B(_1xS(_1xU[2]));}));});}},_1xV=function(_1xW,_1xX,_1xY,_1xZ,_1y0){return new F(function(){return _lc(_1xR,_1xW,function(_1y1,_1y2,_1y3){return new F(function(){return A(_1xX,[new T(function(){return B(_1xS(_1y1));}),_1y2,new T(function(){var _1y4=E(_1y3),_1y5=B(_3P(_1y4[1],_1y4[2],E(_1y2)[2],_h));return [0,E(_1y5[1]),_1y5[2]];})]);});},_1xY,function(_1y6,_1y7,_1y8){return new F(function(){return A(_1xZ,[new T(function(){return B(_1xS(_1y6));}),_1y7,new T(function(){var _1y9=E(_1y8),_1ya=B(_3P(_1y9[1],_1y9[2],E(_1y7)[2],_h));return [0,E(_1ya[1]),_1ya[2]];})]);});},_1y0);});},_1yb=function(_1yc,_1yd,_1ye,_1yf,_1yg,_1yh,_1yi,_1yj){return new F(function(){return _uk(_1yc,_1yd,_1ye,_1yf,_1yg,function(_1yk,_1yl,_1ym){var _1yn=function(_1yo,_1yp,_1yq,_1yr,_1ys,_1yt){var _1yu=function(_1yv){return function(_1yw,_1yx,_1yy,_1yz,_1yA){return new F(function(){return A(_1yz,[[1,new T(function(){var _1yB=[0,[8,_1bJ,[1,[0,_1hO,_1yk],_1yo]],_1yv],_1yC=B(_6v(_6O,_1yB));return E(_1yB);}),_h],_1yw,new T(function(){return [0,E(E(_1yw)[2]),_h];})]);});};},_1yD=function(_1yE,_1yF,_1yG,_1yH,_1yI){var _1yJ=function(_1yK,_1yL,_1yM){return new F(function(){return A(_1yu,[_1yK,_1yL,_1yF,_1yG,function(_1yN,_1yO,_1yP){return new F(function(){return A(_1yH,[_1yN,_1yO,new T(function(){return B(_4a(_1yM,_1yP));})]);});},function(_1yQ){return new F(function(){return A(_1yI,[new T(function(){return B(_4a(_1yM,_1yQ));})]);});}]);});},_1yR=function(_1yS,_1yT,_1yU){return new F(function(){return A(_1yu,[_1yS,_1yT,_1yF,_1yG,function(_1yV,_1yW,_1yX){return new F(function(){return A(_1yF,[_1yV,_1yW,new T(function(){return B(_4a(_1yU,_1yX));})]);});},function(_1yY){return new F(function(){return A(_1yG,[new T(function(){return B(_4a(_1yU,_1yY));})]);});}]);});};return new F(function(){return _xx(_1hQ,_1hS,_1xV,_1yE,function(_1yZ,_1z0,_1z1){return new F(function(){return _7g(_RP,_1z0,function(_1z2,_1z3,_1z4){return new F(function(){return _1yR(_1yZ,_1z3,new T(function(){var _1z5=E(_1z4),_1z6=B(_3P(_1z5[1],_1z5[2],E(_1z3)[2],_h));return [0,E(_1z6[1]),_1z6[2]];}));});},_1yG,function(_1z7,_1z8,_1z9){return new F(function(){return _1yR(_1yZ,_1z8,new T(function(){var _1za=E(_1z1),_1zb=E(_1z9),_1zc=B(_3P(_1zb[1],_1zb[2],E(_1z8)[2],_h)),_1zd=B(_3P(_1za[1],_1za[2],_1zc[1],_1zc[2]));return [0,E(_1zd[1]),_1zd[2]];}));});});});},_1yG,function(_1ze,_1zf,_1zg){return new F(function(){return _7g(_RP,_1zf,function(_1zh,_1zi,_1zj){return new F(function(){return _1yR(_1ze,_1zi,new T(function(){var _1zk=E(_1zj),_1zl=B(_3P(_1zk[1],_1zk[2],E(_1zi)[2],_h));return [0,E(_1zl[1]),_1zl[2]];}));});},_1yG,function(_1zm,_1zn,_1zo){return new F(function(){return _1yJ(_1ze,_1zn,new T(function(){var _1zp=E(_1zg),_1zq=E(_1zo),_1zr=B(_3P(_1zq[1],_1zq[2],E(_1zn)[2],_h)),_1zs=B(_3P(_1zp[1],_1zp[2],_1zr[1],_1zr[2]));return [0,E(_1zs[1]),_1zs[2]];}));});});});},function(_1zt){return new F(function(){return _1yJ(_h,_1yE,new T(function(){var _1zu=E(_1zt),_1zv=B(_3P(_1zu[1],_1zu[2],E(_1yE)[2],_h));return [0,E(_1zv[1]),_1zv[2]];}));});});});};return new F(function(){return _7g(_RP,_1yp,function(_1zw,_1zx,_1zy){return new F(function(){return _1yD(_1zx,_1yq,_1yr,function(_1zz,_1zA,_1zB){return new F(function(){return A(_1yq,[_1zz,_1zA,new T(function(){return B(_4a(_1zy,_1zB));})]);});},function(_1zC){return new F(function(){return A(_1yr,[new T(function(){return B(_4a(_1zy,_1zC));})]);});});});},_1yr,function(_1zD,_1zE,_1zF){return new F(function(){return _1yD(_1zE,_1yq,_1yr,function(_1zG,_1zH,_1zI){return new F(function(){return A(_1ys,[_1zG,_1zH,new T(function(){return B(_4a(_1zF,_1zI));})]);});},function(_1zJ){return new F(function(){return A(_1yt,[new T(function(){return B(_4a(_1zF,_1zJ));})]);});});});});});},_1zK=function(_1zL,_1zM,_1zN){return new F(function(){return _1yn(_1zL,_1zM,_1yh,_1yi,function(_1zO,_1zP,_1zQ){return new F(function(){return A(_1yh,[_1zO,_1zP,new T(function(){var _1zR=E(_1ym),_1zS=E(_1zN),_1zT=E(_1zQ),_1zU=B(_3P(_1zS[1],_1zS[2],_1zT[1],_1zT[2])),_1zV=B(_3P(_1zR[1],_1zR[2],_1zU[1],_1zU[2]));return [0,E(_1zV[1]),_1zV[2]];})]);});},function(_1zW){return new F(function(){return A(_1yi,[new T(function(){var _1zX=E(_1ym),_1zY=E(_1zN),_1zZ=E(_1zW),_1A0=B(_3P(_1zY[1],_1zY[2],_1zZ[1],_1zZ[2])),_1A1=B(_3P(_1zX[1],_1zX[2],_1A0[1],_1A0[2]));return [0,E(_1A1[1]),_1A1[2]];})]);});});});},_1A2=function(_1A3){return new F(function(){return _1zK(_h,_1yl,new T(function(){var _1A4=E(_1A3),_1A5=B(_3P(_1A4[1],_1A4[2],E(_1yl)[2],_h));return [0,E(_1A5[1]),_1A5[2]];}));});},_1A6=function(_1A7,_1A8,_1A9){return new F(function(){return _1yn(_1A7,_1A8,_1yh,_1yi,function(_1Aa,_1Ab,_1Ac){return new F(function(){return A(_1yh,[_1Aa,_1Ab,new T(function(){return B(_4a(_1A9,_1Ac));})]);});},function(_1Ad){return new F(function(){return A(_1yi,[new T(function(){return B(_4a(_1A9,_1Ad));})]);});});});};return new F(function(){return _v8(_RP,_1yl,function(_1Ae,_1Af,_1Ag){var _1Ah=function(_1Ai,_1Aj,_1Ak){return new F(function(){return _1A6(_1Ai,_1Aj,new T(function(){var _1Al=E(_1Ak),_1Am=_1Al[1],_1An=E(_1Al[2]);if(!_1An[0]){var _1Ao=E(_1Ag),_1Ap=B(_3P(_1Ao[1],_1Ao[2],_1Am,_h)),_1Aq=[0,E(_1Ap[1]),_1Ap[2]];}else{var _1Ar=B(_5q(_1Am,_1An,_1hU)),_1As=E(_1Ag),_1At=B(_3P(_1As[1],_1As[2],_1Ar[1],_1Ar[2])),_1Aq=[0,E(_1At[1]),_1At[2]];}var _1Au=_1Aq;return _1Au;}));});};return new F(function(){return _1dN(_1Af,_1A6,_1A2,_1Ah,function(_1Av){return new F(function(){return _1hu(_1Af,_1A6,_1A2,function(_1Aw,_1Ax,_1Ay){return new F(function(){return _1Ah(_1Aw,_1Ax,new T(function(){var _1Az=E(_1Av),_1AA=E(_1Ay),_1AB=B(_3P(_1Az[1],_1Az[2],_1AA[1],_1AA[2]));return [0,E(_1AB[1]),_1AB[2]];}));});},function(_1AC){return new F(function(){return _1zK(_h,_1yl,new T(function(){var _1AD=E(_1Ag),_1AE=E(_1Av),_1AF=E(_1AC),_1AG=B(_3P(_1AE[1],_1AE[2],_1AF[1],_1AF[2])),_1AH=B(_5q(_1AG[1],_1AG[2],_1hU)),_1AI=B(_3P(_1AD[1],_1AD[2],_1AH[1],_1AH[2])),_1AJ=B(_3P(_1AI[1],_1AI[2],E(_1yl)[2],_h));return [0,E(_1AJ[1]),_1AJ[2]];}));});});});});});},_1A2,function(_1AK,_1AL,_1AM){var _1AN=function(_1AO,_1AP,_1AQ){return new F(function(){return _1zK(_1AO,_1AP,new T(function(){var _1AR=E(_1AQ),_1AS=_1AR[1],_1AT=E(_1AR[2]);if(!_1AT[0]){var _1AU=E(_1AM),_1AV=B(_3P(_1AU[1],_1AU[2],_1AS,_h)),_1AW=[0,E(_1AV[1]),_1AV[2]];}else{var _1AX=B(_5q(_1AS,_1AT,_1hU)),_1AY=E(_1AM),_1AZ=B(_3P(_1AY[1],_1AY[2],_1AX[1],_1AX[2])),_1AW=[0,E(_1AZ[1]),_1AZ[2]];}var _1B0=_1AW;return _1B0;}));});};return new F(function(){return _1dN(_1AL,_1A6,_1A2,_1AN,function(_1B1){return new F(function(){return _1hu(_1AL,_1A6,_1A2,function(_1B2,_1B3,_1B4){return new F(function(){return _1AN(_1B2,_1B3,new T(function(){var _1B5=E(_1B1),_1B6=E(_1B4),_1B7=B(_3P(_1B5[1],_1B5[2],_1B6[1],_1B6[2]));return [0,E(_1B7[1]),_1B7[2]];}));});},function(_1B8){return new F(function(){return _1zK(_h,_1yl,new T(function(){var _1B9=E(_1AM),_1Ba=E(_1B1),_1Bb=E(_1B8),_1Bc=B(_3P(_1Ba[1],_1Ba[2],_1Bb[1],_1Bb[2])),_1Bd=B(_5q(_1Bc[1],_1Bc[2],_1hU)),_1Be=B(_3P(_1B9[1],_1B9[2],_1Bd[1],_1Bd[2])),_1Bf=B(_3P(_1Be[1],_1Be[2],E(_1yl)[2],_h));return [0,E(_1Bf[1]),_1Bf[2]];}));});});});});});},_1A2);});},_1yi,function(_1Bg){return new F(function(){return A(_1yj,[new T(function(){var _1Bh=E(_1Bg),_1Bi=B(_5q(_1Bh[1],_1Bh[2],_Bq));return [0,E(_1Bi[1]),_1Bi[2]];})]);});});});},_1Bj=function(_1Bk,_1Bl,_1Bm,_1Bn){return new F(function(){return _v8(_RP,_1Bk,function(_1Bo,_1Bp,_1Bq){var _1Br=E(_1Bp),_1Bs=E(_1Br[2]);return new F(function(){return _1yb(_1Br[1],_1Bs[1],_1Bs[2],_1Bs[3],_1Br[3],_1Bl,_1Bm,function(_1Bt){return new F(function(){return A(_1Bm,[new T(function(){return B(_4a(_1Bq,_1Bt));})]);});});});},_1Bm,function(_1Bu,_1Bv,_1Bw){var _1Bx=E(_1Bv),_1By=E(_1Bx[2]);return new F(function(){return _1yb(_1Bx[1],_1By[1],_1By[2],_1By[3],_1Bx[3],_1Bl,_1Bm,function(_1Bz){return new F(function(){return A(_1Bn,[new T(function(){return B(_4a(_1Bw,_1Bz));})]);});});});},_1Bn);});},_1BA=function(_1BB,_1BC,_1BD,_1BE,_1BF){return new F(function(){return _1Bj(_1BB,_1BC,_1BD,_1BF);});},_1BG=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _1BA(_Xz,_XA,_XB,_XC,_XD);});},_1BH=new T(function(){return B(_zq(_VB,_14p));}),_1BI=new T(function(){return B(unCStr("<!DOCTYPE"));}),_1BJ=function(_1BK,_1BL,_1BM,_1BN,_1BO){return new F(function(){return _Gz(_VB,_1BI,_1BK,_1BL,_1BO,_1BN,_1BO);});},_1BP=function(_1BQ,_1BR,_1BS,_1BT,_1BU,_1BV){var _1BW=function(_1BX,_1BY,_1BZ,_1C0,_1C1,_1C2){var _1C3=function(_1C4,_1C5,_1C6,_1C7,_1C8,_1C9){return new F(function(){return _lc(_1aS,_1C5,function(_1Ca,_1Cb,_1Cc){return new F(function(){return A(_1C6,[new T(function(){return B(_0(_1BQ,new T(function(){return B(_0(_1BX,new T(function(){return B(_0(_1C4,_1Ca));})));})));}),_1Cb,new T(function(){var _1Cd=E(_1Cc),_1Ce=B(_3P(_1Cd[1],_1Cd[2],E(_1Cb)[2],_h));return [0,E(_1Ce[1]),_1Ce[2]];})]);});},_1C7,function(_1Cf,_1Cg,_1Ch){return new F(function(){return A(_1C8,[new T(function(){return B(_0(_1BQ,new T(function(){return B(_0(_1BX,new T(function(){return B(_0(_1C4,_1Cf));})));})));}),_1Cg,new T(function(){var _1Ci=E(_1Ch),_1Cj=B(_3P(_1Ci[1],_1Ci[2],E(_1Cg)[2],_h));return [0,E(_1Cj[1]),_1Cj[2]];})]);});},_1C9);});},_1Ck=function(_1Cl,_1Cm,_1Cn){return new F(function(){return _1C3(_1Cl,_1Cm,_1BZ,_1C0,function(_1Co,_1Cp,_1Cq){return new F(function(){return A(_1C1,[_1Co,_1Cp,new T(function(){return B(_4a(_1Cn,_1Cq));})]);});},function(_1Cr){return new F(function(){return A(_1C2,[new T(function(){return B(_4a(_1Cn,_1Cr));})]);});});});};return new F(function(){return _xx(_1BJ,_1BH,_1BG,_1BY,function(_1Cs,_1Ct,_1Cu){return new F(function(){return _1C3(_1Cs,_1Ct,_1BZ,_1C0,function(_1Cv,_1Cw,_1Cx){return new F(function(){return A(_1BZ,[_1Cv,_1Cw,new T(function(){return B(_4a(_1Cu,_1Cx));})]);});},function(_1Cy){return new F(function(){return A(_1C0,[new T(function(){return B(_4a(_1Cu,_1Cy));})]);});});});},_1C0,_1Ck,function(_1Cz){return new F(function(){return _1Ck(_h,_1BY,new T(function(){var _1CA=E(_1Cz),_1CB=B(_3P(_1CA[1],_1CA[2],E(_1BY)[2],_h));return [0,E(_1CB[1]),_1CB[2]];}));});});});};return new F(function(){return _lc(_1aS,_1BR,function(_1CC,_1CD,_1CE){return new F(function(){return _1BW(_1CC,_1CD,_1BS,_1BT,function(_1CF,_1CG,_1CH){return new F(function(){return A(_1BS,[_1CF,_1CG,new T(function(){return B(_4a(_1CE,_1CH));})]);});},function(_1CI){return new F(function(){return A(_1BT,[new T(function(){return B(_4a(_1CE,_1CI));})]);});});});},_1BT,function(_1CJ,_1CK,_1CL){return new F(function(){return _1BW(_1CJ,_1CK,_1BS,_1BT,function(_1CM,_1CN,_1CO){return new F(function(){return A(_1BU,[_1CM,_1CN,new T(function(){return B(_4a(_1CL,_1CO));})]);});},function(_1CP){return new F(function(){return A(_1BV,[new T(function(){return B(_4a(_1CL,_1CP));})]);});});});},_1BV);});},_1CQ=new T(function(){return B(unCStr("encoding"));}),_1CR=new T(function(){return B(_Rr(_1CQ));}),_1CS=[9,_1CR],_1CT=function(_1CU){return function(_1CV,_1CW,_1CX,_1CY,_1CZ){return new F(function(){return A(_1CY,[[1,new T(function(){var _1D0=[0,_1CS,[1,new T(function(){var _1D1=[0,[0,_1CU],_h],_1D2=B(_6v(_6O,_1D1));return E(_1D1);}),_h]],_1D3=B(_6v(_6O,_1D0));return E(_1D0);}),_h],_1CV,new T(function(){return [0,E(E(_1CV)[2]),_h];})]);});};},_1D4=function(_1D5,_1D6,_1D7,_1D8,_1D9){var _1Da=E(_1D5),_1Db=E(_1Da[2]);return new F(function(){return _lM(_1r,_za,_DD,_1Da[1],_1Db[1],_1Db[2],_1Db[3],_1Da[3],_1D6,_1D9);});},_1Dc=function(_1Dd,_1De,_1Df,_1Dg,_1Dh){return new F(function(){return _5L(_1D4,_DP,_1Dd,_1De,_1Df,_1Dg,_1Dh);});},_1Di=new T(function(){return B(unCStr("._-"));}),_1Dj=function(_1Dk){return new F(function(){return _kO(_7I,_1Dk,_1Di);});},_1Dl=function(_1Dm,_1Dn,_1Do,_1Dp,_1Dq){var _1Dr=E(_1Dm),_1Ds=E(_1Dr[2]);return new F(function(){return _lM(_1r,_za,_1Dj,_1Dr[1],_1Ds[1],_1Ds[2],_1Ds[3],_1Dr[3],_1Dn,_1Dq);});},_1Dt=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_1Dc,_1Dl,_B3,_B4,_B5,_B6,_zb);});},_1Du=function(_B3,_B4,_B5,_B6,_zb){return new F(function(){return _4g(_1fv,_1Dt,_B3,_B4,_B5,_B6,_zb);});},_1Dv=function(_1Dw,_1Dx,_1Dy,_1Dz,_1DA,_1DB,_1DC,_1DD){var _1DE=E(_1Dw);if(!_1DE[0]){return new F(function(){return A(_1DD,[new T(function(){var _1DF=B(_5q([0,_1Dx,E(_1Dy),E(_1Dz)],_7L,_1fi));return [0,E(_1DF[1]),_1DF[2]];})]);});}else{var _1DG=_1DE[2],_1DH=E(_1DE[1]),_1DI=_1DH[1],_1DJ=new T(function(){var _1DK=E(_1Dy),_1DL=function(_1DM,_1DN,_1DO){return new F(function(){return _lc(_1Du,_1DN,function(_1DP,_1DQ,_1DR){return new F(function(){return A(_1DB,[[1,_1DM,_1DP],_1DQ,new T(function(){var _1DS=E(_1DR),_1DT=B(_3P(_1DS[1],_1DS[2],E(_1DQ)[2],_h));return [0,E(_1DT[1]),_1DT[2]];})]);});},_1DC,function(_1DU,_1DV,_1DW){return new F(function(){return A(_1DB,[[1,_1DM,_1DU],_1DV,new T(function(){var _1DX=E(_1DO),_1DY=E(_1DW),_1DZ=B(_3P(_1DY[1],_1DY[2],E(_1DV)[2],_h)),_1E0=B(_3P(_1DX[1],_1DX[2],_1DZ[1],_1DZ[2]));return [0,E(_1E0[1]),_1E0[2]];})]);});},function(_1E1){return new F(function(){return A(_1DC,[new T(function(){return B(_4a(_1DO,_1E1));})]);});});});};switch(E(_1DI)){case 9:var _1E2=E(_1Dz)[1],_1E3=[0,_1Dx,E(_1DK),E([0,(_1E2+8|0)-B(_lF(_1E2-1|0,8))|0])],_1E4=B(_1DL(_1DH,[0,_1DG,E(_1E3),E(_1DA)],[0,E(_1E3),_h]));break;case 10:var _1E5=[0,_1Dx,E([0,_1DK[1]+1|0]),E(_kT)],_1E4=B(_1DL(_1DH,[0,_1DG,E(_1E5),E(_1DA)],[0,E(_1E5),_h]));break;default:var _1E6=[0,_1Dx,E(_1DK),E([0,E(_1Dz)[1]+1|0])],_1E4=B(_1DL(_1DH,[0,_1DG,E(_1E6),E(_1DA)],[0,E(_1E6),_h]));}var _1E7=_1E4;return _1E7;}),_1E8=new T(function(){return B(A(_1DD,[new T(function(){var _1E9=B(_5q([0,_1Dx,E(_1Dy),E(_1Dz)],[1,[0,E([1,_39,new T(function(){return B(_3b([1,_1DH,_h],_7J));})])],_h],_1fi));return [0,E(_1E9[1]),_1E9[2]];})]));});return _1DI<65?_1DI<97?E(_1E8):_1DI>122?E(_1E8):E(_1DJ):_1DI>90?_1DI<97?E(_1E8):_1DI>122?E(_1E8):E(_1DJ):E(_1DJ);}},_1Ea=function(_1Eb,_1Ec,_1Ed,_1Ee,_1Ef){var _1Eg=E(_1Eb),_1Eh=E(_1Eg[2]);return new F(function(){return _1Dv(_1Eg[1],_1Eh[1],_1Eh[2],_1Eh[3],_1Eg[3],_1Ec,_1Ed,_1Ef);});},_1Ei=function(_1Ej,_1Ek,_1El,_1Em,_1En,_1Eo){return new F(function(){return _4g(function(_1Ep,_1Eq,_1Er,_1Es,_1Et){return new F(function(){return _xx(_Lp,_Lp,_1Ej,_1Ep,_1Eq,_1Er,_1Es,_1Et);});},function(_1Eu,_1Ev,_1Ew,_1Ex,_1Ey){return new F(function(){return _xx(_SP,_SP,_1Ej,_1Eu,_1Ev,_1Ew,_1Ex,_1Ey);});},_1Ek,_1El,_1Em,_1En,_1Eo);});},_1Ez=function(_1EA,_1EB,_1EC,_1ED,_1EE){return new F(function(){return _1Ei(_1Ea,_1EA,function(_1EF,_1EG,_1EH){return new F(function(){return A(_1CT,[_1EF,_1EG,_1EB,_1EC,function(_1EI,_1EJ,_1EK){return new F(function(){return A(_1EB,[_1EI,_1EJ,new T(function(){return B(_4a(_1EH,_1EK));})]);});},function(_1EL){return new F(function(){return A(_1EC,[new T(function(){return B(_4a(_1EH,_1EL));})]);});}]);});},_1EC,function(_1EM,_1EN,_1EO){return new F(function(){return A(_1CT,[_1EM,_1EN,_1EB,_1EC,function(_1EP,_1EQ,_1ER){return new F(function(){return A(_1ED,[_1EP,_1EQ,new T(function(){return B(_4a(_1EO,_1ER));})]);});},function(_1ES){return new F(function(){return A(_1EE,[new T(function(){return B(_4a(_1EO,_1ES));})]);});}]);});},_1EE);});},_1ET=function(_1EU,_1EV,_1EW,_1EX,_1EY){return new F(function(){return A(_RQ,[_Lv,_1EU,function(_1EZ,_1F0,_1F1){return new F(function(){return _1Ez(_1F0,_1EV,_1EW,function(_1F2,_1F3,_1F4){return new F(function(){return A(_1EV,[_1F2,_1F3,new T(function(){return B(_4a(_1F1,_1F4));})]);});},function(_1F5){return new F(function(){return A(_1EW,[new T(function(){return B(_4a(_1F1,_1F5));})]);});});});},_1EW,function(_1F6,_1F7,_1F8){return new F(function(){return _1Ez(_1F7,_1EV,_1EW,function(_1F9,_1Fa,_1Fb){return new F(function(){return A(_1EX,[_1F9,_1Fa,new T(function(){return B(_4a(_1F8,_1Fb));})]);});},function(_1Fc){return new F(function(){return A(_1EY,[new T(function(){return B(_4a(_1F8,_1Fc));})]);});});});},_1EY]);});},_1Fd=function(_1Fe,_1Ff,_1Fg,_1Fh,_1Fi){var _1Fj=function(_1Fk,_1Fl){var _1Fm=new T(function(){var _1Fn=E(_1Fl),_1Fo=B(_3P(_1Fn[1],_1Fn[2],E(_1Fk)[2],_h));return [0,E(_1Fo[1]),_1Fo[2]];});return new F(function(){return _1ET(_1Fk,_1Ff,_1Fg,function(_1Fp,_1Fq,_1Fr){return new F(function(){return A(_1Ff,[_1Fp,_1Fq,new T(function(){return B(_4a(_1Fm,_1Fr));})]);});},function(_1Fs){return new F(function(){return A(_1Fg,[new T(function(){return B(_4a(_1Fm,_1Fs));})]);});});});},_1Ft=function(_XB,_XC,_XD){return new F(function(){return (function(_1Fu,_1Fv,_1Fw){return new F(function(){return _1Fj(_1Fv,_1Fw);});})(_XB,_XC,_XD);});};return new F(function(){return _v8(_RP,_1Fe,function(_1Fx,_1Fy,_1Fz){return new F(function(){return _1dp(_1CQ,_1Fy,_1Ft,_1Fi,function(_1FA,_1FB,_1FC){return new F(function(){return _1Fj(_1FB,new T(function(){var _1FD=E(_1Fz),_1FE=E(_1FC),_1FF=B(_3P(_1FD[1],_1FD[2],_1FE[1],_1FE[2]));return [0,E(_1FF[1]),_1FF[2]];}));});},function(_1FG){return new F(function(){return A(_1Fi,[new T(function(){return B(_4a(_1Fz,_1FG));})]);});});});},_1Fi,function(_1FH,_1FI,_1FJ){return new F(function(){return _1dp(_1CQ,_1FI,_1Ft,_1Fi,function(_1FK,_1FL,_1FM){var _1FN=new T(function(){var _1FO=E(_1FJ),_1FP=E(_1FM),_1FQ=B(_3P(_1FO[1],_1FO[2],_1FP[1],_1FP[2])),_1FR=B(_3P(_1FQ[1],_1FQ[2],E(_1FL)[2],_h));return [0,E(_1FR[1]),_1FR[2]];});return new F(function(){return _1ET(_1FL,_1Ff,_1Fg,function(_1FS,_1FT,_1FU){return new F(function(){return A(_1Fh,[_1FS,_1FT,new T(function(){return B(_4a(_1FN,_1FU));})]);});},function(_1FV){return new F(function(){return A(_1Fi,[new T(function(){return B(_4a(_1FN,_1FV));})]);});});});},function(_1FW){return new F(function(){return A(_1Fi,[new T(function(){return B(_4a(_1FJ,_1FW));})]);});});});},_1Fi);});},_1FX=new T(function(){return B(unCStr("no"));}),_1FY=[1,_1FX,_h],_1FZ=new T(function(){return B(unCStr("yes"));}),_1G0=[1,_1FZ,_1FY],_1G1=new T(function(){return B(_Lw(_1dp,_1G0));}),_1G2=new T(function(){return B(_1qN(_4g,_1G1));}),_1G3=new T(function(){return B(unCStr("standalone"));}),_1G4=new T(function(){return B(_Rr(_1G3));}),_1G5=[9,_1G4],_1G6=function(_1G7){return function(_1G8,_1G9,_1Ga,_1Gb,_1Gc){return new F(function(){return A(_1Gb,[[1,new T(function(){var _1Gd=[0,_1G5,[1,new T(function(){var _1Ge=[0,[0,_1G7],_h],_1Gf=B(_6v(_6O,_1Ge));return E(_1Ge);}),_h]],_1Gg=B(_6v(_6O,_1Gd));return E(_1Gd);}),_h],_1G8,new T(function(){return [0,E(E(_1G8)[2]),_h];})]);});};},_1Gh=function(_1Gi,_1Gj,_1Gk,_1Gl,_1Gm){return new F(function(){return _1Ei(_1G2,_1Gi,function(_1Gn,_1Go,_1Gp){return new F(function(){return A(_1G6,[_1Gn,_1Go,_1Gj,_1Gk,function(_1Gq,_1Gr,_1Gs){return new F(function(){return A(_1Gj,[_1Gq,_1Gr,new T(function(){return B(_4a(_1Gp,_1Gs));})]);});},function(_1Gt){return new F(function(){return A(_1Gk,[new T(function(){return B(_4a(_1Gp,_1Gt));})]);});}]);});},_1Gk,function(_1Gu,_1Gv,_1Gw){return new F(function(){return A(_1G6,[_1Gu,_1Gv,_1Gj,_1Gk,function(_1Gx,_1Gy,_1Gz){return new F(function(){return A(_1Gl,[_1Gx,_1Gy,new T(function(){return B(_4a(_1Gw,_1Gz));})]);});},function(_1GA){return new F(function(){return A(_1Gm,[new T(function(){return B(_4a(_1Gw,_1GA));})]);});}]);});},_1Gm);});},_1GB=function(_1GC,_1GD,_1GE,_1GF,_1GG){return new F(function(){return A(_RQ,[_Lv,_1GC,function(_1GH,_1GI,_1GJ){return new F(function(){return _1Gh(_1GI,_1GD,_1GE,function(_1GK,_1GL,_1GM){return new F(function(){return A(_1GD,[_1GK,_1GL,new T(function(){return B(_4a(_1GJ,_1GM));})]);});},function(_1GN){return new F(function(){return A(_1GE,[new T(function(){return B(_4a(_1GJ,_1GN));})]);});});});},_1GE,function(_1GO,_1GP,_1GQ){return new F(function(){return _1Gh(_1GP,_1GD,_1GE,function(_1GR,_1GS,_1GT){return new F(function(){return A(_1GF,[_1GR,_1GS,new T(function(){return B(_4a(_1GQ,_1GT));})]);});},function(_1GU){return new F(function(){return A(_1GG,[new T(function(){return B(_4a(_1GQ,_1GU));})]);});});});},_1GG]);});},_1GV=function(_1GW,_1GX,_1GY,_1GZ,_1H0){var _1H1=function(_1H2,_1H3){var _1H4=new T(function(){var _1H5=E(_1H3),_1H6=B(_3P(_1H5[1],_1H5[2],E(_1H2)[2],_h));return [0,E(_1H6[1]),_1H6[2]];});return new F(function(){return _1GB(_1H2,_1GX,_1GY,function(_1H7,_1H8,_1H9){return new F(function(){return A(_1GX,[_1H7,_1H8,new T(function(){return B(_4a(_1H4,_1H9));})]);});},function(_1Ha){return new F(function(){return A(_1GY,[new T(function(){return B(_4a(_1H4,_1Ha));})]);});});});},_1Hb=function(_XB,_XC,_XD){return new F(function(){return (function(_1Hc,_1Hd,_1He){return new F(function(){return _1H1(_1Hd,_1He);});})(_XB,_XC,_XD);});};return new F(function(){return _v8(_RP,_1GW,function(_1Hf,_1Hg,_1Hh){return new F(function(){return _1dp(_1G3,_1Hg,_1Hb,_1H0,function(_1Hi,_1Hj,_1Hk){return new F(function(){return _1H1(_1Hj,new T(function(){var _1Hl=E(_1Hh),_1Hm=E(_1Hk),_1Hn=B(_3P(_1Hl[1],_1Hl[2],_1Hm[1],_1Hm[2]));return [0,E(_1Hn[1]),_1Hn[2]];}));});},function(_1Ho){return new F(function(){return A(_1H0,[new T(function(){return B(_4a(_1Hh,_1Ho));})]);});});});},_1H0,function(_1Hp,_1Hq,_1Hr){return new F(function(){return _1dp(_1G3,_1Hq,_1Hb,_1H0,function(_1Hs,_1Ht,_1Hu){var _1Hv=new T(function(){var _1Hw=E(_1Hr),_1Hx=E(_1Hu),_1Hy=B(_3P(_1Hw[1],_1Hw[2],_1Hx[1],_1Hx[2])),_1Hz=B(_3P(_1Hy[1],_1Hy[2],E(_1Ht)[2],_h));return [0,E(_1Hz[1]),_1Hz[2]];});return new F(function(){return _1GB(_1Ht,_1GX,_1GY,function(_1HA,_1HB,_1HC){return new F(function(){return A(_1GZ,[_1HA,_1HB,new T(function(){return B(_4a(_1Hv,_1HC));})]);});},function(_1HD){return new F(function(){return A(_1H0,[new T(function(){return B(_4a(_1Hv,_1HD));})]);});});});},function(_1HE){return new F(function(){return A(_1H0,[new T(function(){return B(_4a(_1Hr,_1HE));})]);});});});},_1H0);});},_1HF=function(_1HG,_1HH,_1HI,_1HJ,_1HK,_1HL){var _1HM=function(_1HN,_1HO,_1HP,_1HQ,_1HR,_1HS){var _1HT=function(_1HU){var _1HV=new T(function(){return B(_0(_1HG,new T(function(){return B(_0(_1HN,_1HU));})));});return function(_1HW,_1HX,_1HY,_1HZ,_1I0){return new F(function(){return _7g(_RP,_1HW,function(_1I1,_1I2,_1I3){return new F(function(){return A(_1HX,[_1HV,_1I2,new T(function(){var _1I4=E(_1I3),_1I5=B(_3P(_1I4[1],_1I4[2],E(_1I2)[2],_h));return [0,E(_1I5[1]),_1I5[2]];})]);});},_1HY,function(_1I6,_1I7,_1I8){return new F(function(){return A(_1HZ,[_1HV,_1I7,new T(function(){var _1I9=E(_1I8),_1Ia=B(_3P(_1I9[1],_1I9[2],E(_1I7)[2],_h));return [0,E(_1Ia[1]),_1Ia[2]];})]);});});});};},_1Ib=function(_1Ic,_1Id,_1Ie){return new F(function(){return A(_1HT,[_1Ic,_1Id,_1HP,_1HQ,function(_1If,_1Ig,_1Ih){return new F(function(){return A(_1HR,[_1If,_1Ig,new T(function(){return B(_4a(_1Ie,_1Ih));})]);});},function(_1Ii){return new F(function(){return A(_1HS,[new T(function(){return B(_4a(_1Ie,_1Ii));})]);});}]);});};return new F(function(){return _1GV(_1HO,function(_1Ij,_1Ik,_1Il){return new F(function(){return A(_1HT,[_1Ij,_1Ik,_1HP,_1HQ,function(_1Im,_1In,_1Io){return new F(function(){return A(_1HP,[_1Im,_1In,new T(function(){return B(_4a(_1Il,_1Io));})]);});},function(_1Ip){return new F(function(){return A(_1HQ,[new T(function(){return B(_4a(_1Il,_1Ip));})]);});}]);});},_1HQ,_1Ib,function(_1Iq){return new F(function(){return _1Ib(_h,_1HO,new T(function(){var _1Ir=E(_1Iq),_1Is=B(_3P(_1Ir[1],_1Ir[2],E(_1HO)[2],_h));return [0,E(_1Is[1]),_1Is[2]];}));});});});},_1It=function(_1Iu,_1Iv,_1Iw){return new F(function(){return _1HM(_1Iu,_1Iv,_1HI,_1HJ,function(_1Ix,_1Iy,_1Iz){return new F(function(){return A(_1HK,[_1Ix,_1Iy,new T(function(){return B(_4a(_1Iw,_1Iz));})]);});},function(_1IA){return new F(function(){return A(_1HL,[new T(function(){return B(_4a(_1Iw,_1IA));})]);});});});};return new F(function(){return _1Fd(_1HH,function(_1IB,_1IC,_1ID){return new F(function(){return _1HM(_1IB,_1IC,_1HI,_1HJ,function(_1IE,_1IF,_1IG){return new F(function(){return A(_1HI,[_1IE,_1IF,new T(function(){return B(_4a(_1ID,_1IG));})]);});},function(_1IH){return new F(function(){return A(_1HJ,[new T(function(){return B(_4a(_1ID,_1IH));})]);});});});},_1HJ,_1It,function(_1II){return new F(function(){return _1It(_h,_1HH,new T(function(){var _1IJ=E(_1II),_1IK=B(_3P(_1IJ[1],_1IJ[2],E(_1HH)[2],_h));return [0,E(_1IK[1]),_1IK[2]];}));});});});},_1IL=new T(function(){return B(unCStr("version info (with quoted version number)"));}),_1IM=[1,_1IL,_h],_1IN=new T(function(){return B(unCStr("version"));}),_1IO=new T(function(){return B(_Rr(_1IN));}),_1IP=[9,_1IO],_1IQ=function(_1IR){return function(_1IS,_1IT,_1IU,_1IV,_1IW){return new F(function(){return A(_1IV,[[1,new T(function(){var _1IX=[0,_1IP,[1,new T(function(){var _1IY=[0,[0,_1IR],_h],_1IZ=B(_6v(_6O,_1IY));return E(_1IY);}),_h]],_1J0=B(_6v(_6O,_1IX));return E(_1IX);}),_h],_1IS,new T(function(){return [0,E(E(_1IS)[2]),_h];})]);});};},_1J1=function(_1J2,_1J3,_1J4,_1J5,_1J6){return new F(function(){return _yy(_uf,_1J2,_1J3,_1J4,_1J5,_1J6);});},_1J7=function(_1J8,_1J9,_1Ja,_1Jb,_1Jc){return new F(function(){return _1Ei(_1J1,_1J8,function(_1Jd,_1Je,_1Jf){return new F(function(){return A(_1IQ,[_1Jd,_1Je,_1J9,_1Ja,function(_1Jg,_1Jh,_1Ji){return new F(function(){return A(_1J9,[_1Jg,_1Jh,new T(function(){return B(_4a(_1Jf,_1Ji));})]);});},function(_1Jj){return new F(function(){return A(_1Ja,[new T(function(){return B(_4a(_1Jf,_1Jj));})]);});}]);});},_1Ja,function(_1Jk,_1Jl,_1Jm){return new F(function(){return A(_1IQ,[_1Jk,_1Jl,_1J9,_1Ja,function(_1Jn,_1Jo,_1Jp){return new F(function(){return A(_1Jb,[_1Jn,_1Jo,new T(function(){return B(_4a(_1Jm,_1Jp));})]);});},function(_1Jq){return new F(function(){return A(_1Jc,[new T(function(){return B(_4a(_1Jm,_1Jq));})]);});}]);});},_1Jc);});},_1Jr=function(_1Js,_1Jt,_1Ju,_1Jv,_1Jw){return new F(function(){return A(_RQ,[_Lv,_1Js,function(_1Jx,_1Jy,_1Jz){return new F(function(){return _1J7(_1Jy,_1Jt,_1Ju,function(_1JA,_1JB,_1JC){return new F(function(){return A(_1Jt,[_1JA,_1JB,new T(function(){return B(_4a(_1Jz,_1JC));})]);});},function(_1JD){return new F(function(){return A(_1Ju,[new T(function(){return B(_4a(_1Jz,_1JD));})]);});});});},_1Ju,function(_1JE,_1JF,_1JG){return new F(function(){return _1J7(_1JF,_1Jt,_1Ju,function(_1JH,_1JI,_1JJ){return new F(function(){return A(_1Jv,[_1JH,_1JI,new T(function(){return B(_4a(_1JG,_1JJ));})]);});},function(_1JK){return new F(function(){return A(_1Jw,[new T(function(){return B(_4a(_1JG,_1JK));})]);});});});},_1Jw]);});},_1JL=function(_1JM,_1JN,_1JO,_1JP,_1JQ){var _1JR=function(_1JS,_1JT){var _1JU=new T(function(){var _1JV=E(_1JT),_1JW=B(_3P(_1JV[1],_1JV[2],E(_1JS)[2],_h));return [0,E(_1JW[1]),_1JW[2]];});return new F(function(){return _1Jr(_1JS,_1JN,_1JO,function(_1JX,_1JY,_1JZ){return new F(function(){return A(_1JN,[_1JX,_1JY,new T(function(){return B(_4a(_1JU,_1JZ));})]);});},function(_1K0){return new F(function(){return A(_1JO,[new T(function(){return B(_4a(_1JU,_1K0));})]);});});});},_1K1=function(_XB,_XC,_XD){return new F(function(){return (function(_1K2,_1K3,_1K4){return new F(function(){return _1JR(_1K3,_1K4);});})(_XB,_XC,_XD);});};return new F(function(){return _v8(_RP,_1JM,function(_1K5,_1K6,_1K7){return new F(function(){return _1dp(_1IN,_1K6,_1K1,_1JQ,function(_1K8,_1K9,_1Ka){return new F(function(){return _1JR(_1K9,new T(function(){var _1Kb=E(_1K7),_1Kc=E(_1Ka),_1Kd=B(_3P(_1Kb[1],_1Kb[2],_1Kc[1],_1Kc[2]));return [0,E(_1Kd[1]),_1Kd[2]];}));});},function(_1Ke){return new F(function(){return A(_1JQ,[new T(function(){return B(_4a(_1K7,_1Ke));})]);});});});},_1JQ,function(_1Kf,_1Kg,_1Kh){return new F(function(){return _1dp(_1IN,_1Kg,_1K1,_1JQ,function(_1Ki,_1Kj,_1Kk){var _1Kl=new T(function(){var _1Km=E(_1Kh),_1Kn=E(_1Kk),_1Ko=B(_3P(_1Km[1],_1Km[2],_1Kn[1],_1Kn[2])),_1Kp=B(_3P(_1Ko[1],_1Ko[2],E(_1Kj)[2],_h));return [0,E(_1Kp[1]),_1Kp[2]];});return new F(function(){return _1Jr(_1Kj,_1JN,_1JO,function(_1Kq,_1Kr,_1Ks){return new F(function(){return A(_1JP,[_1Kq,_1Kr,new T(function(){return B(_4a(_1Kl,_1Ks));})]);});},function(_1Kt){return new F(function(){return A(_1JQ,[new T(function(){return B(_4a(_1Kl,_1Kt));})]);});});});},function(_1Ku){return new F(function(){return A(_1JQ,[new T(function(){return B(_4a(_1Kh,_1Ku));})]);});});});},_1JQ);});},_1Kv=function(_1Kw,_1Kx,_1Ky,_1Kz,_1KA){return new F(function(){return _1JL(_1Kw,function(_1KB,_1KC,_1KD){return new F(function(){return _1HF(_1KB,_1KC,_1Kx,_1Ky,function(_1KE,_1KF,_1KG){return new F(function(){return A(_1Kx,[_1KE,_1KF,new T(function(){return B(_4a(_1KD,_1KG));})]);});},function(_1KH){return new F(function(){return A(_1Ky,[new T(function(){return B(_4a(_1KD,_1KH));})]);});});});},_1Ky,function(_1KI,_1KJ,_1KK){var _1KL=new T(function(){var _1KM=E(_1KK),_1KN=E(_1KM[2]);if(!_1KN[0]){var _1KO=E(_1KM);}else{var _1KP=B(_5q(_1KM[1],_1KN,_1IM)),_1KO=[0,E(_1KP[1]),_1KP[2]];}var _1KQ=_1KO;return _1KQ;});return new F(function(){return _1HF(_1KI,_1KJ,_1Kx,_1Ky,function(_1KR,_1KS,_1KT){return new F(function(){return A(_1Kz,[_1KR,_1KS,new T(function(){return B(_4a(_1KL,_1KT));})]);});},function(_1KU){return new F(function(){return A(_1KA,[new T(function(){return B(_4a(_1KL,_1KU));})]);});});});},function(_1KV){return new F(function(){return A(_1KA,[new T(function(){var _1KW=E(_1KV),_1KX=B(_5q(_1KW[1],_1KW[2],_1IM));return [0,E(_1KX[1]),_1KX[2]];})]);});});});},_1KY=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _Gz(_VB,_10O,_Xz,_XA,_XB,_XC,_XD);});},_1KZ=new T(function(){return B(unCStr("<?xml"));}),_1L0=function(_1L1,_1L2,_1L3,_1L4,_1L5){return new F(function(){return _Gz(_VB,_1KZ,_1L1,_1L2,_1L5,_1L4,_1L5);});},_1L6=new T(function(){return B(unCStr("xml declaration"));}),_1L7=[1,_1L6,_h],_1L8=new T(function(){return B(_Rr(_wR));}),_1L9=function(_1La){return function(_1Lb,_1Lc,_1Ld,_1Le,_1Lf){return new F(function(){return A(_1Le,[[1,new T(function(){var _1Lg=[0,[6,_1L8,_1La],_h],_1Lh=B(_6v(_6O,_1Lg));return E(_1Lg);}),_h],_1Lb,new T(function(){return [0,E(E(_1Lb)[2]),_h];})]);});};},_1Li=function(_1Lj,_1Lk,_1Ll,_1Lm,_1Ln){return new F(function(){return _xx(_1L0,_1KY,_1Kv,_1Lj,function(_1Lo,_1Lp,_1Lq){return new F(function(){return A(_1L9,[_1Lo,_1Lp,_1Lk,_1Ll,function(_1Lr,_1Ls,_1Lt){return new F(function(){return A(_1Lk,[_1Lr,_1Ls,new T(function(){return B(_4a(_1Lq,_1Lt));})]);});},function(_1Lu){return new F(function(){return A(_1Ll,[new T(function(){return B(_4a(_1Lq,_1Lu));})]);});}]);});},_1Ll,function(_1Lv,_1Lw,_1Lx){var _1Ly=new T(function(){var _1Lz=E(_1Lx),_1LA=E(_1Lz[2]);if(!_1LA[0]){var _1LB=E(_1Lz);}else{var _1LC=B(_5q(_1Lz[1],_1LA,_1L7)),_1LB=[0,E(_1LC[1]),_1LC[2]];}var _1LD=_1LB;return _1LD;});return new F(function(){return A(_1L9,[_1Lv,_1Lw,_1Lk,_1Ll,function(_1LE,_1LF,_1LG){return new F(function(){return A(_1Lm,[_1LE,_1LF,new T(function(){return B(_4a(_1Ly,_1LG));})]);});},function(_1LH){return new F(function(){return A(_1Ln,[new T(function(){return B(_4a(_1Ly,_1LH));})]);});}]);});},function(_1LI){return new F(function(){return A(_1Ln,[new T(function(){var _1LJ=E(_1LI),_1LK=B(_5q(_1LJ[1],_1LJ[2],_1L7));return [0,E(_1LK[1]),_1LK[2]];})]);});});});},_1LL=function(_1LM,_1LN,_1LO,_1LP,_1LQ){var _1LR=function(_1LS,_1LT,_1LU){return new F(function(){return _1BP(_1LS,_1LT,_1LN,_1LO,function(_1LV,_1LW,_1LX){return new F(function(){return A(_1LP,[_1LV,_1LW,new T(function(){return B(_4a(_1LU,_1LX));})]);});},function(_1LY){return new F(function(){return A(_1LQ,[new T(function(){return B(_4a(_1LU,_1LY));})]);});});});};return new F(function(){return _1Li(_1LM,function(_1LZ,_1M0,_1M1){return new F(function(){return _1BP(_1LZ,_1M0,_1LN,_1LO,function(_1M2,_1M3,_1M4){return new F(function(){return A(_1LN,[_1M2,_1M3,new T(function(){return B(_4a(_1M1,_1M4));})]);});},function(_1M5){return new F(function(){return A(_1LO,[new T(function(){return B(_4a(_1M1,_1M5));})]);});});});},_1LO,_1LR,function(_1M6){return new F(function(){return _1LR(_h,_1LM,new T(function(){var _1M7=E(_1M6),_1M8=B(_3P(_1M7[1],_1M7[2],E(_1LM)[2],_h));return [0,E(_1M8[1]),_1M8[2]];}));});});});},_1M9=function(_1Ma,_1Mb,_1Mc,_1Md){return new F(function(){return _1LL(_1Ma,function(_1Me,_1Mf,_1Mg){return new F(function(){return _1aT(_1Me,_1Mf,_1Mb,_1Mc,function(_1Mh){return new F(function(){return A(_1Mc,[new T(function(){return B(_4a(_1Mg,_1Mh));})]);});});});},_1Mc,function(_1Mi,_1Mj,_1Mk){return new F(function(){return _1aT(_1Mi,_1Mj,_1Mb,_1Mc,function(_1Ml){return new F(function(){return A(_1Md,[new T(function(){return B(_4a(_1Mk,_1Ml));})]);});});});},_1Md);});},_1Mm=function(_1Mn,_1Mo,_1Mp,_1Mq,_1Mr){return new F(function(){return _1M9(_1Mn,_1Mo,_1Mp,_1Mr);});},_1Ms=function(_Xz,_XA,_XB,_XC,_XD){return new F(function(){return _1Mm(_Xz,_XA,_XB,_XC,_XD);});},_1Mt=function(_1Mu,_1Mv,_1Mw){return new F(function(){return A(_1Mu,[[1,_w2,new T(function(){return B(A(_1Mv,[_1Mw]));})]]);});},_1Mx=[1,_9,_h],_1My=function(_){var _=0,_1Mz=jsMkStdout(),_1MA=_1Mz;return [0,_1MA];},_1MB=new T(function(){return B(_t(_1My));}),_1MC=function(_1MD,_){while(1){var _1ME=(function(_1MF,_){var _1MG=E(_1MF);if(!_1MG[0]){return _G;}else{var _1MH=E(_1MG[1]),_1MI=B(_N(_1MB,[1,_a,new T(function(){return B(A(_1qN,[_1Mt,[1,function(_1MJ){return new F(function(){return _3h(_1MH[1],_1MJ);});},[1,function(_1MJ){return new F(function(){return _3h(_1MH[2],_1MJ);});},_h]],_1Mx]));})],_)),_1MK=_1MI;_1MD=_1MG[2];return null;}})(_1MD,_);if(_1ME!=null){return _1ME;}}},_1ML=new T(function(){return B(unCStr("Failed API request"));}),_1MM=new T(function(){return B(err(_1ML));}),_1MN=function(_1MO){return E(E(_1MO)[6]);},_1MP=function(_1MQ,_1MR,_1MS){var _1MT=B(A(_1MR,[_1MS]));if(!_1MT[0]){var _1MU=function(_1MV){var _1MW=E(_1MV);if(!_1MW[0]){return [0];}else{return new F(function(){return _0(B(_1MP(_1MQ,_1MR,_1MW[1])),new T(function(){return B(_1MU(_1MW[2]));}));});}};return new F(function(){return _1MU(B(A(_1MN,[_1MQ,_1MS])));});}else{return E(_1MT);}},_1MX=function(_1MY,_1MZ){while(1){var _1N0=E(_1MY);if(!_1N0[0]){return E(_1MZ);}else{_1MY=_1N0[2];var _1N1=_1MZ+E(_1N0[1])[1]|0;_1MZ=_1N1;continue;}}},_1N2=function(_1N3,_1N4){return [0,1+B(_1MX(_1N4,0))|0];},_1N5=function(_1N6,_1N7){return new F(function(){return A(_1N6,[new T(function(){return E(E(_1N7)[1]);}),new T(function(){return B(_Lw(function(_6L){return new F(function(){return _1N5(_1N6,_6L);});},E(_1N7)[2]));})]);});},_1N8=function(_6L){return new F(function(){return _1N5(_1N2,_6L);});},_1N9=function(_1Na,_1Nb){return [0,new T(function(){return E(E(_1Nb)[1]);}),new T(function(){return B(A(_1Na,[new T(function(){return E(E(_1Nb)[2]);})]));})];},_1Nc=function(_1Nd,_1Ne){return [0,new T(function(){return B(A(_1Nd,[new T(function(){return E(E(_1Ne)[1]);})]));}),new T(function(){return E(E(_1Ne)[2]);})];},_1Nf=function(_1Ng,_1Nh){while(1){var _1Ni=E(_1Nh);if(!_1Ni[0]){return E(_1Ng);}else{var _1Nj=_1Ni[2],_1Nk=E(_1Ni[1])[1];if(_1Ng>_1Nk){_1Nh=_1Nj;continue;}else{_1Ng=_1Nk;_1Nh=_1Nj;continue;}}}},_1Nl=function(_1Nm,_1Nn){return [0,1+B(_1Nf(0,_1Nn))|0];},_1No=function(_6L){return new F(function(){return _1N5(_1Nl,_6L);});},_1Np=new T(function(){return B(unCStr("+---"));}),_1Nq=[0,10],_1Nr=function(_1Ns){return E(E(_1Ns)[5]);},_1Nt=function(_1Nu){return E(_1Nu);},_1Nv=new T(function(){return B(unCStr("    "));}),_1Nw=new T(function(){return B(unCStr("|\n"));}),_1Nx=new T(function(){return B(unCStr("|   "));}),_1Ny=function(_1Nz,_1NA,_1NB,_1NC,_1ND){var _1NE=function(_1NF,_1NG){var _1NH=E(_1NF);if(!_1NH[0]){return E(_1NG);}else{var _1NI=_1NH[2],_1NJ=E(_1NH[1]);return E(_1NJ[1])==10?[1,_1Nq,new T(function(){return B(A(_1NC,[new T(function(){return B(_1NE(_1NI,_1NG));})]));})]:[1,_1NJ,new T(function(){return B(_1NE(_1NI,_1NG));})];}};return function(_1NK){return new F(function(){return A(_1NB,[new T(function(){return B(_1NE(new T(function(){return B(A(_1NA,[new T(function(){return B(A(_1Nr,[_1Nz,_1ND]));})]));}),[1,_1Nq,new T(function(){return B(A(new T(function(){var _1NL=function(_1NM,_1NN){var _1NO=E(_1NN);if(!_1NO[0]){return E(_1Nt);}else{var _1NP=_1NO[1],_1NQ=E(_1NO[2]);return _1NQ[0]==0?function(_1NR){return new F(function(){return A(_1NM,[new T(function(){return B(_0(_1Nw,new T(function(){return B(A(new T(function(){return B(_1Ny(_1Nz,_1NA,function(_1NS){return new F(function(){return A(_1NM,[new T(function(){return B(_0(_1Np,_1NS));})]);});},function(_1NT){return new F(function(){return A(_1NM,[new T(function(){return B(_0(_1Nv,_1NT));})]);});},_1NP));}),[_1NR]));})));})]);});}:function(_1NU){return new F(function(){return A(_1NM,[new T(function(){return B(_0(_1Nw,new T(function(){return B(A(new T(function(){return B(_1Ny(_1Nz,_1NA,function(_1NV){return new F(function(){return A(_1NM,[new T(function(){return B(_0(_1Np,_1NV));})]);});},function(_1NW){return new F(function(){return A(_1NM,[new T(function(){return B(_0(_1Nx,_1NW));})]);});},_1NP));}),[new T(function(){return B(A(new T(function(){return B(_1NL(_1NM,_1NQ));}),[_1NU]));})]));})));})]);});};}};return B(_1NL(_1NC,B(A(_1MN,[_1Nz,_1ND]))));}),[_1NK]));})]));})]);});};},_1NX=new T(function(){return B(unCStr("   "));}),_1NY=function(_6L){return new F(function(){return _0(_1NX,_6L);});},_1NZ=new T(function(){return B(unCStr("---"));}),_1O0=function(_6L){return new F(function(){return _0(_1NZ,_6L);});},_1O1=function(_1O2,_1O3){return new F(function(){return A(_1Ny,[_1O4,_1O2,_1O0,_1NY,_1O3,_h]);});},_1O5=function(_1O6){return E(E(_1O6)[2]);},_1O7=function(_1O8){return E(E(_1O8)[1]);},_1O9=function(_1Oa){return E(E(_1Oa)[2])[0]==0?false:true;},_1Ob=function(_1Oc){return E(E(_1Oc)[2])[0]==0?true:false;},_1Od=function(_1Oe){return [0,_1Oe,_h];},_1Of=function(_1Og,_6L){return [0,_1Og,_6L];},_1Oh=function(_1Oi){var _1Oj=E(_1Oi);if(!_1Oj[0]){return [0];}else{return new F(function(){return _0(_1Oj[1],new T(function(){return B(_1Oh(_1Oj[2]));}));});}},_1Ok=function(_1Ol,_1Om){return [1,_1Ol,new T(function(){return B(_1Oh(_1Om));})];},_1On=function(_6L){return new F(function(){return _1N5(_1Ok,_6L);});},_1Oo=function(_1Op,_1Oq){return [0,new T(function(){return E(E(_1Oq)[1]);}),_1Op];},_1Or=function(_1Os,_1Ot){return [0,_1Os,new T(function(){return E(E(_1Ot)[2]);})];},_1O4=new T(function(){return [0,_1Of,_1Od,_1Ob,_1O9,_1O7,_1O5,_1Nc,_1N9,_1Or,_1Oo,_1N5,_1On,_1No,_1N8,_1O1];}),_1Ou=function(_1Ov,_1Ow){var _1Ox=function(_1Oy,_1Oz){return !B(_10J(_1Oy,_1Ov))?B(_1OA(_1Oz)):[1,_1Oy,new T(function(){return B(_1OA(_1Oz));})];},_1OA=function(_1OB){while(1){var _1OC=(function(_1OD){var _1OE=E(_1OD);if(!_1OE[0]){return [0];}else{var _1OF=_1OE[1],_1OG=_1OE[2];if(!B(_10J(_1OF,_1Ov))){_1OB=_1OG;return null;}else{return [1,_1OF,new T(function(){return B(_1OA(_1OG));})];}}})(_1OB);if(_1OC!=null){return _1OC;}}},_1OH=function(_1OI){var _1OJ=E(_1OI);if(!_1OJ[0]){return [0];}else{var _1OK=_1OJ[2],_1OL=E(_1OJ[1]),_1OM=function(_1ON){return B(_1Ox(new T(function(){var _1OO=E(_1ON),_1OP=E(_1OO[1])[2],_1OQ=E(_1OO[2]);return E(_1OQ[1])[1]!=E(E(_xf)[1])[1]?B(_0(_1OQ[2],[1,_7d,_1OP])):E(_1OP);}),_h))[0]==0?B(_1OH(_1OK)):[1,_1OL,new T(function(){return B(_1OH(_1OK));})];},_1OR=E(_1OL[1]);switch(_1OR[0]){case 6:return new F(function(){return _1OM(_1OR[1]);});break;case 7:return new F(function(){return _1OM(_1OR[1]);});break;case 9:return new F(function(){return _1OM(_1OR[1]);});break;default:var _1OS=function(_1OT){while(1){var _1OU=(function(_1OV){var _1OW=E(_1OV);if(!_1OW[0]){return [0];}else{var _1OX=_1OW[1],_1OY=_1OW[2];if(!B(_10J(_1OX,_1Ov))){_1OT=_1OY;return null;}else{return [1,_1OX,new T(function(){return B(_1OS(_1OY));})];}}})(_1OT);if(_1OU!=null){return _1OU;}}},_1OZ=function(_1P0){while(1){var _1P1=(function(_1P2){var _1P3=E(_1P2);if(!_1P3[0]){return [0];}else{var _1P4=_1P3[2],_1P5=E(_1P3[1]),_1P6=function(_1P7){return B((function(_1P8,_1P9){return !B(_10J(_1P8,_1Ov))?B(_1OS(_1P9)):[1,_1P8,new T(function(){return B(_1OS(_1P9));})];})(new T(function(){var _1Pa=E(_1P7),_1Pb=E(_1Pa[1])[2],_1Pc=E(_1Pa[2]);return E(_1Pc[1])[1]!=E(E(_xf)[1])[1]?B(_0(_1Pc[2],[1,_7d,_1Pb])):E(_1Pb);}),_h))[0]==0?B(_1OZ(_1P4)):[1,_1P5,new T(function(){return B(_1OZ(_1P4));})];},_1Pd=E(_1P5[1]);switch(_1Pd[0]){case 6:return new F(function(){return _1P6(_1Pd[1]);});break;case 7:return new F(function(){return _1P6(_1Pd[1]);});break;case 9:return new F(function(){return _1P6(_1Pd[1]);});break;default:_1P0=_1P4;return null;}}})(_1P0);if(_1P1!=null){return _1P1;}}};return new F(function(){return _1OZ(_1OK);});}}};return new F(function(){return _1MP(_1O4,function(_1Pe){var _1Pf=E(_1Pe),_1Pg=E(_1Pf[1]);return _1Pg[0]==7?B((function(_1Ph,_1Pi,_1Pj,_1Pk){return B(_1Ox(new T(function(){var _1Pl=E(_1Ph),_1Pm=E(_1Pl[1])[2],_1Pn=E(_1Pl[2]);return E(_1Pn[1])[1]!=E(E(_xf)[1])[1]?B(_0(_1Pn[2],[1,_7d,_1Pm])):E(_1Pm);}),_h))[0]==0?B(_1OH(_1Pk)):[1,[0,[7,_1Ph,_1Pi],_1Pj],new T(function(){return B(_1OH(_1Pk));})];})(_1Pg[1],_1Pg[2],_1Pf[2],_h)):[0];},_1Ow);});},_1Po=new T(function(){return B(unCStr("station"));}),_1Pp=function(_1Pq){var _1Pr=E(_1Pq);return _1Pr[0]==0?[0]:[1,_1Pr[1],new T(function(){return B(_1Pp(_1Pr[2]));})];},_1Ps=function(_1Pt){var _1Pu=E(_1Pt);if(!_1Pu[0]){return [0];}else{return new F(function(){return _0(B(_1Pp([1,_1Pu[1],_h])),new T(function(){return B(_1Ps(_1Pu[2]));}));});}},_1Pv=function(_1Pw,_1Px){var _1Py=function(_1Pz){var _1PA=E(_1Pz);return _1PA[0]==0?[0]:[1,[0,_1PA[1],new T(function(){return E(E(_1Px)[2]);})],new T(function(){return B(_1Py(_1PA[2]));})];};return new F(function(){return _1Py(B(A(_1Pw,[new T(function(){return E(E(_1Px)[1]);})])));});},_1PB=new T(function(){return B(unCStr("name"));}),_1PC=function(_1PD){while(1){var _1PE=(function(_1PF){var _1PG=E(_1PF);if(!_1PG[0]){return [0];}else{var _1PH=_1PG[2],_1PI=E(E(_1PG[1])[1]);switch(_1PI[0]){case 0:return [1,_1PI[1],new T(function(){return B(_1PC(_1PH));})];case 1:return [1,new T(function(){return B(_Kg(_1PI[1]));}),new T(function(){return B(_1PC(_1PH));})];default:_1PD=_1PH;return null;}}})(_1PD);if(_1PE!=null){return _1PE;}}},_1PJ=function(_1PK){var _1PL=E(_1PK);if(!_1PL[0]){return [0];}else{return new F(function(){return _0(B(_1PC(E(_1PL[1])[2])),new T(function(){return B(_1PJ(_1PL[2]));}));});}},_1PM=function(_1PN){var _1PO=E(_1PN);if(!_1PO[0]){return [0];}else{return new F(function(){return _0(B(_1PJ(B(_1Ou(_1PB,_1PO[1])))),new T(function(){return B(_1PM(_1PO[2]));}));});}},_1PP=function(_1PQ){return new F(function(){return _1PM([1,_1PQ,_h]);});},_1PR=function(_1PS){var _1PT=E(_1PS);return _1PT[0]==0?[0]:[1,new T(function(){var _1PU=E(_1PT[1]);return [0,_1PU[2],_1PU[1]];}),new T(function(){return B(_1PR(_1PT[2]));})];},_1PV=function(_1PW){var _1PX=E(_1PW);if(!_1PX[0]){return [0];}else{return new F(function(){return _0(B(_1PR(B(_1Pv(_1PP,_1PX[1])))),new T(function(){return B(_1PV(_1PX[2]));}));});}},_1PY=function(_1PZ){var _1Q0=E(_1PZ);if(!_1Q0[0]){return [0];}else{return new F(function(){return _0(B(_1Ps(B(_1PV([1,_1Q0[1],_h])))),new T(function(){return B(_1PY(_1Q0[2]));}));});}},_1Q1=new T(function(){return B(unCStr("abbr"));}),_1Q2=function(_1Q3){var _1Q4=E(_1Q3);if(!_1Q4[0]){return [0];}else{return new F(function(){return _0(B(_1PC(E(_1Q4[1])[2])),new T(function(){return B(_1Q2(_1Q4[2]));}));});}},_1Q5=function(_1Q6){var _1Q7=E(_1Q6);if(!_1Q7[0]){return [0];}else{return new F(function(){return _0(B(_1Q2(B(_1Ou(_1Q1,_1Q7[1])))),new T(function(){return B(_1Q5(_1Q7[2]));}));});}},_1Q8=function(_1Q9){return new F(function(){return _1Q5([1,_1Q9,_h]);});},_1Qa=function(_1Qb){var _1Qc=E(_1Qb);return _1Qc[0]==0?[0]:[1,new T(function(){var _1Qd=E(_1Qc[1]);return [0,_1Qd[2],_1Qd[1]];}),new T(function(){return B(_1Qa(_1Qc[2]));})];},_1Qe=function(_1Qf){var _1Qg=E(_1Qf);if(!_1Qg[0]){return [0];}else{return new F(function(){return _0(B(_1Qa(B(_1Pv(_1Q8,_1Qg[1])))),new T(function(){return B(_1Qe(_1Qg[2]));}));});}},_1Qh=function(_1Qi){var _1Qj=E(_1Qi);if(!_1Qj[0]){return [0];}else{var _1Qk=_1Qj[1];return new F(function(){return _0(B(_1PY(B(_1Qe([1,[0,_1Qk,_1Qk],_h])))),new T(function(){return B(_1Qh(_1Qj[2]));}));});}},_1Ql=function(_1Qm){var _1Qn=E(_1Qm);if(!_1Qn[0]){return [0];}else{return new F(function(){return _0(B(_1Qh([1,_1Qn[1],_h])),new T(function(){return B(_1Ql(_1Qn[2]));}));});}},_1Qo=function(_1Qp){var _1Qq=E(_1Qp);if(!_1Qq[0]){return [0];}else{return new F(function(){return _0(B(_1Ql(B(_1Ou(_1Po,_1Qq[1])))),new T(function(){return B(_1Qo(_1Qq[2]));}));});}},_1Qr=function(_1Qs,_1Qt,_1Qu,_1Qv,_1Qw,_1Qx){return new F(function(){return _64(_1r,_19x,_3r,_1Qt,function(_1Qy,_1Qz,_1QA){return new F(function(){return A(_1Qu,[_1Qs,_1Qz,new T(function(){var _1QB=E(_1QA),_1QC=B(_3P(_1QB[1],_1QB[2],E(_1Qz)[2],_h));return [0,E(_1QC[1]),_1QC[2]];})]);});},_1Qv,function(_1QD,_1QE,_1QF){return new F(function(){return A(_1Qw,[_1Qs,_1QE,new T(function(){var _1QG=E(_1QF),_1QH=B(_3P(_1QG[1],_1QG[2],E(_1QE)[2],_h));return [0,E(_1QH[1]),_1QH[2]];})]);});},_1Qx);});},_1QI=new T(function(){return B(unCStr("..."));}),_1QJ=function(_1QK,_1QL){var _1QM=E(_1QK);if(!_1QM[0]){return E(_1QI);}else{var _1QN=_1QM[1];return _1QL>1?[1,_1QN,new T(function(){return B(_1QJ(_1QM[2],_1QL-1|0));})]:[1,_1QN,_1QI];}},_1QO=true,_1QP=[0,E(_1QO),_G],_1QQ=function(_1QR,_1QS){switch(E(_1QR)[0]){case 0:switch(E(_1QS)[0]){case 0:return 1;case 1:return 0;case 2:return 0;default:return 0;}break;case 1:switch(E(_1QS)[0]){case 0:return 2;case 1:return 1;case 2:return 0;default:return 0;}break;case 2:switch(E(_1QS)[0]){case 0:return 2;case 1:return 2;case 2:return 1;default:return 0;}break;default:switch(E(_1QS)[0]){case 0:return 2;case 1:return 2;case 2:return 2;default:return 1;}}},_1QT=new T(function(){return B(unCStr("end of input"));}),_1QU=new T(function(){return B(unCStr("unexpected"));}),_1QV=new T(function(){return B(unCStr("expecting"));}),_1QW=new T(function(){return B(unCStr("unknown parse error"));}),_1QX=new T(function(){return B(unCStr("or"));}),_1QY=[0,58],_1QZ=[0,34],_1R0=[0,41],_1R1=[1,_1R0,_h],_1R2=function(_1R3,_1R4,_1R5){var _1R6=new T(function(){return B(unAppCStr("(line ",new T(function(){return B(_0(B(_b(0,_1R4,_h)),new T(function(){return B(unAppCStr(", column ",new T(function(){return B(_0(B(_b(0,_1R5,_h)),_1R1));})));})));})));}),_1R7=E(_1R3);return _1R7[0]==0?E(_1R6):[1,_1QZ,new T(function(){return B(_0(_1R7,new T(function(){return B(unAppCStr("\" ",_1R6));})));})];},_1R8=new T(function(){return B(_Ct(_7I));}),_1R9=function(_1Ra,_1Rb,_1Rc){var _1Rd=E(_1Rc);if(!_1Rd[0]){return E(_1Rb);}else{return new F(function(){return _0(_1Rb,new T(function(){return B(_0(_1Ra,new T(function(){return B(_1R9(_1Ra,_1Rd[1],_1Rd[2]));})));}));});}},_1Re=function(_1Rf){return E(_1Rf)[0]==0?false:true;},_1Rg=new T(function(){return B(unCStr(", "));}),_1Rh=function(_1Ri){var _1Rj=E(_1Ri);if(!_1Rj[0]){return [0];}else{return new F(function(){return _0(_1Rj[1],new T(function(){return B(_1Rh(_1Rj[2]));}));});}},_1Rk=function(_1Rl,_1Rm){var _1Rn=E(_1Rm);return _1Rn[0]==0?[0]:[1,_1Rl,new T(function(){return B(_1Rk(_1Rn[1],_1Rn[2]));})];},_1Ro=function(_1Rp,_1Rq){while(1){var _1Rr=E(_1Rq);if(!_1Rr[0]){return E(_1Rp);}else{_1Rp=_1Rr[1];_1Rq=_1Rr[2];continue;}}},_1Rs=function(_1Rt){switch(E(_1Rt)[0]){case 0:return true;case 1:return false;case 2:return false;default:return false;}},_1Ru=function(_1Rv){switch(E(_1Rv)[0]){case 0:return false;case 1:return true;case 2:return false;default:return false;}},_1Rw=function(_1Rx){switch(E(_1Rx)[0]){case 0:return false;case 1:return false;case 2:return true;default:return false;}},_1Ry=[0,10],_1Rz=[1,_1Ry,_h],_1RA=function(_1RB){return new F(function(){return _0(_1Rz,_1RB);});},_1RC=[0,32],_1RD=function(_1RE){var _1RF=E(_1RE);switch(_1RF[0]){case 0:return E(_1RF[1]);case 1:return E(_1RF[1]);case 2:return E(_1RF[1]);default:return E(_1RF[1]);}},_1RG=function(_1RH,_1RI){var _1RJ=function(_1RK,_1RL){while(1){var _1RM=(function(_1RN,_1RO){var _1RP=E(_1RN);if(!_1RP[0]){return [0];}else{var _1RQ=_1RP[1],_1RR=_1RP[2];if(!B(_kO(_1RH,_1RQ,_1RO))){return [1,_1RQ,new T(function(){return B(_1RJ(_1RR,[1,_1RQ,_1RO]));})];}else{_1RK=_1RR;var _1RS=_1RO;_1RL=_1RS;return null;}}})(_1RK,_1RL);if(_1RM!=null){return _1RM;}}};return new F(function(){return _1RJ(_1RI,_h);});},_1RT=function(_1RU,_1RV,_1RW,_1RX,_1RY,_1RZ){var _1S0=E(_1RZ);if(!_1S0[0]){return E(_1RV);}else{var _1S1=new T(function(){var _1S2=B(_ws(_1Rs,_1S0));return [0,_1S2[1],_1S2[2]];}),_1S3=new T(function(){var _1S4=B(_ws(_1Ru,E(_1S1)[2]));return [0,_1S4[1],_1S4[2]];}),_1S5=new T(function(){return E(E(_1S3)[1]);}),_1S6=function(_1S7,_1S8){var _1S9=E(_1S8);if(!_1S9[0]){return E(_1S7);}else{var _1Sa=new T(function(){return B(_0(_1RU,[1,_1RC,new T(function(){return B(_1Ro(_1S7,_1S9));})]));}),_1Sb=B(_1RG(_1R8,B(_5g(_1Re,B(_1Rk(_1S7,_1S9))))));if(!_1Sb[0]){return new F(function(){return _0(_h,[1,_1RC,_1Sa]);});}else{var _1Sc=_1Sb[1],_1Sd=E(_1Sb[2]);if(!_1Sd[0]){return new F(function(){return _0(_1Sc,[1,_1RC,_1Sa]);});}else{return new F(function(){return _0(B(_0(_1Sc,new T(function(){return B(_0(_1Rg,new T(function(){return B(_1R9(_1Rg,_1Sd[1],_1Sd[2]));})));}))),[1,_1RC,_1Sa]);});}}}},_1Se=function(_1Sf,_1Sg){var _1Sh=B(_1RG(_1R8,B(_5g(_1Re,B(_Lw(_1RD,_1Sg))))));if(!_1Sh[0]){return [0];}else{var _1Si=_1Sh[1],_1Sj=_1Sh[2],_1Sk=E(_1Sf);return _1Sk[0]==0?B(_1S6(_1Si,_1Sj)):B(_0(_1Sk,[1,_1RC,new T(function(){return B(_1S6(_1Si,_1Sj));})]));}},_1Sl=new T(function(){var _1Sm=B(_ws(_1Rw,E(_1S3)[2]));return [0,_1Sm[1],_1Sm[2]];});return new F(function(){return _1Rh(B(_Lw(_1RA,B(_1RG(_1R8,B(_5g(_1Re,[1,new T(function(){if(!E(_1S5)[0]){var _1Sn=E(E(_1S1)[1]);if(!_1Sn[0]){var _1So=[0];}else{var _1Sp=E(_1Sn[1]);switch(_1Sp[0]){case 0:var _1Sq=E(_1Sp[1]),_1Sr=_1Sq[0]==0?B(_0(_1RX,[1,_1RC,_1RY])):B(_0(_1RX,[1,_1RC,_1Sq]));break;case 1:var _1Ss=E(_1Sp[1]),_1Sr=_1Ss[0]==0?B(_0(_1RX,[1,_1RC,_1RY])):B(_0(_1RX,[1,_1RC,_1Ss]));break;case 2:var _1St=E(_1Sp[1]),_1Sr=_1St[0]==0?B(_0(_1RX,[1,_1RC,_1RY])):B(_0(_1RX,[1,_1RC,_1St]));break;default:var _1Su=E(_1Sp[1]),_1Sr=_1Su[0]==0?B(_0(_1RX,[1,_1RC,_1RY])):B(_0(_1RX,[1,_1RC,_1Su]));}var _1So=_1Sr;}var _1Sv=_1So,_1Sw=_1Sv;}else{var _1Sw=[0];}return _1Sw;}),[1,new T(function(){return B(_1Se(_1RX,_1S5));}),[1,new T(function(){return B(_1Se(_1RW,E(_1Sl)[1]));}),[1,new T(function(){return B((function(_1Sx){var _1Sy=B(_1RG(_1R8,B(_5g(_1Re,B(_Lw(_1RD,_1Sx))))));return _1Sy[0]==0?[0]:B(_1S6(_1Sy[1],_1Sy[2]));})(E(_1Sl)[2]));}),_h]]]])))))));});}},_1Sz=[1,_h,_h],_1SA=function(_1SB,_1SC){var _1SD=function(_1SE,_1SF){var _1SG=E(_1SE);if(!_1SG[0]){return E(_1SF);}else{var _1SH=_1SG[1],_1SI=E(_1SF);if(!_1SI[0]){return E(_1SG);}else{var _1SJ=_1SI[1];return B(A(_1SB,[_1SH,_1SJ]))==2?[1,_1SJ,new T(function(){return B(_1SD(_1SG,_1SI[2]));})]:[1,_1SH,new T(function(){return B(_1SD(_1SG[2],_1SI));})];}}},_1SK=function(_1SL){var _1SM=E(_1SL);if(!_1SM[0]){return [0];}else{var _1SN=E(_1SM[2]);return _1SN[0]==0?E(_1SM):[1,new T(function(){return B(_1SD(_1SM[1],_1SN[1]));}),new T(function(){return B(_1SK(_1SN[2]));})];}},_1SO=function(_1SP){while(1){var _1SQ=E(_1SP);if(!_1SQ[0]){return E(new T(function(){return B(_1SO(B(_1SK(_h))));}));}else{if(!E(_1SQ[2])[0]){return E(_1SQ[1]);}else{_1SP=B(_1SK(_1SQ));continue;}}}},_1SR=function(_1SS){var _1ST=E(_1SS);if(!_1ST[0]){return E(_1Sz);}else{var _1SU=_1ST[1],_1SV=E(_1ST[2]);if(!_1SV[0]){return [1,_1ST,_h];}else{var _1SW=_1SV[1],_1SX=_1SV[2];if(B(A(_1SB,[_1SU,_1SW]))==2){return new F(function(){return _1SY(_1SW,[1,_1SU,_h],_1SX);});}else{return new F(function(){return _1SZ(_1SW,function(_1T0){return [1,_1SU,_1T0];},_1SX);});}}}},_1T1=new T(function(){return B(_1SR(_h));}),_1SY=function(_1T2,_1T3,_1T4){while(1){var _1T5=(function(_1T6,_1T7,_1T8){var _1T9=E(_1T8);if(!_1T9[0]){return [1,[1,_1T6,_1T7],_1T1];}else{var _1Ta=_1T9[1];if(B(A(_1SB,[_1T6,_1Ta]))==2){_1T2=_1Ta;var _1Tb=[1,_1T6,_1T7];_1T4=_1T9[2];_1T3=_1Tb;return null;}else{return [1,[1,_1T6,_1T7],new T(function(){return B(_1SR(_1T9));})];}}})(_1T2,_1T3,_1T4);if(_1T5!=null){return _1T5;}}},_1SZ=function(_1Tc,_1Td,_1Te){while(1){var _1Tf=(function(_1Tg,_1Th,_1Ti){var _1Tj=E(_1Ti);if(!_1Tj[0]){return [1,new T(function(){return B(A(_1Th,[[1,_1Tg,_h]]));}),_1T1];}else{var _1Tk=_1Tj[1],_1Tl=_1Tj[2];switch(B(A(_1SB,[_1Tg,_1Tk]))){case 0:_1Tc=_1Tk;_1Td=function(_1Tm){return new F(function(){return A(_1Th,[[1,_1Tg,_1Tm]]);});};_1Te=_1Tl;return null;case 1:_1Tc=_1Tk;_1Td=function(_1Tn){return new F(function(){return A(_1Th,[[1,_1Tg,_1Tn]]);});};_1Te=_1Tl;return null;default:return [1,new T(function(){return B(A(_1Th,[[1,_1Tg,_h]]));}),new T(function(){return B(_1SR(_1Tj));})];}}})(_1Tc,_1Td,_1Te);if(_1Tf!=null){return _1Tf;}}};return new F(function(){return _1SO(B(_1SR(_1SC)));});},_1To=function(_1Tp,_1Tq,_1Tr,_1Ts){return new F(function(){return _0(B(_1R2(_1Tp,_1Tq,_1Tr)),[1,_1QY,new T(function(){return B(_1RT(_1QX,_1QW,_1QV,_1QU,_1QT,B(_1SA(_1QQ,_1Ts))));})]);});},_1Tt=function(_1Tu,_1Tv,_1Tw){return [0,_1Tu,E(E(_1Tv)),_1Tw];},_1Tx=function(_1Ty,_1Tz,_1TA){var _1TB=new T(function(){return B(_tR(_1Ty));}),_1TC=new T(function(){return B(_tR(_1Ty));});return new F(function(){return A(_1Tz,[_1TA,function(_1TD,_1TE,_1TF){return new F(function(){return A(_1TC,[[0,new T(function(){return B(A(_1TB,[new T(function(){return B(_1Tt(_1TD,_1TE,_1TF));})]));})]]);});},function(_1TG){return new F(function(){return A(_1TC,[[0,new T(function(){return B(A(_1TB,[[1,_1TG]]));})]]);});},function(_1TH,_1TI,_1TJ){return new F(function(){return A(_1TC,[new T(function(){return [1,E(B(A(_1TB,[new T(function(){return B(_1Tt(_1TH,_1TI,_1TJ));})])))];})]);});},function(_1TK){return new F(function(){return A(_1TC,[new T(function(){return [1,E(B(A(_1TB,[[1,_1TK]])))];})]);});}]);});},_1TL=function(_1TM,_1TN,_1TO,_1TP,_1TQ){var _1TR=new T(function(){return B(_tR(_1TM));});return new F(function(){return A(_3s,[_1TM,new T(function(){return B(_1Tx(_1TM,_1TN,new T(function(){return [0,_1TQ,E([0,_1TP,E(_kT),E(_kT)]),E(E(_1TO))];})));}),function(_1TS){return new F(function(){return A(new T(function(){return B(_3s(_1TM));}),[new T(function(){var _1TT=E(_1TS);return _1TT[0]==0?E(_1TT[1]):E(_1TT[1]);}),function(_1TU){var _1TV=E(_1TU);return _1TV[0]==0?B(A(_1TR,[[1,_1TV[1]]])):B(A(_1TR,[[0,_1TV[1]]]));}]);});}]);});},_1TW=[0,2],_1TX=[1,_1u4,_h],_1TY=function(_1TZ,_1U0,_1U1,_1U2){var _1U3=B(_1TL(_1r,_1TZ,_1U0,_1U1,_1U2));return _1U3[0]==0?[1,new T(function(){var _1U4=[0,[10,_1TW,new T(function(){var _1U5=E(_1U3[1]),_1U6=E(_1U5[1]);return B(_0(B(_1To(_1U6[1],E(_1U6[2])[1],E(_1U6[3])[1],_1U5[2])),_1TX));})],_h],_1U7=B(_6v(_6O,_1U4));return E(_1U4);}),_h]:E(_1U3[1]);},_1U8=function(_1U9,_1Ua){return new F(function(){return _1TY(function(_1Ub,_1Uc,_1Ud,_1Ue,_1Uf){return new F(function(){return A(_1U9,[_1Ub,function(_1Ug,_1Uh,_1Ui){return new F(function(){return _1Qr(_1Ug,_1Uh,_1Uc,_1Ud,function(_1Uj,_1Uk,_1Ul){return new F(function(){return A(_1Uc,[_1Uj,_1Uk,new T(function(){return B(_4a(_1Ui,_1Ul));})]);});},function(_1Um){return new F(function(){return A(_1Ud,[new T(function(){return B(_4a(_1Ui,_1Um));})]);});});});},_1Ud,function(_1Un,_1Uo,_1Up){return new F(function(){return _1Qr(_1Un,_1Uo,_1Uc,_1Ud,function(_1Uq,_1Ur,_1Us){return new F(function(){return A(_1Ue,[_1Uq,_1Ur,new T(function(){return B(_4a(_1Up,_1Us));})]);});},function(_1Ut){return new F(function(){return A(_1Uf,[new T(function(){return B(_4a(_1Up,_1Ut));})]);});});});},_1Uf]);});},_1QP,new T(function(){return B(unAppCStr("string: ",[1,_39,new T(function(){if(B(_E2(_1Ua,0))<=40){var _1Uu=B(_3b(_1Ua,_Lt));}else{var _1Uu=B(_3b(B(_1QJ(_1Ua,40)),_Lt));}var _1Uv=_1Uu;return _1Uv;})]));}),_1Ua);});},_1Uw=function(_1Ux){return [0,function(_){var _1Uy=B(_1MC(B(_1Qo(B(_1U8(_1Ms,new T(function(){var _1Uz=E(_1Ux);return _1Uz[0]==0?E(_1MM):E(_1Uz[1]);}))))),_)),_1UA=_1Uy;return _1d;}];},_1UB=function(_1UC,_1UD){var _1UE=E(_1UC);return _1UE[0]==0?[0]:B(A(_1UD,[_1UE[1]]));},_1UF=new T(function(){return B(unCStr("POST"));}),_1UG=new T(function(){return B(unCStr("GET"));}),_1UH=[0,63],_1UI=[1,_1UH,_h],_1UJ=new T(function(){return [0,toJSStr(_1UI)];}),_1UK=new T(function(){return [0,toJSStr(_h)];}),_1UL=[0,38],_1UM=[1,_1UL,_h],_1UN=new T(function(){return [0,toJSStr(_1UM)];}),_1UO=[0,61],_1UP=[1,_1UO,_h],_1UQ=new T(function(){return [0,toJSStr(_1UP)];}),_1UR=function(_1US,_1UT){var _1UU=jsCat([1,_1US,[1,_1UT,_h]],E(_1UQ)[1]),_1UV=_1UU;return [0,_1UV];},_1UW=function(_1UX){var _1UY=E(_1UX);return new F(function(){return _1UR(_1UY[1],_1UY[2]);});},_1UZ=function(_1V0){var _1V1=jsCat(new T(function(){return B(_Lw(_1UW,_1V0));}),E(_1UN)[1]),_1V2=_1V1;return [0,_1V2];},_1V3=function(_1V4,_1V5,_1V6,_1V7,_1V8){return new F(function(){return A(_1V4,[function(_){var _1V9=function(_1Va){var _1Vb=function(_1Vc){var _1Vd=ajaxReq(toJSStr(_1Va),_1Vc,1,E(_1UK)[1],_1V8);return _G;},_1Ve=E(_1V7);if(!_1Ve[0]){return new F(function(){return _1Vb(E(_1V6)[1]);});}else{var _1Vf=jsCat([1,_1V6,[1,new T(function(){return B(_1UZ(_1Ve));}),_h]],E(_1UJ)[1]),_1Vg=_1Vf;return new F(function(){return _1Vb(_1Vg);});}};if(!E(_1V5)){return new F(function(){return _1V9(E(_1UG));});}else{return new F(function(){return _1V9(E(_1UF));});}}]);});},_1Vh=0,_1Vi=function(_1Vj){return [0,toJSStr(E(_1Vj))];},_1Vk=function(_1Vl){var _1Vm=E(_1Vl);if(!_1Vm[0]){return [0];}else{var _1Vn=E(_1Vm[1]);return [1,[0,new T(function(){return B(_1Vi(_1Vn[1]));}),new T(function(){return B(_1Vi(_1Vn[2]));})],new T(function(){return B(_1Vk(_1Vm[2]));})];}},_1Vo=function(_1Vp){return [2];},_1Vq=function(_1Vr){return E(E(_1Vr)[2]);},_1Vs=function(_1Vt){return new F(function(){return _1Vo(_1Vt);});},_1Vu=function(_1Vv,_1Vw,_1Vx){return [0,function(_){var _1Vy=E(_1Vv)[1],_1Vz=rMV(_1Vy),_1VA=_1Vz,_1VB=E(_1VA);if(!_1VB[0]){var _=wMV(_1Vy,[0,_1VB[1],new T(function(){return B(_0(_1VB[2],[1,[0,_1Vw,function(_1VC){return E(new T(function(){return B(A(_1Vx,[_G]));}));}],_h]));})]);return _1d;}else{var _1VD=E(_1VB[1]);if(!_1VD[0]){var _=wMV(_1Vy,[0,_1Vw,_h]);return new T(function(){return B(A(_1Vx,[_G]));});}else{var _=wMV(_1Vy,[1,_1VD[2]]);return [1,[1,new T(function(){return B(A(_1Vx,[_G]));}),[1,new T(function(){return B(A(_1VD[1],[_1Vw,_1Vs]));}),_h]]];}}}];},_1VE=[1,_h],_1VF=function(_1VG,_1VH){return [0,function(_){var _1VI=E(_1VG)[1],_1VJ=rMV(_1VI),_1VK=_1VJ,_1VL=E(_1VK);if(!_1VL[0]){var _1VM=_1VL[1],_1VN=E(_1VL[2]);if(!_1VN[0]){var _=wMV(_1VI,_1VE);return new T(function(){return B(A(_1VH,[_1VM]));});}else{var _1VO=E(_1VN[1]),_=wMV(_1VI,[0,_1VO[1],_1VN[2]]);return [1,[1,new T(function(){return B(A(_1VH,[_1VM]));}),[1,new T(function(){return B(A(_1VO[2],[_1Vs]));}),_h]]];}}else{var _=wMV(_1VI,[1,new T(function(){return B(_0(_1VL[1],[1,function(_1VP){return function(_1VQ){return E(new T(function(){return B(A(_1VH,[_1VP]));}));};},_h]));})]);return _1d;}}];},_1VR=function(_1VS,_1VT,_1VU){return function(_1VV){return [0,function(_){var _1VW=nMV(_1VE),_1VX=_1VW,_1VY=[0,_1VX];return [0,function(_){var _1VZ=B(A(_1V3,[_1Nt,_1Vh,new T(function(){return [0,toJSStr(E(_1VT))];}),new T(function(){return B(_1Vk(_1VU));}),function(_1W0,_){return new F(function(){return _S([1,new T(function(){return B(_1Vu(_1VY,new T(function(){return B(_1UB(_1W0,new T(function(){return B(_1Vq(_1VS));})));}),_1Vo));}),_h],_);});},_])),_1W1=_1VZ;return new T(function(){return B(_1VF(_1VY,_1VV));});}];}];};},_1W2=new T(function(){return B(unCStr("stn.aspx"));}),_1W3=[0,47],_1W4=[1,_1W3,_1W2],_1W5=new T(function(){return B(unAppCStr("http://api.bart.gov/api",_1W4));}),_1W6=function(_1W7){return E(E(_1W7)[2]);},_1W8=function(_1W9){return E(E(_1W9)[1]);},_1Wa=function(_1Wb){return [0,new T(function(){return B(_1W8(_1Wb));}),new T(function(){return B(_1W6(_1Wb));})];},_1Wc=function(_1Wd){return new F(function(){return fromJSStr(E(_1Wd)[1]);});},_1We=function(_1Wf){return [1,new T(function(){return B(_1Wc(_1Wf));})];},_1Wg=[0,_1Vi,_1We],_1Wh=new T(function(){return B(_1Wa(_1Wg));}),_1Wi=new T(function(){return B(unCStr("MW9S-E7SL-26DU-VV8V"));}),_1Wj=new T(function(){return B(unCStr("key"));}),_1Wk=[0,_1Wj,_1Wi],_1Wl=[1,_1Wk,_h],_1Wm=new T(function(){return B(unCStr("cmd"));}),_1Wn=new T(function(){return B(unCStr("stns"));}),_1Wo=[0,_1Wm,_1Wn],_1Wp=[1,_1Wo,_1Wl],_1Wq=new T(function(){return B(_1VR(_1Wh,_1W5,_1Wp));}),_1Wr=new T(function(){return B(A(_1Wq,[_1Uw]));}),_1Ws=[1,_1Wr,_h],_1Wt=[0,0],_1Wu=new T(function(){return [0,"window.innerHeight"];}),_1Wv=new T(function(){return [0,"window.innerWidth"];}),_1Ww=function(_1Wx,_1Wy){return E(_1Wx)[1]!=E(_1Wy)[1]?true:false;},_1Wz=function(_1WA,_1WB){return E(_1WA)[1]==E(_1WB)[1];},_1WC=[0,_1Wz,_1Ww],_1WD=function(_1WE){var _1WF=E(_1WE)[1];return [0,Math.log(_1WF+(_1WF+1)*Math.sqrt((_1WF-1)/(_1WF+1)))];},_1WG=function(_1WH){var _1WI=E(_1WH)[1];return [0,Math.log(_1WI+Math.sqrt(1+_1WI*_1WI))];},_1WJ=function(_1WK){var _1WL=E(_1WK)[1];return [0,0.5*Math.log((1+_1WL)/(1-_1WL))];},_1WM=function(_1WN,_1WO){return [0,Math.log(E(_1WO)[1])/Math.log(E(_1WN)[1])];},_1WP=[0,3.141592653589793],_1WQ=new T(function(){return [0,0/0];}),_1WR=new T(function(){return [0,-1/0];}),_1WS=new T(function(){return [0,1/0];}),_1WT=[0,0],_1WU=function(_1WV,_1WW){while(1){var _1WX=E(_1WV);if(!_1WX[0]){_1WV=[1,I_fromInt(_1WX[1])];continue;}else{var _1WY=E(_1WW);if(!_1WY[0]){_1WV=_1WX;_1WW=[1,I_fromInt(_1WY[1])];continue;}else{return new F(function(){return I_fromRat(_1WX[1],_1WY[1]);});}}}},_1WZ=function(_1X0,_1X1){var _1X2=E(_1X0);if(!_1X2[0]){var _1X3=_1X2[1],_1X4=E(_1X1);return _1X4[0]==0?_1X3==_1X4[1]:I_compareInt(_1X4[1],_1X3)==0?true:false;}else{var _1X5=_1X2[1],_1X6=E(_1X1);return _1X6[0]==0?I_compareInt(_1X5,_1X6[1])==0?true:false:I_compare(_1X5,_1X6[1])==0?true:false;}},_1X7=function(_1X8,_1X9){var _1Xa=E(_1X8);if(!_1Xa[0]){var _1Xb=_1Xa[1],_1Xc=E(_1X9);return _1Xc[0]==0?_1Xb<_1Xc[1]:I_compareInt(_1Xc[1],_1Xb)>0;}else{var _1Xd=_1Xa[1],_1Xe=E(_1X9);return _1Xe[0]==0?I_compareInt(_1Xd,_1Xe[1])<0:I_compare(_1Xd,_1Xe[1])<0;}},_1Xf=function(_1Xg,_1Xh){return !B(_1WZ(_1Xh,_1WT))?[0,B(_1WU(_1Xg,_1Xh))]:!B(_1WZ(_1Xg,_1WT))?!B(_1X7(_1Xg,_1WT))?E(_1WS):E(_1WR):E(_1WQ);},_1Xi=function(_1Xj){var _1Xk=E(_1Xj);return new F(function(){return _1Xf(_1Xk[1],_1Xk[2]);});},_1Xl=function(_1Xm){return [0,1/E(_1Xm)[1]];},_1Xn=function(_1Xo){var _1Xp=E(_1Xo),_1Xq=_1Xp[1];return _1Xq<0?[0, -_1Xq]:E(_1Xp);},_1Xr=function(_1Xs){var _1Xt=E(_1Xs);return _1Xt[0]==0?_1Xt[1]:I_toNumber(_1Xt[1]);},_1Xu=function(_1Xv){return [0,B(_1Xr(_1Xv))];},_1Xw=[0,0],_1Xx=[0,1],_1Xy=[0,-1],_1Xz=function(_1XA){var _1XB=E(_1XA)[1];return _1XB!=0?_1XB<=0?E(_1Xy):E(_1Xx):E(_1Xw);},_1XC=function(_1XD,_1XE){return [0,E(_1XD)[1]-E(_1XE)[1]];},_1XF=function(_1XG){return [0, -E(_1XG)[1]];},_1XH=function(_1XI,_1XJ){return [0,E(_1XI)[1]+E(_1XJ)[1]];},_1XK=function(_1XL,_1XM){return [0,E(_1XL)[1]*E(_1XM)[1]];},_1XN=[0,_1XH,_1XK,_1XC,_1XF,_1Xn,_1Xz,_1Xu],_1XO=function(_1XP,_1XQ){return [0,E(_1XP)[1]/E(_1XQ)[1]];},_1XR=[0,_1XN,_1XO,_1Xl,_1Xi],_1XS=function(_1XT){return [0,Math.acos(E(_1XT)[1])];},_1XU=function(_1XV){return [0,Math.asin(E(_1XV)[1])];},_1XW=function(_1XX){return [0,Math.atan(E(_1XX)[1])];},_1XY=function(_1XZ){return [0,Math.cos(E(_1XZ)[1])];},_1Y0=function(_1Y1){return [0,cosh(E(_1Y1)[1])];},_1Y2=function(_1Y3){return [0,Math.exp(E(_1Y3)[1])];},_1Y4=function(_1Y5){return [0,Math.log(E(_1Y5)[1])];},_1Y6=function(_1Y7,_1Y8){return [0,Math.pow(E(_1Y7)[1],E(_1Y8)[1])];},_1Y9=function(_1Ya){return [0,Math.sin(E(_1Ya)[1])];},_1Yb=function(_1Yc){return [0,sinh(E(_1Yc)[1])];},_1Yd=function(_1Ye){return [0,Math.sqrt(E(_1Ye)[1])];},_1Yf=function(_1Yg){return [0,Math.tan(E(_1Yg)[1])];},_1Yh=function(_1Yi){return [0,tanh(E(_1Yi)[1])];},_1Yj=[0,_1XR,_1WP,_1Y2,_1Yd,_1Y4,_1Y6,_1WM,_1Y9,_1Yf,_1XY,_1XU,_1XW,_1XS,_1Yb,_1Yh,_1Y0,_1WG,_1WJ,_1WD],_1Yk=[0,0],_1Yl=[0,_1Yk,_1Yk],_1Ym=function(_1Yn,_1Yo){return E(_1Yn)[1]<E(_1Yo)[1];},_1Yp=function(_1Yq,_1Yr){return E(_1Yq)[1]<=E(_1Yr)[1];},_1Ys=function(_1Yt,_1Yu){return E(_1Yt)[1]>E(_1Yu)[1];},_1Yv=function(_1Yw,_1Yx){return E(_1Yw)[1]>=E(_1Yx)[1];},_1Yy=function(_1Yz,_1YA){var _1YB=E(_1Yz)[1],_1YC=E(_1YA)[1];return _1YB>=_1YC?_1YB!=_1YC?2:1:0;},_1YD=function(_1YE,_1YF){var _1YG=E(_1YE),_1YH=E(_1YF);return _1YG[1]>_1YH[1]?E(_1YG):E(_1YH);},_1YI=function(_1YJ,_1YK){var _1YL=E(_1YJ),_1YM=E(_1YK);return _1YL[1]>_1YM[1]?E(_1YM):E(_1YL);},_1YN=[0,_1WC,_1Yy,_1Ym,_1Yv,_1Ys,_1Yp,_1YD,_1YI],_1YO=function(_1YP){return E(E(_1YP)[1]);},_1YQ=function(_1YR){return E(E(_1YR)[2]);},_1YS=function(_1YT){return E(E(_1YT)[1]);},_1YU=function(_1YV){return E(E(_1YV)[2]);},_1YW=function(_1YX){return E(E(_1YX)[7]);},_1YY=[0,2],_1YZ=[0,3],_1Z0=[0,0],_1Z1=function(_1Z2,_1Z3,_1Z4,_1Z5,_1Z6,_1Z7,_1Z8,_1Z9,_1Za){var _1Zb=new T(function(){return B(_1YO(_1Z6));}),_1Zc=new T(function(){var _1Zd=new T(function(){if(!B(A(_1Z3,[_1Z9,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))){var _1Ze=new T(function(){return !B(A(_1Z5,[_1Z9,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?B(A(_1YW,[_1Zb,_1Z0])):!B(A(_1Z3,[_1Za,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?B(A(_1YW,[_1Zb,_1Z0])):B(A(_1YS,[_1Zb,new T(function(){return B(A(_1Z8,[new T(function(){return B(A(_1YU,[_1Z6,_1Za,_1Z9]));})]));}),new T(function(){return B(A(_1YQ,[_1Zb,new T(function(){return B(A(_1YW,[_1Zb,_1YY]));}),_1Z7]));})]));}),_1Zf=!B(A(_kM,[_1Z2,_1Z9,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?E(_1Ze):!B(A(_1Z3,[_1Za,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?E(_1Ze):B(A(_1YU,[_1Z6,new T(function(){return B(A(_1YQ,[_1Zb,new T(function(){return B(A(_1YW,[_1Zb,_1YZ]));}),_1Z7]));}),new T(function(){return B(A(_1YW,[_1Zb,_1YY]));})]));}else{var _1Zf=B(A(_1YS,[_1Zb,new T(function(){return B(A(_1Z8,[new T(function(){return B(A(_1YU,[_1Z6,_1Za,_1Z9]));})]));}),_1Z7]));}return _1Zf;});return !B(A(_kM,[_1Z2,_1Z9,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?E(_1Zd):!B(A(_1Z5,[_1Za,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?E(_1Zd):B(A(_1YU,[_1Z6,_1Z7,new T(function(){return B(A(_1YW,[_1Zb,_1YY]));})]));});return !B(A(_1Z5,[_1Z9,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?E(_1Zc):!B(A(_1Z4,[_1Za,new T(function(){return B(A(_1YW,[_1Zb,_1Z0]));})]))?E(_1Zc):B(A(_1Z8,[new T(function(){return B(A(_1YU,[_1Z6,_1Za,_1Z9]));})]));},_1Zg=function(_1Zh){return E(E(_1Zh)[1]);},_1Zi=function(_1Zj,_1Zk,_1Zl,_1Zm){return new F(function(){return A(_1Zk,[new T(function(){var _1Zn=E(E(_1Zj)[1]),_1Zo=_1Zn[2];return B(A(_1Zn[1],[new T(function(){return B(A(_1Zo,[_1Zl,_1Zl]));}),new T(function(){return B(A(_1Zo,[_1Zm,_1Zm]));})]));})]);});},_1Zp=function(_1Zq){return E(E(_1Zq)[10]);},_1Zr=function(_1Zs){return E(E(_1Zs)[8]);},_1Zt=function(_1Zu,_1Zv,_1Zw,_1Zx,_1Zy){var _1Zz=new T(function(){return B(_1YO(new T(function(){return B(_1Zg(_1Zw));})));}),_1ZA=new T(function(){return B(A(_1YS,[_1Zz,_1Zx,new T(function(){var _1ZB=E(_1Zv),_1ZC=E(_1Zw),_1ZD=E(_1Zy);return B(_1Z1(_1Zu,_1ZB[3],_1ZB[4],_1ZB[5],_1ZC[1],_1ZC[2],_1ZC[12],_1ZD[1],_1ZD[2]));})]));}),_1ZE=new T(function(){var _1ZF=E(_1Zw),_1ZG=E(_1Zy);return B(_1Zi(_1ZF[1],_1ZF[4],_1ZG[1],_1ZG[2]));});return [0,new T(function(){return B(A(_1YQ,[_1Zz,_1ZE,new T(function(){return B(A(_1Zp,[_1Zw,_1ZA]));})]));}),new T(function(){return B(A(_1YQ,[_1Zz,_1ZE,new T(function(){return B(A(_1Zr,[_1Zw,_1ZA]));})]));})];},_1ZH=[0,1],_1ZI=[0,_1ZH,_1Yk],_1ZJ=new T(function(){var _1ZK=B(_1Zt(_1WC,_1YN,_1Yj,_1Yk,_1ZI));return [0,_1ZK[1],_1ZK[2]];}),_1ZL=function(_1ZM,_1ZN){var _1ZO=E(_1ZM);if(!_1ZO[0]){var _1ZP=E(_1ZJ),_1ZQ=new T(function(){return [0,E(_1ZN)[1]*E(_1ZO[1])[1]];});return [0,new T(function(){return B(_1XK(_1ZQ,_1ZP[1]));}),new T(function(){return B(_1XK(_1ZQ,_1ZP[2]));})];}else{var _1ZR=_1ZO[1],_1ZS=_1ZO[2],_1ZT=_1ZO[3],_1ZU=new T(function(){if(!E(_1ZR)){var _1ZV=[0, -E(_1ZS)[1]];}else{var _1ZV=E(_1ZS);}return _1ZV;}),_1ZW=new T(function(){return [0,-1*E(_1ZU)[1]];}),_1ZX=new T(function(){return [0,E(_1ZU)[1]*0];}),_1ZY=new T(function(){return [0,-1*E(_1ZX)[1]];}),_1ZZ=new T(function(){var _200=E(_1ZY)[1],_201=E(_1ZW)[1];return [0,Math.sqrt(_200*_200+_201*_201)];}),_202=new T(function(){if(!E(_1ZR)){var _203=[0, -(E(_1ZN)[1]*E(_1ZT)[1])+B(_1Z1(_1WC,_1Ym,_1Yv,_1Ys,_1XR,_1WP,_1XW,_1ZY,_1ZW))[1]];}else{var _203=[0,E(_1ZN)[1]*E(_1ZT)[1]+B(_1Z1(_1WC,_1Ym,_1Yv,_1Ys,_1XR,_1WP,_1XW,_1ZY,_1ZW))[1]];}return _203;});return [0,new T(function(){return [0,E(_1ZX)[1]+E(_1ZZ)[1]*Math.cos(E(_202)[1])];}),new T(function(){return [0,E(_1ZU)[1]+E(_1ZZ)[1]*Math.sin(E(_202)[1])];})];}},_204=new T(function(){return B(unCStr("head"));}),_205=new T(function(){return B(_1qJ(_204));}),_206=new T(function(){return B(unCStr("tail"));}),_207=new T(function(){return B(_1qJ(_206));}),_208=function(_209){var _20a=new T(function(){var _20b=E(_209);return _20b[0]==0?E(_205):E(_20b[1]);}),_20c=new T(function(){var _20d=E(_20a);if(!_20d[0]){var _20e=E(_1Yk);}else{var _20f=_20d[3];if(!E(_20d[1])){var _20g=[0, -E(_20f)[1]];}else{var _20g=E(_20f);}var _20e=_20g;}return _20e;});return [0,new T(function(){var _20h=B(_1ZL(_20a,_1ZH)),_20i=B(_1Zt(_1WC,_1YN,_1Yj,_20c,_1Yl));return [0,new T(function(){return B(_1XH(_20h[1],_20i[1]));}),new T(function(){return B(_1XH(_20h[2],_20i[2]));})];}),_20c,new T(function(){var _20j=E(_209);return _20j[0]==0?E(_207):E(_20j[2]);})];},_20k=function(_20l){var _20m=jsShow(E(_20l)[1]),_20n=_20m;return new F(function(){return fromJSStr(_20n);});},_20o=function(_20p){return function(_20q){return new F(function(){return _0(new T(function(){return B(_20k(_20p));}),_20q);});};},_20r=[0,45],_20s=function(_20t,_20u,_20v){var _20w=new T(function(){return B(A(_20t,[[0, -_20v]]));}),_20x=new T(function(){if(E(_20u)[1]<=6){var _20y=function(_20z){return [1,_20r,new T(function(){return B(A(_20w,[_20z]));})];};}else{var _20y=function(_20A){return [1,_a,[1,_20r,new T(function(){return B(A(_20w,[[1,_9,_20A]]));})]];};}var _20B=_20y;return _20B;});if(_20v>=0){var _20C=isDoubleNegativeZero(_20v),_20D=_20C;return E(_20D)==0?B(A(_20t,[[0,_20v]])):E(_20x);}else{return E(_20x);}},_20E=[0,0],_20F=function(_20G){return new F(function(){return A(_20s,[_20o,_20E,E(_20G)[1],_h]);});},_20H=[0,77],_20I=[1,_20H,_h],_20J=new T(function(){return B(_20s(_20o,_20E,0));}),_20K=new T(function(){return B(A(_20J,[_h]));}),_20L=new T(function(){return B(_20s(_20o,_20E,1));}),_20M=new T(function(){return B(A(_20L,[_h]));}),_20N=[0,76],_20O=[1,_20N,_h],_20P=[0,65],_20Q=[1,_20P,_h],_20R=new T(function(){return B(A(_20J,[_h]));}),_20S=new T(function(){return B(A(_20L,[_h]));}),_20T=new T(function(){return B(A(_20J,[_h]));}),_20U=[0,32],_20V=function(_20W){var _20X=E(_20W);if(!_20X[0]){return [0];}else{var _20Y=_20X[1],_20Z=E(_20X[2]);if(!_20Z[0]){return E(_20Y);}else{return new F(function(){return _0(_20Y,[1,_20U,new T(function(){return B(_20V(_20Z));})]);});}}},_210=function(_211){var _212=new T(function(){return E(E(_211)[1]);}),_213=new T(function(){return E(E(_211)[3]);});return new F(function(){return _20V([1,_20I,[1,new T(function(){return B(A(_20s,[_20o,_20E,E(E(_212)[1])[1],_h]));}),[1,new T(function(){return B(A(_20s,[_20o,_20E,E(E(_212)[2])[1],_h]));}),[1,new T(function(){return E(_213)[0]==0?E(_20O):E(_20Q);}),new T(function(){var _214=new T(function(){var _215=E(_211),_216=E(_215[1]),_217=B(_1Zt(_1WC,_1YN,_1Yj,_215[2],new T(function(){var _218=B(_1ZL(_215[3],_1ZH));return [0,_218[1],_218[2]];})));return [0,new T(function(){return B(_1XH(_216[1],_217[1]));}),new T(function(){return B(_1XH(_216[2],_217[2]));})];}),_219=E(_213);if(!_219[0]){var _21a=[1,new T(function(){return B(A(_20s,[_20o,_20E,E(E(_214)[1])[1],_h]));}),[1,new T(function(){return B(A(_20s,[_20o,_20E,E(E(_214)[2])[1],_h]));}),_h]];}else{var _21b=_219[2],_21a=[1,new T(function(){return B(_20F(_21b));}),[1,new T(function(){return B(_20F(_21b));}),[1,_20R,[1,new T(function(){return E(_219[3])[1]>3.141592653589793?E(_20S):E(_20T);}),[1,new T(function(){return E(_219[1])==0?E(_20K):E(_20M);}),[1,new T(function(){return B(A(_20s,[_20o,_20E,E(E(_214)[1])[1],_h]));}),[1,new T(function(){return B(A(_20s,[_20o,_20E,E(E(_214)[2])[1],_h]));}),_h]]]]]]];}return _21a;})]]]]);});},_21c=function(_21d,_21e,_21f){var _21g=E(_21f);if(!_21g[0]){return [0];}else{return new F(function(){return _0(B(_210([0,new T(function(){var _21h=E(_21d),_21i=B(_1Zt(_1WC,_1YN,_1Yj,_21e,_1Yl));return [0,new T(function(){return [0,E(_21h[1])[1]+E(_21i[1])[1]];}),new T(function(){return [0,E(_21h[2])[1]+E(_21i[2])[1]];})];}),_21e,_21g[1]])),new T(function(){var _21j=B(_208(_21g));return B(_21c(new T(function(){var _21k=E(_21d),_21l=B(_1Zt(_1WC,_1YN,_1Yj,_21e,_21j[1]));return [0,new T(function(){return [0,E(_21k[1])[1]+E(_21l[1])[1]];}),new T(function(){return [0,E(_21k[2])[1]+E(_21l[2])[1]];})];}),new T(function(){return [0,E(_21e)[1]+E(_21j[2])[1]];}),_21j[3]));}));});}},_21m=[0,2],_21n=[0,10],_21o=[0,_21n,_21n],_21p=[0,5],_21q=[0,_21p],_21r=0,_21s=[0,3],_21t=[1,_21r,_21s,_1WP],_21u=[1,_21t,_h],_21v=[1,_21q,_21u],_21w=new T(function(){return B(_21c(_21o,_21m,_21v));}),_21x=new T(function(){return [0,"(function(pathString,paper){return paper.path(pathString);})"];}),_21y=new T(function(){return B(_x(_21x));}),_21z=function(_){var _21A=B(A(_x,[_1Wv,_])),_21B=_21A,_21C=B(A(_x,[_1Wu,_])),_21D=_21C,_21E=B(_z(_1Wt,_1Wt,new T(function(){var _21F=jsRound(_21B),_21G=_21F;return [0,_21G];}),new T(function(){var _21H=jsRound(_21D),_21I=_21H;return [0,_21I];}),_)),_21J=_21E,_21K=B(A(_1c,[_21J,_])),_21L=_21K,_21M=E(_21w),_21N=B(A(_21y,[E(toJSStr(_21M)),E(E(_21J)[1]),_])),_21O=_21N,_21P=B(_N(_1MB,_21M,_)),_21Q=_21P;return new F(function(){return _S(_1Ws,_);});},_21R=function(_){return new F(function(){return _21z(_);});};
var hasteMain = function() {B(A(_21R, [0]));};window.onload = hasteMain;