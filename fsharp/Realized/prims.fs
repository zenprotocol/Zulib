#light "off"
module Prims

module Check = Checked
module I64 = FSharp.Compatibility.OCaml.Int64
module Obj = FSharp.Compatibility.OCaml.Obj
module P = FSharp.Compatibility.OCaml.Pervasives

// Converts a string to a byte array
let private fsharpStringConvert (s: Core.string): Core.byte Core.array =
    s.ToCharArray() |> Collections.Array.map Check.byte
// Converts a byte array to a string
let private fstarStringConvert: Core.byte Core.array -> Core.string =
    Collections.Array.map Check.char >> System.String
// Ceiling division
let private cdiv (x: Core.int64) (y: Core.int64): Core.int64 =
    let r = x / y in
    if Check.op_Multiply r y < x then Check.(+) r 1L else r
// Flooring division
let private fdiv (x: Core.int64) (y: Core.int64): Core.int64 =
    let r = x / y in
    if Check.( * ) r y > x then Check.(-) r 1L else r
// Euclidean division
let private ediv (x: Core.int64) (y: Core.int64): Core.int64 =
    if y < 0L then cdiv x y else fdiv x y
// Euclidean remainder
let private erem (x: Core.int64) (y: Core.int64): Core.int64 =
    let r = x % y in
    if r < 0L then Check.(+) r (abs y) else r

type string' = Core.byte Core.array // ZF* only

(* Port of prims.ml from upstream F* follows *)

type int = Core.int64
let parse_int (s: string'): int = I64.of_string (fstarStringConvert s)
let to_string (x: int): string' = fsharpStringConvert (I64.to_string x)

//type tmp = string [@@deriving yojson]
//let int_to_yojson x = tmp_to_yojson (to_string x)
//let int_of_yojson x =
//  match tmp_of_yojson x with
//  | Ok x -> Ok (parse_int x)
//  | Error x -> Error x

type attribute = Core.unit
let (cps : attribute): unit = ()
type 'Auu____5 hasEq = Core.unit
type eqtype = Core.unit
type bool' = Core.bool
//[@@deriving yojson,show]
type bool = bool'
//[@@deriving yojson,show]
type c_False = Core.unit
(*This is how Coq extracts Inductive void := . Our extraction needs to be fixed to recognize when there
       are no constructors and generate this type abbreviation*)
type c_True =
  | T
let (uu___is_T : c_True -> bool) = fun projectee  -> true
type unit = Core.unit
type 'Ap squash = unit
type 'Ap auto_squash = unit
type l_True = unit
type l_False = unit
type ('Aa,'Ax,'dummyV0) equals =
  | Refl
let uu___is_Refl : 'Aa -> 'Aa -> ('Aa,unit,unit) equals -> bool =
  fun x  -> fun uu____65  -> fun projectee  -> true
type ('Aa,'Ax,'Ay) eq2 = unit
type ('Aa,'Ax,'dummyV0,'dummyV1) h_equals =
  | HRefl
let uu___is_HRefl :
  'Aa -> unit -> Obj.t -> ('Aa,unit,Obj.t,unit) h_equals -> bool
  = fun x  -> fun b  -> fun uu____141  -> fun projectee  -> true
type ('Aa,'Ab,'Ax,'Ay) eq3 = unit
type ('Aa,'Ab,'Ax,'Ay) op_Equals_Equals_Equals = unit
type 'Ab b2t = unit
type ('Ap,'Aq) c_and =
  | And of 'Ap * 'Aq
let uu___is_And : ('Ap,'Aq) c_and -> bool =
  fun projectee  -> true
let __proj__And__item___0 : ('Ap,'Aq) c_and -> 'Ap =
  fun projectee  -> match projectee with | And (_0,_1) -> _0
let __proj__And__item___1 : ('Ap,'Aq) c_and -> 'Aq =
  fun projectee  -> match projectee with | And (_0,_1) -> _1
type ('Ap,'Aq) l_and = unit
type ('Ap,'Aq) c_or =
  | Left of 'Ap
  | Right of 'Aq
let uu___is_Left : ('Ap,'Aq) c_or -> bool =
  fun projectee  ->
    match projectee with | Left _0 -> true | uu____344 -> false

let __proj__Left__item___0 : ('Ap,'Aq) c_or -> 'Ap =
  fun projectee  -> match projectee with | Left _0 -> _0
let uu___is_Right : ('Ap,'Aq) c_or -> bool =
  fun projectee  ->
    match projectee with | Right _0 -> true | uu____404 -> false

let __proj__Right__item___0 : ('Ap,'Aq) c_or -> 'Aq =
  fun projectee  -> match projectee with | Right _0 -> _0
type ('Ap,'Aq) l_or = unit
type ('Ap,'Aq) l_imp = unit
type ('Ap,'Aq) l_iff = unit
type 'Ap l_not = unit
type ('Ap,'Aq,'Ar) l_ITE = unit
type ('Aa,'Ab,'Auu____484,'Auu____485) precedes = unit
type ('Aa,'Auu____490,'Auu____491) has_type = unit
type ('Aa,'Ap) l_Forall = unit
type prop = unit
type ('Aa,'Ab) dtuple2 =
  | Mkdtuple2 of 'Aa * 'Ab
let uu___is_Mkdtuple2 : ('Aa,'Ab) dtuple2 -> bool =
  fun projectee  -> true
let __proj__Mkdtuple2__item___1 : ('Aa,'Ab) dtuple2 -> 'Aa =
  fun projectee  -> match projectee with | Mkdtuple2 (_1,_2) -> _1
let __proj__Mkdtuple2__item___2 : ('Aa,'Ab) dtuple2 -> 'Ab =
  fun projectee  -> match projectee with | Mkdtuple2 (_1,_2) -> _2
type ('Aa,'Ap) l_Exists = unit
type _pos = int * int
type _rng = string' * _pos * _pos
type range = _rng * _rng
//type string' = string[@@deriving yojson,show]
type string = string'
type pure_pre = unit
type ('Aa,'Apre) pure_post' = unit
type 'Aa pure_post = unit
type 'Aa pure_wp = unit
type 'Auu____655 guard_free = unit
type ('Aa,'Ax,'Ap) pure_return = unit
type ('Ar1,'Aa,'Ab,'Awp1,'Awp2,'Ap) pure_bind_wp = 'Awp1
type ('Aa,'Ap,'Awp_then,'Awp_else,'Apost) pure_if_then_else = unit
type ('Aa,'Awp,'Apost) pure_ite_wp = unit
type ('Aa,'Awp1,'Awp2) pure_stronger = unit
type ('Aa,'Ab,'Awp,'Ap) pure_close_wp = unit
type ('Aa,'Aq,'Awp,'Ap) pure_assert_p = unit
type ('Aa,'Aq,'Awp,'Ap) pure_assume_p = unit
type ('Aa,'Ap) pure_null_wp = unit
type ('Aa,'Awp) pure_trivial = 'Awp
type ('Aa,'Awp,'Auu____878) purewp_id = 'Awp
let mk_range (f: string) (a: int) (b: int) (c: int) (d: int): range = let r = (f, (a, b), (c, d)) in (r, r)
let range_0 : range = let z = 0L in mk_range "<dummy>"B z z z z

let op_AmpAmp (x: bool) (y: bool): bool = x && y
let op_BarBar (x: bool) (y: bool): bool = x || y
let op_Negation (x: bool): bool = not x

let ( + )     : int -> int -> int = Check.(+)
let ( - )     : int -> int -> int = Check.(-)
let ( * )     : int -> int -> int = Check.( * )
let ( / )     : int -> int -> int = ediv
let ( <= )    : int -> int -> bool = (<=)
let ( >= )    : int -> int -> bool = (>=)
let ( < )     : int -> int -> bool = (<)
let ( > )     : int -> int -> bool = (>)
let ( mod )   : int -> int -> int = erem
let ( ~- )    : int -> int        = Check.(~-)
let abs       : int -> int        = abs

let op_Star (x: int) (y: int): int = x * y
//let op_Subtraction (x: int) (y: int): int = x - y
//let op_Addition (x: int) (y: int): int = x + y
let op_Minus (x: int): int = -x
//let op_LessThan (x: int) (y: int): bool = x < y
//let op_LessThanOrEqual (x: int) (y: int): bool = x <= y
//let op_GreaterThan (x: int) (y: int): bool = x > y
//let op_GreaterThanOrEqual (x: int) (y: int): bool = x >= y
let op_Equality: 'A  -> 'A -> bool = (=)
let op_disEquality: 'A  -> 'A -> bool = (<>)

type exn = Core.exn
type 'a array' = 'a Core.array
type 'a array = 'a array'
//let strcat (x: string) (y: string): string = Collections.Array.append x y

type 'a list' = | Nil' | Cons' of int * 'a * 'a list'
type 'a list = 'a list'
let uu___is_Nil : 'Aa list -> bool =
  fun projectee  -> match projectee with | Nil'  -> true | _ -> false
let uu___is_Cons : 'Aa list -> bool =
  fun projectee  ->
    match projectee with | Cons' _ -> true | _ -> false

let __proj__Cons__item__hd : 'Aa list -> 'Aa =
  fun projectee  -> match projectee with | Cons'(_, hd, _) -> hd
let __proj__Cons__item__tl : 'Aa list -> 'Aa list =
  fun projectee  -> match projectee with | Cons'(_, _, tl) -> tl
type pattern = unit


type ('Aa,'Auu____1278) decreases = unit
let returnM : 'Aa -> 'Aa = fun x  -> x
type lex_t =
  | LexTop
  | LexCons of unit * Obj.t * lex_t
let (uu___is_LexTop : lex_t -> bool) =
  fun projectee  ->
    match projectee with | LexTop  -> true | uu____1313 -> false

let (uu___is_LexCons : lex_t -> bool) =
  fun projectee  ->
    match projectee with | LexCons (a,_1,_2) -> true | uu____1327 -> false

type 'Aprojectee __proj__LexCons__item__a = Obj.t
let (__proj__LexCons__item___1 : lex_t -> Obj.t) =
  fun projectee  -> match projectee with | LexCons (a,_1,_2) -> _1
let (__proj__LexCons__item___2 : lex_t -> lex_t) =
  fun projectee  -> match projectee with | LexCons (a,_1,_2) -> _2
type ('Aa,'Awp) as_requires = 'Awp
type ('Aa,'Awp,'Ax) as_ensures = unit
let admit (): 'Aa = failwith "Prims.admit: cannot be executed"
let magic (): 'Aa = failwith "Prims.magic: cannot be executed"
let unsafe_coerce : 'Aa -> 'Ab =
  fun x -> Obj.magic x

type 'Ap spinoff = 'Ap

type nat = int
type pos = int
type nonzero = int
let op_Modulus (x: int) (y: int): int = x % y
//let op_Division (x: int) (y: int): int = x / y
let rec pow2 : nat -> pos =
  fun x  ->
    match x with
    | 0L -> 1L
    | _ ->
        op_Multiply 2L
          (pow2 (op_Subtraction x 1L))

let (min : int -> int -> int) =
  fun x  -> fun y  -> if x <= y then x else y
//let (abs : int -> int) =
  //fun x  -> if x >= 0L then x else op_Minus x
let string_of_bool (b: bool): string = fsharpStringConvert (P.string_of_bool b)
let string_of_int (x: int): string = fsharpStringConvert (I64.to_string x)

type ('Ar,'Amsg,'Ab) labeled = 'Ab

(** ZF* specific definitions follow **)

let max (x: int) (y: int): int = System.Math.Max(x, y)

// Lists with constant-time length lookup
let Nil = Nil'
let Cons: 'A * 'A list -> 'A list = function
    | hd, (Cons' (i, _, _) as tl) -> Cons' (i+1L, hd, tl)
    | hd, Nil' -> Cons' (1L, hd, Nil)
let (|Cons|Nil|) = function
    | Nil' -> Nil
    | Cons' (_, hd, tl) -> Cons (hd, tl)
let length: 'a list -> int = function
    | Nil' -> 0L
    | Cons' (i, _, _) -> i

let strlen (s:string) : nat = int64 (s.Length)
