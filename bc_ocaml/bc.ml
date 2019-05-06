open Core

exception NoSuchOpException
exception NoSuchFunctionException
exception NameConflictException
exception NoEnvironmentException
exception ArgumentMismatchException

type expr = 
	| Num of float
	| Var of string
	| Op1 of string*expr
	| Op2 of string*expr*expr
	| Fct of string * expr list

type statement = 
	| Assign of string*expr
	| Return of expr
	| Break
	| Continue
	| Expr of expr
	| If of expr*statement list * statement list
	| While of expr*statement list
	| For of statement*expr*statement*statement list
	| FctDef of string * string list * statement list
	| NoOp

type signal =
	| SigOK
	| SigReturn of float
	| SigBreak
	| SigContinue

type block = statement list

type varT =
	| VarValue of float
	| FctValue of string list * block

type env = (Core.String.t, varT) Core.Hashtbl.t

type envQueue = env list

let rec varEval (_v: string) (_q: envQueue): float =
	match _q with
		| [] -> 0.0
		| hd :: tl ->
		let opt = Hashtbl.find hd _v in
		(match opt with
			| Some v ->
				(match v with
					| VarValue f -> f
					| _ -> raise NameConflictException
				)
			| None -> (varEval _v tl)
		)
let varStore (varName: string) (varValue: float) (_q: envQueue): unit =
	let putAt (q: envQueue): unit =
		match q with
			| [] -> raise NoEnvironmentException
			| hd :: _ -> (Hashtbl.set hd ~key:varName ~data:(VarValue varValue))
	in
	let rec findVar (q: envQueue): bool  =
		match q with
			| [] -> false
			| hd :: tl ->
			let opt = Hashtbl.find hd varName in
			(match opt with
				| Some _ -> (Hashtbl.set hd ~key:varName ~data:(VarValue varValue)); true
				| None -> findVar tl
			)
	in let added = findVar _q in
	if (not added) then putAt _q
	(*
	let getFirst (q: envQueue): env =
		match q with
			| [] -> raise NoEnvironmentException
			| hd :: _ -> hd
	in
	let rec getLast (q: envQueue): env =
		match q with
			| [] -> raise NoEnvironmentException
			| [item] -> item
			| _ :: tl -> getLast tl
	in let first = getFirst _q
	in let last = getLast _q
	in if (Hashtbl.exists first ~f:(fun (x: string) -> x == varName))
	*)

let rec getFuncByName (fname: string) (_q: envQueue) =
	match _q with
		| [] -> raise NoSuchFunctionException
		| hd :: tl ->
		let opt = Hashtbl.find hd fname in
		(match opt with
			| Some v -> v
			| None -> (getFuncByName fname tl)
		)

let rec evalCode (_code: block) (_q:envQueue): signal = 
	(* create new environment *)
	let rec myFold (stmtList: block) (newQ: envQueue): signal =
		match stmtList with
			| [] -> SigOK
			| hd :: tl -> let sg = evalStatement hd newQ in
				match sg with
					| SigOK -> myFold tl newQ
					| SigReturn ret -> SigReturn ret
					| SigBreak -> SigBreak
					| SigContinue -> SigContinue
	in
	let newEnv = Hashtbl.create (module(String)) in
	let newQ = newEnv :: _q in
	myFold _code newQ;

and evalExpr (_e: expr) (_q:envQueue): float =
	let evalFunction (fname: string) (actualParam: expr list) (_q: envQueue): float =
		let symbol = getFuncByName fname _q in
		let (formalParam, code) =
		match symbol with
			| FctValue (a, b) -> (a, b)
			| _ -> raise NameConflictException
		in
		let countItems list = List.fold_left list ~init:0 ~f:(fun x _ -> x + 1) in
		let () = if ((countItems formalParam) <> (countItems actualParam)) then
			raise ArgumentMismatchException
		in
		let newEnv = Hashtbl.create (module(String)) in
		let addParam (paramName: string) (paramExpr: expr): unit =
			Hashtbl.set newEnv ~key:paramName ~data:(VarValue (evalExpr paramExpr _q))
		in
		let () = List.iter2_exn formalParam actualParam ~f:addParam in
		let newQ = newEnv :: _q
		in match (evalCode code newQ) with
			| SigReturn ret -> ret
			| _ -> 0.0
		
	in
	let coerce (b: bool) : float = if b then 1.0 else 0.0 in
	let asBool (f: float) : bool = f <> 0. in
	let addToVar (varName: string) (amt: float): unit =
		let _ = evalStatement (Assign (varName, (Op2 ("+", (Var varName), (Num amt))))) _q
		in ()
	in
	match _e with 
		| Num n -> n
		| Var v -> varEval v _q
		| Op1 (op, e) ->
			let varName =
			(match e with
				| Var v -> v
				| _ -> ""
			)
			in
			let v = evalExpr e _q in
			(match op with
				| "++a" -> (addToVar varName 1.); v +. 1.
				| "--a" -> (addToVar varName (-1.)); v -. 1.
				| "a++" -> (addToVar varName 1.); v
				| "a--" -> (addToVar varName (-1.)); v
				| "-" -> v *. (-1.)
				| "!" -> (not (asBool v)) |> coerce
				| _ -> raise NoSuchOpException
			)
		| Op2 (op, e0, e1) ->
			let lhs = evalExpr e0 _q in
			let rhs = evalExpr e1 _q in
			(match op with
				| "+" -> lhs +. rhs
				| "-" -> lhs -. rhs
				| "*" -> lhs *. rhs
				| "/" -> lhs /. rhs
				| "<" -> coerce (lhs < rhs)
				| ">" -> coerce (lhs > rhs)
				| "<=" -> coerce (lhs <= rhs)
				| ">=" -> coerce (lhs >= rhs)
				| "!=" -> coerce (lhs <> rhs)
				| "==" -> coerce (lhs = rhs)
				| "&&" -> ((asBool lhs) && (asBool rhs)) |> coerce
				| "||" -> ((asBool lhs) || (asBool rhs)) |> coerce
				| _ -> raise NoSuchOpException
			)
		| Fct (fname, exprList) -> (evalFunction fname exprList _q)

and evalStatement (s: statement) (q: envQueue): signal =
	let rec evalWithPrint (s: statement) (q: envQueue) (shouldPrint: bool): signal =
	match s with
		| NoOp -> SigOK
		| Assign (varName, _e) -> (varStore varName (evalExpr _e q) q); SigOK
		| If (_e, codeT, codeF) -> 
			let cond = evalExpr _e q in
			if (cond <> 0.0) then
				evalCode codeT q
			else
				evalCode codeF q
		| Return _e ->
			let exprValue = evalExpr _e q in
			SigReturn exprValue
		| Break -> SigBreak
		| Continue -> SigContinue
		| Expr _e ->
			let exprValue = evalExpr _e q in
			if (shouldPrint) then
				printf "%F\n" exprValue;
			SigOK
		| While (_e, code) ->
			let rec runner (): signal = 
			let cond = evalExpr _e q in
			if (cond <> 0.0) then (
				let sg = evalCode code q in
					match sg with
						| SigOK -> runner ()
						| SigReturn ret -> SigReturn ret
						| SigBreak -> SigOK
						| SigContinue -> runner()
			) else SigOK
			in
			runner ()
		| For (_sInit, _e, _sInc, code) ->
			let rec runner (): signal =
			let cond = evalExpr _e q in
			if (cond <> 0.0) then (
				let sg = evalCode code q in
					match sg with
						| SigOK -> let _ = (evalWithPrint _sInc q false) in runner ()
						| SigReturn ret -> SigReturn ret
						| SigBreak -> SigOK
						| SigContinue -> let _ = (evalWithPrint _sInc q false) in runner()
			) else SigOK
			in
			let _ = evalWithPrint _sInit q false in
			runner ()
		| FctDef (fctName, formalArgs, code) ->
			(match q with
				| [] -> raise NoEnvironmentException
				| hd :: _ -> (Hashtbl.set hd ~key:fctName ~data:(FctValue (formalArgs, code)))
			);
			SigOK
	in (evalWithPrint s q true)

and runCode (_code: block) (_q:envQueue): unit =
	let _ = evalCode _code _q in ()


(*** TESTS ***)
(*** TESTS ***)
(*** TESTS ***)
		
(* Test expressions *)
let%expect_test "evalNum" = 
	evalExpr (Num 10.0) [] |>
	printf "%F";
	[%expect {| 10. |}]

let%test _ = evalExpr (Num (-2.)) [] = -2.
let%test _ = evalExpr (Op1 ("-", (Num 2.))) [] = -2.
let%test _ = evalExpr (Op1 ("!", (Num 2.))) [] = 0.
let%test _ = evalExpr (Op1 ("!", (Num 2.))) [] = 0.
let%test _ = evalExpr (Op2 ("/", (Op2 ("+", (Num 2.), (Num 3.))), (Num 0.5))) [] = 10.
let%test _ = evalExpr (Op2 (">", (Num (-5.)), (Num 5.))) [] = 0.
let%test _ = evalExpr (Op2 ("<", (Num (-5.)), (Num 5.))) [] = 1.
let%test _ = evalExpr (Op2 ("==", (Num (-5.)), (Num 5.))) [] = 0.
let%test _ = evalExpr (Op2 ("!=", (Num (-5.)), (Num 5.))) [] = 1.
let%test _ = evalExpr (Op2 ("&&", (Num 1.), (Num 0.))) [] = 0.
let%test _ = evalExpr (Op2 ("||", (Num 1.), (Num 0.))) [] = 1.

(* 
	v = 10; 
	v // display v
 *)
let p1: block = [
		Assign("v", Num(1.0));
		Expr(Var("v")) 
]

let%expect_test "p1" =
	runCode p1 []; 
	[%expect {| 1. |}]

let varyingP0 (inital: float): block = [
	Assign("v", Num(inital));
	If(
		Op2(">", Var("v"), Num(10.0)), 
		[Assign("v", Op2("+", Var("v"), Num(1.0)))], 
		[]
	);
	Expr(Var("v"))
]


let%expect_test "p4" =
	runCode (varyingP0 2.) []; 
	[%expect {| 2. |}]

let%expect_test "p4" =
	runCode (varyingP0 50.) []; 
	[%expect {| 51. |}]

(*
	v = 1.0;
	if (v>10.0) then
		v = v + 1.0
	else
		for(i=2.0; i<10.0; i++) {
			v = v * i
		}
	v   // display v
*)
let p2: block = [
	Assign("v", Num(1.0));
	If(
		Op2(">", Var("v"), Num(10.0)), 
		[Assign("v", Op2("+", Var("v"), Num(1.0)))], 
		[For(
			Assign("i", Num(2.0)),
			Op2("<", Var("i"), Num(10.0)),
			Expr(Op1("++a", Var("i"))),
			[
				Assign("v", Op2("*", Var("v"), Var("i")))
			]
		)]
	);
	Expr(Var("v"))
]

let%expect_test "p2" =
	runCode p2 []; 
	[%expect {| 362880. |}]

(* simple function
define f(x, y) {
	return x * 5 + y
}
f(1, 2)
f(3, 4)
f(-5, 5)
*)

let pSimpleFunction: block =
	[
		FctDef("f", ["x"; "y"], [
			Return(Op2("+", Op2("*", Var("x"), Num(5.)), Var("y")))
		]);
		Expr(Fct("f", [Num(1.); Num(2.)]));
		Expr(Fct("f", [Num(3.); Num(4.)]));
		Expr(Fct("f", [Num(-5.); Num(5.)]));
	]

let%expect_test "pSimpleFunction" =
	runCode pSimpleFunction []; 
	[%expect {| 
		7.
		19.
		-20.
	|}]



(*  Fibbonaci sequence
define f(x) {
	if (x < 1) {
		return 1
	} else {
		return f(x - 2)	+ f(x - 1)
	}
}
f(3)
f(5)
f(8)
f(9)
*)
let p3: block = 
	[
		FctDef("f", ["x"], [
			If(
				Op2("<", Var("x"), Num(1.0)),
				[Return(Num(1.0))],
				[Return(Op2("+",
					Fct("f", [Op2("-", Var("x"), Num(1.0))]),
					Fct("f", [Op2("-", Var("x"), Num(2.0))])
				))])
		]);
		Expr(Fct("f", [Num(3.)]));
		Expr(Fct("f", [Num(5.)]));
		Expr(Fct("f", [Num(8.)]));
		Expr(Fct("f", [Num(9.)]));
	]

let%expect_test "p3" =
	runCode p3 []; 
	[%expect {| 
		5.
		13.
		55.
		89.
	|}]


(*  factorial
define f(x) {
	if (x <= 1) {
		return 1.0
	} else {
		return f(x - 1)	* x
	}
}
f(3)
f(5)
f(8)
*)
let p5: block = 
	[
		FctDef("f", ["x"], [
			If(
				Op2("<=", Var("x"), Num(1.0)),
				[Return(Num(1.0))],
				[Return(Op2("*",
					Fct("f", [Op2("-", Var("x"), Num(1.0))]),
					Var("x")
				))])
		]);
		Expr(Fct("f", [Num(3.)]));
		Expr(Fct("f", [Num(5.)]));
		Expr(Fct("f", [Num(8.)]));
	]

let%expect_test "p5" =
	runCode p5 []; 
	[%expect {|
		6.
		120.
		40320.
	|}]


(* break statements
/* this should not print 500 numbers */
for (i = 0; i < 500; ++i) {
	if (i == 8)
		break
	i
}
*)

let p6: block = [
	For(
		Assign("i", Num(0.)),
		Op2("<", Var("i"), Num(500.)),
		Expr(Op1("++a", Var("i"))),
		[
			If(
				Op2("==", Var("i"), Num(8.)),
				[Break],
				[]
			);
			Expr(Var("i"))
		]
	)
]

let%expect_test "p6" =
	runCode p6 []; 
	[%expect {|
		0.
		1.
		2.
		3.
		4.
		5.
		6.
		7.
	|}]

(* continue statements
for (i = 0; i < 10; ++i) {
	if (i < 5)
		continue
	i
}
*)

let p7: block = [
	For(
		Assign("i", Num(0.)),
		Op2("<", Var("i"), Num(10.)),
		Expr(Op1("++a", Var("i"))),
		[
			If(
				Op2("<", Var("i"), Num(5.)),
				[Continue],
				[]
			);
			Expr(Var("i"))
		]
	)
]

let%expect_test "p7" =
	runCode p7 []; 
	[%expect {|
		5.
		6.
		7.
		8.
		9.
	|}]

