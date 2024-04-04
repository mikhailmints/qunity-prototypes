open Util
open Reals
open Syntax

type xexpr =
  | Null
  | Var of string
  | Qpair of (xexpr * xexpr)
  | Ctrl of (xexpr * xexpr * (xexpr * xexpr) list * xexpr)
  | Try of (xexpr * xexpr)
  | Apply of (xexpr * xexpr)
  | Void
  | Qunit
  | SumType of (xexpr * xexpr)
  | ProdType of (xexpr * xexpr)
  | U3 of (realexpr * realexpr * realexpr)
  | Left of (xexpr * xexpr)
  | Right of (xexpr * xexpr)
  | Lambda of (xexpr * xexpr * xexpr)
  | Gphase of (xexpr * realexpr)
  | XReal of realexpr
  | Invoke of string * xexpr list
  | Ifeq of (xexpr * xexpr * xexpr * xexpr)

and xresult =
  | RReal of real
  | RType of exprtype
  | RProg of prog
  | RExpr of expr
  | RNone of string

and definition = string list * xexpr
and defmap = definition StringMap.t
and xvaluation = xresult StringMap.t

type qunityfile = defmap * xexpr

let rec realexpr_eval (r : realexpr) (xv : xvaluation) : real =
  match r with
  | Pi -> Pi
  | Euler -> Euler
  | Const x -> Const x
  | Var x -> begin
      match StringMap.find_opt x xv with
      | Some value -> begin
          match value with
          | RReal r -> r
          | _ -> failwith "Expected real"
        end
      | _ -> failwith "Value not found"
    end
  | Negate r1 -> Negate (realexpr_eval r1 xv)
  | Plus (r1, r2) -> Plus (realexpr_eval r1 xv, realexpr_eval r2 xv)
  | Times (r1, r2) -> Times (realexpr_eval r1 xv, realexpr_eval r2 xv)
  | Div (r1, r2) -> Div (realexpr_eval r1 xv, realexpr_eval r2 xv)
  | Pow (r1, r2) -> Pow (realexpr_eval r1 xv, realexpr_eval r2 xv)
  | Sin r1 -> Sin (realexpr_eval r1 xv)
  | Cos r1 -> Cos (realexpr_eval r1 xv)
  | Tan r1 -> Tan (realexpr_eval r1 xv)
  | Arcsin r1 -> Arcsin (realexpr_eval r1 xv)
  | Arccos r1 -> Arccos (realexpr_eval r1 xv)
  | Arctan r1 -> Arctan (realexpr_eval r1 xv)
  | Exp r1 -> Exp (realexpr_eval r1 xv)
  | Ln r1 -> Ln (realexpr_eval r1 xv)
  | Sqrt r1 -> Sqrt (realexpr_eval r1 xv)

and xexpr_eval (v : xexpr) (dm : defmap) (xv : xvaluation) : xresult =
  match v with
  | Null -> RExpr Null
  | Var x -> RExpr (Var x)
  | Qpair (xe0, xe1) -> begin
      match (xexpr_eval xe0 dm xv, xexpr_eval xe1 dm xv) with
      | RExpr e0, RExpr e1 -> RExpr (Qpair (e0, e1))
      | RNone err, _
      | _, RNone err ->
          RNone (err ^ "\nin Qpair")
      | _, _ -> RNone "Expected expr"
    end
  | Ctrl (xe0, xt0, xl, xt1) -> begin
      match
        ( xexpr_eval xe0 dm xv,
          xexpr_eval xt0 dm xv,
          all_or_nothing
            (List.map
               (fun (xej, xej') ->
                 match (xexpr_eval xej dm xv, xexpr_eval xej' dm xv) with
                 | RExpr ej, RExpr ej' -> Some (ej, ej')
                 | _ -> None)
               xl),
          xexpr_eval xt1 dm xv )
      with
      | RExpr e0, RType t0, Some l, RType t1 -> RExpr (Ctrl (e0, t0, l, t1))
      | _ -> RNone "Preprocessing error in Ctrl"
    end
  | Try (xe0, xe1) -> begin
      match (xexpr_eval xe0 dm xv, xexpr_eval xe1 dm xv) with
      | RExpr e0, RExpr e1 -> RExpr (Try (e0, e1))
      | RNone err, _
      | _, RNone err ->
          RNone (err ^ "\nin Try")
      | _, _ -> RNone "Expected expr in Try"
    end
  | Apply (xf, xe') -> begin
      match (xexpr_eval xf dm xv, xexpr_eval xe' dm xv) with
      | RProg f, RExpr e' -> RExpr (Apply (f, e'))
      | RNone err, _
      | _, RNone err ->
          RNone (err ^ "\nin Apply")
      | _ -> RNone "Expected expr in Apply"
    end
  | Void -> RType Void
  | Qunit -> RType Qunit
  | SumType (xt0, xt1) -> begin
      match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
      | RType t0, RType t1 -> RType (SumType (t0, t1))
      | RNone err, _
      | _, RNone err ->
          RNone (err ^ "\nin SumType")
      | _ -> RNone "Expected type in SumType"
    end
  | ProdType (xt0, xt1) -> begin
      match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
      | RType t0, RType t1 -> RType (ProdType (t0, t1))
      | RNone err, _
      | _, RNone err ->
          RNone (err ^ "\nin ProdType")
      | _ -> RNone "Expected type in ProdType"
    end
  | U3 (theta, phi, lambda) ->
      RProg
        (U3
           ( realexpr_eval theta xv,
             realexpr_eval phi xv,
             realexpr_eval lambda xv ))
  | Left (xt0, xt1) -> begin
      match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
      | RType t0, RType t1 -> RProg (Left (t0, t1))
      | RNone err, _
      | _, RNone err ->
          RNone (err ^ "\nin Left")
      | _ -> RNone "Expected prog in Left"
    end
  | Right (xt0, xt1) -> begin
      match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
      | RType t0, RType t1 -> RProg (Right (t0, t1))
      | RNone err, _
      | _, RNone err ->
          RNone (err ^ "\nin Right")
      | _ -> RNone "Expected prog in Right"
    end
  | Lambda (xe, xt, xe') -> begin
      match
        (xexpr_eval xe dm xv, xexpr_eval xt dm xv, xexpr_eval xe' dm xv)
      with
      | RExpr e, RType t, RExpr e' -> RProg (Lambda (e, t, e'))
      | RNone err, _, _
      | _, RNone err, _
      | _, _, RNone err ->
          RNone (err ^ "\nin Lambda")
      | _ -> RNone "Expected prog in Lambda"
    end
  | Gphase (xt, r) -> begin
      match xexpr_eval xt dm xv with
      | RType t -> RProg (Gphase (t, realexpr_eval r xv))
      | RNone err -> RNone (err ^ "\nin Gphase")
      | _ -> RNone "Expected prog in Gphase"
    end
  | XReal r -> RReal (realexpr_eval r xv)
  | Invoke (s, l) -> begin
      match StringMap.find_opt s xv with
      | Some res ->
          if List.length l = 0 then
            res
          else
            RNone "No arguments expected for expression in Invoke"
      | None -> begin
          match StringMap.find_opt s dm with
          | None ->
              RNone (Printf.sprintf "Definition %s not found in Invoke" s)
          | Some (argnames, body) -> begin
              let l_result = List.map (fun x -> xexpr_eval x dm xv) l in
                if List.length l <> List.length argnames then
                  RNone "Incorrect number of arguments in Invoke"
                else
                  let xv' =
                    StringMap.of_seq
                      (List.to_seq (List.combine argnames l_result))
                  in
                  let xv'' = StringMap.union (fun _ _ v -> Some v) xv xv' in
                    xexpr_eval body dm xv''
            end
        end
    end
  | Ifeq (v0, v1, vtrue, vfalse) -> begin
      let branch =
        match (xexpr_eval v0 dm xv, xexpr_eval v1 dm xv) with
        | RReal r0, RReal r1 ->
            SomeE (float_approx_equal (float_of_real r0) (float_of_real r1))
        | RType t0, RType t1 -> SomeE (t0 = t1)
        | RProg f0, RProg f1 -> SomeE (f0 = f1)
        | RExpr e0, RExpr e1 -> SomeE (e0 = e1)
        | RNone err, _
        | _, RNone err ->
            NoneE (err ^ "\nin Ifeq")
        | _ -> NoneE "Inconsistent types in Ifeq"
      in
        match branch with
        | SomeE true -> xexpr_eval vtrue dm xv
        | SomeE false -> xexpr_eval vfalse dm xv
        | NoneE err -> RNone err
    end

let add_def (name : string) (d : definition) ((dm, main) : qunityfile) :
    qunityfile =
  if StringMap.find_opt name dm <> None then
    (dm, main)
  else
    (StringMap.add name d dm, main)

let preprocess ((dm, main) : qunityfile) : expr optionE =
  match xexpr_eval main dm StringMap.empty with
  | RExpr e -> SomeE e
  | RNone err -> NoneE err
  | _ -> NoneE "Expected expr in main"
