open Util
open Reals
open Syntax
open Typechecking

type comparison = Equal | Leq | Lt | Geq | Gt

type xexpr =
  | XNull
  | XVar of string
  | XQpair of (xexpr * xexpr)
  | XCtrl of (xexpr * xexpr * (xexpr * xexpr) list * xexpr * xexpr option)
  | XMatch of (xexpr * xexpr * (xexpr * xexpr) list * xexpr * xexpr option)
  | XPMatch of (xexpr * (xexpr * xexpr) list * xexpr * xexpr option)
  | XTry of (xexpr * xexpr)
  | XApply of (xexpr * xexpr)
  | XVoid
  | XQunit
  | XSumType of (xexpr * xexpr)
  | XProdType of (xexpr * xexpr)
  | XU3 of (xexpr * xexpr * xexpr)
  | XLeft of (xexpr * xexpr)
  | XRight of (xexpr * xexpr)
  | XLambda of (xexpr * xexpr * xexpr)
  | XRphase of (xexpr * xexpr * xexpr * xexpr)
  | XReal of xexpr
  | XInvoke of string * xexpr list
  | XIfcmp of (xexpr * comparison * xexpr * xexpr * xexpr)
  | XPi
  | XEuler
  | XConst of int
  | XRealVar of string
  | XNegate of xexpr
  | XPlus of (xexpr * xexpr)
  | XTimes of (xexpr * xexpr)
  | XDiv of (xexpr * xexpr)
  | XPow of (xexpr * xexpr)
  | XMod of (xexpr * xexpr)
  | XSin of xexpr
  | XCos of xexpr
  | XTan of xexpr
  | XArcsin of xexpr
  | XArccos of xexpr
  | XArctan of xexpr
  | XExp of xexpr
  | XLn of xexpr
  | XLog2 of xexpr
  | XSqrt of xexpr
  | XCeil of xexpr
  | XFloor of xexpr
  | XFail

and xresult =
  | RReal of real
  | RType of exprtype
  | RProg of prog
  | RExpr of expr
  | RNone of string

and definition = string list * xexpr
and defmap = definition StringMap.t
and xvaluation = xresult StringMap.t

type qunityfile = { dm : defmap; main : xexpr option }

let rec realexpr_eval (r : xexpr) (dm : defmap) (xv : xvaluation) : real =
  match r with
  | XPi -> Pi
  | XEuler -> Euler
  | XConst x -> Const x
  | XRealVar x -> begin
      match StringMap.find_opt x xv with
      | Some value -> begin
          match value with
          | RReal r -> r
          | RNone err -> failwith err
          | _ -> failwith "Expected real"
        end
      | _ -> begin
          match xexpr_eval (XInvoke (x, [])) dm xv with
          | RReal r -> r
          | _ -> failwith (Printf.sprintf "Value %s not found" x)
        end
    end
  | XNegate r0 -> Negate (realexpr_eval r0 dm xv)
  | XPlus (r0, r1) -> Plus (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XTimes (r0, r1) -> Times (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XDiv (r0, r1) -> Div (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XPow (r0, r1) -> Pow (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XMod (r0, r1) -> Mod (realexpr_eval r0 dm xv, realexpr_eval r1 dm xv)
  | XSin r0 -> Sin (realexpr_eval r0 dm xv)
  | XCos r0 -> Cos (realexpr_eval r0 dm xv)
  | XTan r0 -> Tan (realexpr_eval r0 dm xv)
  | XArcsin r0 -> Arcsin (realexpr_eval r0 dm xv)
  | XArccos r0 -> Arccos (realexpr_eval r0 dm xv)
  | XArctan r0 -> Arctan (realexpr_eval r0 dm xv)
  | XExp r0 -> Exp (realexpr_eval r0 dm xv)
  | XLn r0 -> Ln (realexpr_eval r0 dm xv)
  | XLog2 r0 -> Log2 (realexpr_eval r0 dm xv)
  | XSqrt r0 -> Sqrt (realexpr_eval r0 dm xv)
  | XCeil r0 -> Ceil (realexpr_eval r0 dm xv)
  | XFloor r0 -> Floor (realexpr_eval r0 dm xv)
  | _ -> failwith "Not a realexpr"

and xexpr_eval (v : xexpr) (dm : defmap) (xv : xvaluation) : xresult =
  let expand_xelse (t0 : exprtype) (xelseopt : xexpr option)
      (l : (expr * expr) list) : (expr * expr) list optionE =
    match xelseopt with
    | None -> SomeE l
    | Some xelse -> begin
        match missing_span t0 (List.map fst l) with
        | None -> NoneE "Ortho check failed when preprocessing else expression"
        | Some (mspan, _) -> begin
            match xexpr_eval xelse dm xv with
            | RNone err -> NoneE (err ^ "\nin Ctrl")
            | RExpr eelse -> SomeE (l @ List.map (fun e -> (e, eelse)) mspan)
            | _ -> NoneE "Expected expression"
          end
      end
  in
  let ctrl_eval (xe0 : xexpr) (xt0 : xexpr) (xl : (xexpr * xexpr) list)
      (xt1 : xexpr) (xelseopt : xexpr option) :
      (expr * exprtype * (expr * expr) list * exprtype) optionE =
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
    | RExpr e0, RType t0, Some l, RType t1 -> begin
        match expand_xelse t0 xelseopt l with
        | NoneE err -> NoneE err
        | SomeE l' -> SomeE (e0, t0, l', t1)
      end
    | RNone err, _, _, _
    | _, RNone err, _, _
    | _, _, _, RNone err ->
        NoneE err
    | _ -> NoneE "Preprocessing error in Ctrl"
  in
    match v with
    | XNull -> RExpr Null
    | XVar x -> RExpr (Var x)
    | XQpair (xe0, xe1) -> begin
        match (xexpr_eval xe0 dm xv, xexpr_eval xe1 dm xv) with
        | RExpr e0, RExpr e1 -> RExpr (Qpair (e0, e1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Qpair")
        | _, _ -> RNone "Expected expression"
      end
    | XCtrl (xe0, xt0, xl, xt1, xelseopt) -> begin
        match ctrl_eval xe0 xt0 xl xt1 xelseopt with
        | SomeE (e0, t0, l, t1) -> RExpr (Ctrl (e0, t0, l, t1))
        | NoneE err -> RNone err
      end
    | XMatch (xe0, xt0, xl, xt1, xelseopt) -> begin
        let xelseopt' =
          begin
            match xelseopt with
            | Some xelse -> Some (XQpair (xe0, xelse))
            | None -> None
          end
        in
        let xl' = List.map (fun (xej, xej') -> (xej, XQpair (xe0, xej'))) xl in
          xexpr_eval
            (XApply
               ( XLambda
                   ( XQpair (XVar "$x0", XVar "$x1"),
                     XProdType (xt0, xt1),
                     XVar "$x1" ),
                 XCtrl (xe0, xt0, xl', XProdType (xt0, xt1), xelseopt') ))
            dm xv
      end
    | XPMatch (xt0, xl, xt1, xelseopt) -> begin
        match ctrl_eval (XVar "$x") xt0 xl xt1 xelseopt with
        | SomeE (_, t0, l, t1) -> begin
            let l0 =
              List.map (fun (ej, ej') -> (ej, Qpair (Var "$x", ej'))) l
            in
            let l1 =
              List.map (fun (ej, ej') -> (ej', Qpair (ej, Var "$y"))) l
            in
            let ctrl0 = Ctrl (Var "$x", t0, l0, ProdType (t0, t1)) in
            let ctrl1 = Ctrl (Var "$y", t1, l1, ProdType (t0, t1)) in
            let spec_erasure = Lambda (ctrl1, ProdType (t0, t1), Var "$y") in
              RProg (Lambda (Var "$x", t0, Apply (spec_erasure, ctrl0)))
          end
        | NoneE err -> RNone err
      end
    | XTry (xe0, xe1) -> begin
        match (xexpr_eval xe0 dm xv, xexpr_eval xe1 dm xv) with
        | RExpr e0, RExpr e1 -> RExpr (Try (e0, e1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Try")
        | _, _ -> RNone "Expected expression in Try"
      end
    | XApply (xf, xe') -> begin
        match (xexpr_eval xf dm xv, xexpr_eval xe' dm xv) with
        | RProg f, RExpr e' -> RExpr (Apply (f, e'))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Apply")
        | _ -> RNone "Expected expression in Apply"
      end
    | XVoid -> RType Void
    | XQunit -> RType Qunit
    | XSumType (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RType (SumType (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin SumType")
        | _ -> RNone "Expected type in SumType"
      end
    | XProdType (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RType (ProdType (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin ProdType")
        | _ -> RNone "Expected type in ProdType"
      end
    | XU3 (theta, phi, lambda) -> begin
        try
          RProg
            (U3
               ( realexpr_eval theta dm xv,
                 realexpr_eval phi dm xv,
                 realexpr_eval lambda dm xv ))
        with
        | Failure err -> RNone err
      end
    | XLeft (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RProg (Left (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Left")
        | _ -> RNone "Expected program in Left"
      end
    | XRight (xt0, xt1) -> begin
        match (xexpr_eval xt0 dm xv, xexpr_eval xt1 dm xv) with
        | RType t0, RType t1 -> RProg (Right (t0, t1))
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Right")
        | _ -> RNone "Expected program in Right"
      end
    | XLambda (xe, xt, xe') -> begin
        match
          (xexpr_eval xe dm xv, xexpr_eval xt dm xv, xexpr_eval xe' dm xv)
        with
        | RExpr e, RType t, RExpr e' -> RProg (Lambda (e, t, e'))
        | RNone err, _, _
        | _, RNone err, _
        | _, _, RNone err ->
            RNone (err ^ "\nin Lambda")
        | _ -> RNone "Expected program in Lambda"
      end
    | XRphase (xt, xer, r0, r1) -> begin
        match (xexpr_eval xt dm xv, xexpr_eval xer dm xv) with
        | RType t, RExpr er -> begin
            try
              RProg
                (Rphase (t, er, realexpr_eval r0 dm xv, realexpr_eval r1 dm xv))
            with
            | Failure err -> RNone err
          end
        | RNone err, _
        | _, RNone err ->
            RNone (err ^ "\nin Rphase")
        | _ -> RNone "Expected program in Rphase"
      end
    | XReal r -> begin
        try RReal (realexpr_eval r dm xv) with
        | Failure err -> RNone err
      end
    | XInvoke (s, l) -> begin
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
                      xexpr_eval body dm xv'
              end
          end
      end
    | XIfcmp (v0, cmp, v1, vtrue, vfalse) -> begin
        let branch =
          match (xexpr_eval v0 dm xv, cmp, xexpr_eval v1 dm xv) with
          | RReal r0, Equal, RReal r1 -> SomeE (real_equal r0 r1)
          | RReal r0, Leq, RReal r1 -> SomeE (real_le r0 r1)
          | RReal r0, Lt, RReal r1 -> SomeE (real_lt r0 r1)
          | RReal r0, Geq, RReal r1 -> SomeE (real_ge r0 r1)
          | RReal r0, Gt, RReal r1 -> SomeE (real_gt r0 r1)
          | RType t0, Equal, RType t1 -> SomeE (t0 = t1)
          | RProg f0, Equal, RProg f1 -> SomeE (f0 = f1)
          | RExpr e0, Equal, RExpr e1 -> SomeE (e0 = e1)
          | RNone err, _, _
          | _, _, RNone err ->
              NoneE (err ^ "\nin Ifcmp")
          | _ -> NoneE "Inconsistent types in Ifcmp"
        in
          match branch with
          | SomeE true -> xexpr_eval vtrue dm xv
          | SomeE false -> xexpr_eval vfalse dm xv
          | NoneE err -> RNone err
      end
    | XFail -> RNone "Failure triggered"
    | _ -> (
        try RReal (realexpr_eval v dm xv) with
        | Failure err -> RNone err)

let add_defmap (dm : defmap) (dm_new : defmap) : defmap =
  StringMap.union (fun _ _ x -> Some x) dm dm_new

let add_def (name : string) (d : definition) (qf : qunityfile) : qunityfile =
  if StringMap.find_opt name qf.dm <> None then
    (* When a program is parsed, definitions are added in reverse order *)
    qf
  else
    { dm = StringMap.add name d qf.dm; main = qf.main }
