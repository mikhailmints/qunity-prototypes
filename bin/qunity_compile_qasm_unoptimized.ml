open Qunity
open Parsing
open Util
open Reals
open Typechecking_old
open Gate
open Compilation_unoptimized

let simple_gate_to_qasm_str (u : gate) : string * int list =
  match u with
  | Identity -> failwith "Identities should be removed"
  | U3Gate (i, theta, phi, lambda) ->
      if gate_equal u (gate_paulix i) then
        ("x", [i])
      else if gate_equal u (gate_pauliz i) then
        ("z", [i])
      else if gate_equal u (gate_had i) then
        ("h", [i])
      else
        ( Printf.sprintf "U(%.20f, %.20f, %.20f)" (float_of_real theta)
            (float_of_real phi) (float_of_real lambda),
          [i] )
  | GphaseGate theta ->
      (Printf.sprintf "gphase(%.20f)" (float_of_real theta), [])
  | Reset i -> ("reset", [i])
  | Swap (i, j) -> ("swap", [i; j])
  | _ -> failwith "Expected simple gate"

let rec gate_to_qasm_str (u : gate) (err_num : int) : string * int =
  let arglist_fold cur x =
    if cur = "" then
      Printf.sprintf " q[%d]" x
    else
      cur ^ Printf.sprintf ", q[%d]" x
  in
    match u with
    | Controlled (l, bl, u0) -> begin
        match u0 with
        | Sequence _ -> failwith "Controls should be distributed"
        | Controlled _ -> failwith "Controls should be combined"
        | Annotation _
        | PotentialDeletionLabel _
        | ZeroStateLabel _ ->
            ("", err_num)
        | _ -> begin
            let l, bl, (s, l0), apply_x =
              begin
                match u0 with
                | GphaseGate theta -> begin
                    ( List.tl l,
                      List.tl bl,
                      ( (if real_equal theta Pi then
                           "z"
                         else
                           Printf.sprintf "p(%.20f)" (float_of_real theta)),
                        [List.hd l] ),
                      if not (List.hd bl) then Some (List.hd l) else None )
                  end
                | _ -> (l, bl, simple_gate_to_qasm_str u0, None)
              end
            in
            let s =
              List.fold_left
                (fun cur bi ->
                  if bi then cur ^ "ctrl @ " else cur ^ "negctrl @ ")
                "" bl
              ^ s
              ^ List.fold_left arglist_fold "" (l @ l0)
              ^ ";\n"
            in
            let s =
              match apply_x with
              | Some i -> Printf.sprintf "x q[%d];\n%sx q[%d];\n" i s i
              | None -> s
            in
              (s, err_num)
          end
      end
    | Sequence (u0, u1) ->
        let s0, err_num = gate_to_qasm_str u0 err_num in
        let s1, err_num = gate_to_qasm_str u1 err_num in
          (s0 ^ s1, err_num)
    | Annotation (l, s) ->
        if l = [] then
          (Printf.sprintf "// %s\n" s, err_num)
        else
          ( Printf.sprintf "// %s %s\n" s (string_of_list string_of_int l),
            err_num )
    | MeasureAsErr i ->
        (Printf.sprintf "err[%d] = measure q[%d];\n" err_num i, err_num + 1)
    | PotentialDeletionLabel i ->
        (Printf.sprintf "// Potential deletion %d\n" i, err_num)
    | ZeroStateLabel i -> (Printf.sprintf "// Zero state %d\n" i, err_num)
    | _ ->
        let s, l = simple_gate_to_qasm_str u in
          ( Printf.sprintf "%s%s;\n" s (List.fold_left arglist_fold "" l),
            err_num )

let gate_to_qasm_file (u : gate) (nqubits : int) (out_reg : int list)
    (flag_reg : int list) : string =
  let n_out = List.length out_reg in
  let n_flag = List.length flag_reg in
  let header =
    Printf.sprintf
      "OPENQASM 3.0;\n\
       include \"stdgates.inc\";\n\
       qubit[%d] q;\n\
       bit[%d] out;\n\
       bit[%d] err;\n"
      nqubits n_out n_flag
  in
  let body, _ = if u = Identity then ("", 0) else gate_to_qasm_str u 0 in
  let footer =
    List.fold_left ( ^ ) ""
      (List.map
         (fun (i, j) -> Printf.sprintf "out[%d] = measure q[%d];\n" i j)
         (List.combine (range n_out) out_reg))
    ^ List.fold_left ( ^ ) ""
        (List.map
           (fun (i, j) -> Printf.sprintf "err[%d] = measure q[%d];\n" i j)
           (List.combine (range n_flag) flag_reg))
  in
    header ^ body ^ footer

let compile_file (prog_filename : string) (out_filename : string) : unit =
  Printf.printf "Preprocessing\n%!";
  let e_opt = get_expr_from_file prog_filename in
    match e_opt with
    | NoneE err ->
        Printf.printf "%s\n" err;
        exit 1
    | SomeE (e, _, _) -> begin
        Printf.printf "Typechecking\n%!";
        match mixed_type_check StringMap.empty e with
        | NoneE err ->
            Printf.printf "Typechecking error: %s\n" err;
            exit 1
        | SomeE _ -> begin
            Printf.printf "Compiling to QASM\n%!";
            let gate, nqubits, out_reg, flag_reg = expr_compile false e in
              Printf.printf "Outputting to file\n%!";
              let qasm_str = gate_to_qasm_file gate nqubits out_reg flag_reg in
              let out_file = open_out out_filename in
                Printf.fprintf out_file "%s" qasm_str;
                close_out out_file
          end
      end

let () =
  let prog_filename = Sys.argv.(1) in
  let out_filename = Sys.argv.(2) in
  let debug_mode = bool_of_string Sys.argv.(3) in
  let annotation_mode = bool_of_string Sys.argv.(4) in
  let post_optimize = bool_of_string Sys.argv.(5) in
    Gate.optimization_print := true;
    Metaprogramming.debug_mode := debug_mode;
    Compilation.debug_mode := debug_mode;
    Compilation.annotation_mode := annotation_mode;
    Compilation.post_optimize := post_optimize;
    compile_file prog_filename out_filename
