open Qunity
open Syntax
open Typechecking
open Semantics
open Compilation
open Matrix
open Gate

let all_passed = ref true

let rec expr_to_encoded_basis_state (e : expr) : matrix =
  let t = type_of_pure_expr_proof (pure_type_check_noopt e) in
    match (e, t) with
    | Null, Qunit -> mat_identity 1
    | Apply (Left (t0, t1), e0), SumType (t0', t1') when t0 = t0' && t1 = t1'
      ->
        let rest_size = max (type_size t0) (type_size t1) - type_size t0 in
          mat_tensor (basis_column_vec 2 0)
            (mat_tensor
               (expr_to_encoded_basis_state e0)
               (basis_column_vec (1 lsl rest_size) 0))
    | Apply (Right (t0, t1), e1), SumType (t0', t1') when t0 = t0' && t1 = t1'
      ->
        let rest_size = max (type_size t0) (type_size t1) - type_size t1 in
          mat_tensor (basis_column_vec 2 1)
            (mat_tensor
               (expr_to_encoded_basis_state e1)
               (basis_column_vec (1 lsl rest_size) 0))
    | Qpair (e1, e2), ProdType (_, _) ->
        mat_tensor
          (expr_to_encoded_basis_state e1)
          (expr_to_encoded_basis_state e2)
    | _, _ -> failwith "Type mismatch"

let test_compilation_correctness (testname : string) (e : expr) =
  Printf.printf "%s: " testname;
  try
    let t = type_of_mixed_expr_proof (mixed_type_check_noopt e) in
    let gate, nqubits, out_reg = expr_compile false e in
    let gate_sem = gate_semantics gate nqubits out_reg in
    let sem = top_mixed_expr_semantics e in
      if
        List.for_all
          (fun be ->
            let bs = expr_to_basis_state t be in
            let enc_bs = expr_to_encoded_basis_state be in
              mat_approx_equal
                (mat_adjoint bs *@ sem *@ bs)
                (mat_adjoint enc_bs *@ gate_sem *@ enc_bs))
          (Array.to_list (all_basis_exprs t))
      then
        Printf.printf "passed\n"
      else begin
        Printf.printf "FAILED\n";
        Printf.printf "sem matrix:\n";
        print_mat sem;
        Printf.printf "gate_sem matrix:\n";
        print_mat gate_sem;
        all_passed := false
      end
  with
  | Failure err
  | Invalid_argument err -> begin
      Printf.printf "FAILED\nWith error: %s\n" err;
      all_passed := false
    end

let const0 = Lambda (Var "x", bit, bit0)
let const1 = Lambda (Var "x", bit, bit1)

let () =
  begin
    Printf.printf
      "=========================\n\
       RUNNING COMPILATION TESTS\n\
       =========================\n";

    test_compilation_correctness "compile_null" Null;

    test_compilation_correctness "compile_bit0" bit0;
    test_compilation_correctness "compile_bit1" bit1;

    test_compilation_correctness "compile_had0" (Apply (had, bit0));
    test_compilation_correctness "compile_had1" (Apply (had, bit1));

    test_compilation_correctness "compile_bit00" (Qpair (bit0, bit0));
    test_compilation_correctness "compile_bit01" (Qpair (bit0, bit1));
    test_compilation_correctness "compile_bit10" (Qpair (bit1, bit0));
    test_compilation_correctness "compile_bit11" (Qpair (bit1, bit1));

    test_compilation_correctness "compile_const0_app0" (Apply (const0, bit0));
    test_compilation_correctness "compile_const0_app1" (Apply (const0, bit1));
    test_compilation_correctness "compile_const1_app0" (Apply (const1, bit0));
    test_compilation_correctness "compile_const1_app1" (Apply (const1, bit1));

    test_compilation_correctness "compile_equal_superpos"
      (Qpair (bitplus, Qpair (bitplus, Qpair (bitplus, Qpair (bitplus, Null)))));

    test_compilation_correctness "compile_trycatch_example"
      (Try (Apply (Lambda (bit0, bit, bit1), bitplus), bitminus));

    test_compilation_correctness "compile_share_discard"
      (Apply
         ( Lambda
             ( Var "x",
               bit,
               Apply
                 ( Lambda
                     (Qpair (Var "x0", Var "x1"), ProdType (bit, bit), Var "x0"),
                   Qpair (Var "x", Var "x") ) ),
           bit0 ));

    if !all_passed then
      Printf.printf "\nALL COMPILATION TESTS PASSED\n\n"
    else
      Printf.printf "\nSOME COMPILATION TESTS FAILED\n\n"
  end
