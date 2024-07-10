open Util
open Reals
open Syntax
open Typechecking
open Gate

let debug_mode = false
let annotation_mode = ref true

(*
A circuit is a wrapper around a gate that labels the qubits by their roles as
input qubits, output qubits, prep qubits (which are initialized in the |0>
state, flag qubigs (which are expected to be zero in the end if no error is
thrown), and garbage qubits, which are measured and discarded, unless the
circuit is instantiated with the reset_garb option set to false (as it is when
constructing) a purified version of the circuit.
*)
type circuit = {
  name : string;
  in_regs : int list list;
  prep_reg : int list;
  out_regs : int list list;
  flag_reg : int list;
  garb_reg : int list;
  gate : gate;
}

(*
A circuit specification wraps a function for building a circuit provided a
certain input register and a set of wires that were already used, as well as
an option that tells whether or not to reset the garbage register.
*)
type circuit_spec = {
  in_sizes : int list;
  out_sizes : int list;
  prep_size : int;
  flag_size : int;
  (* in_regs, used_wires, reset_garb -> (circuit, updated used_wires) *)
  circ_fun : int list list -> IntSet.t -> bool -> circuit * IntSet.t;
}

(*
A valuation for the intermediate representation maps strings (variable names)
to quantum registers that they represent.
*)
type inter_valuation = int list StringMap.t

(*
An operator in the intermediate representation is any one of the operators
detled in Appendix H.1 of Voichick et al, or a user-defined ILambda. The
implementation of the pre-defined operators is contained in this file. A
user-defined operator can apply a number of operators in sequence to some
registers, storing them as variables in its body.
*)
type inter_op =
  | IEmpty
  | IIdentity of exprtype (* a {T} -> a *)
  | IU3 of real * real * real
  | IRphase of exprtype * inter_op * real * real
  | ILeft of exprtype * exprtype (* a {T0} -> left{T0, T1} a *)
  | IRight of exprtype * exprtype (* a {T1} -> right{T0, T1} a *)
  | IPair of exprtype * exprtype (* a {T0}, b {T1} -> (a, b) {T0 * T1} *)
  | IShare of exprtype (* a {T} -> [a; a] *)
  | IContextShare of context
  | IAdjoint of inter_op
  | IDirsum of inter_op * inter_op
  | IAssoc of
      exprtype * exprtype * exprtype (* (T0 + T1) + T2 -> T0 + (T1 + T2) *)
  | IDistrLeft of
      exprtype * exprtype * exprtype (* T * (T0 + T1) -> T * T0 + T * T1 *)
  | IDistrRight of
      exprtype * exprtype * exprtype (* (T0 + T1) * T -> T0 * T + T1 * T *)
  | IMixedErr of inter_op
  | IPureErr of inter_op
  | IDiscard of exprtype
  | IContextDiscard of context
  | IPurify of inter_op
  | IContextPartition of
      context * StringSet.t (* context -> [things in set; things not in set] *)
  | IContextMerge of (context * context)
    (* merge contexts assumed to be disjoint *)
  | ILambda of ((string * int) list * inter_com list * (string * int) list)
(* Output sizes, arg names with sizes, body, return vars. *)

(*
A command in the intermediate representation consists of the output variable
names, the and operator, input variable names. The commands represent
assigning to the output variables the operator applied to the input variables,
and they also delete the input variables since the registers will overlap in
an arbitrary way in the reassignment.
*)
and inter_com = string list * inter_op * string list

(* Alias for ILambda. *)
let inter_lambda (arglist : (string * int) list) (body : inter_com list)
    (ret : (string * int) list) : inter_op =
  ILambda (arglist, body, ret)

(* Alias for construcing an inter_com for clarity. *)
let inter_letapp (target : string list) (op : inter_op) (args : string list) :
    inter_com =
  (target, op, args)

(* Add annotations describing the roles of the registers to a circuit. *)
let annotate_circuit (circ : circuit) : circuit =
  if circ.name = "init" then
    circ
  else
    {
      circ with
      gate =
        List.fold_left ( @& ) Identity
          (List.map
             (fun (reg, i) ->
               Annotation (reg, Printf.sprintf "%s in%d" circ.name i))
             (List.combine circ.in_regs (range (List.length circ.in_regs))))
        @& Annotation (circ.prep_reg, Printf.sprintf "%s prep" circ.name)
        @& circ.gate
        @& List.fold_left ( @& ) Identity
             (List.map
                (fun (reg, i) ->
                  Annotation (reg, Printf.sprintf "%s out%d" circ.name i))
                (List.combine circ.out_regs
                   (range (List.length circ.out_regs))))
        @& Annotation (circ.flag_reg, Printf.sprintf "%s flag" circ.name)
        @& Annotation (circ.garb_reg, Printf.sprintf "%s garb" circ.name);
    }

(*
Wrapper around calling a circuit_spec's circ_fun
*)
let build_circuit (cs : circuit_spec) (in_regs : int list list)
    (used_wires : IntSet.t) (reset_garb : bool) : circuit * IntSet.t =
  let circ, used_wires = cs.circ_fun in_regs used_wires reset_garb in
    if !annotation_mode then
      (annotate_circuit circ, used_wires)
    else
      (circ, used_wires)

let circuit_spec_find_unknown_sizes (in_sizes : int list)
    (out_sizes : int list)
    (circ_fun : int list list -> IntSet.t -> bool -> circuit * IntSet.t) :
    circuit_spec =
  let prep_size, flag_size =
    begin
      let in_regs, used_wires = fresh_int_lists IntSet.empty in_sizes in
      let temp_circ, _ = circ_fun in_regs used_wires true in
        (List.length temp_circ.prep_reg, List.length temp_circ.flag_reg)
    end
  in
    { in_sizes; out_sizes; prep_size; flag_size; circ_fun }

(*
To create a controlled version of something that is already provided as a
circuit, this should be used instead of just adding a Controlled to the
circuit's gate. This is because whatever permutation is applied to the input
qubits when rearranging them should also be controlled. This undoes the
permutation and redoes it under a control to ensure this.
*)
let controlled_circ (l : int list) (bl : bool list) (circ : circuit) : gate =
  let circ_in = List.concat circ.in_regs @ circ.prep_reg in
  let circ_out = List.concat circ.out_regs @ circ.flag_reg @ circ.garb_reg in
    Controlled (l, bl, circ.gate)
    @& gate_permute circ_out circ_in
    @& Controlled (l, bl, gate_permute circ_in circ_out)

(*
Number of qubits needed to represent a value of a given size.
*)
let rec type_size (t : exprtype) =
  match t with
  | Void
  | Qunit ->
      0
  | SumType (t0, t1) -> 1 + max (type_size t0) (type_size t1)
  | ProdType (t0, t1) -> type_size t0 + type_size t1

(*
Number of qubits needed to represent a set of values corresponding to
a certain context.
*)
let context_size (d : context) =
  List.fold_left ( + ) 0
    (List.map (fun (_, t) -> type_size t) (StringMap.bindings d))

(*
Utility function for error-checking when only one in_reg should be provided
to a circuit_spec.
*)
let expect_single_in_reg (name : string) (in_regs : int list list) (size : int)
    : int list =
  match in_regs with
  | [in_reg] when List.length in_reg = size -> in_reg
  | _ ->
      failwith
        (Printf.sprintf "%s: expected input lengths [%d], got %s" name size
           (string_of_list string_of_int (List.map List.length in_regs)))

let expect_sizes (name : string) (expected : int list) (regs : int list list) :
    unit =
  if List.map List.length regs <> expected then
    failwith
      (Printf.sprintf "%s: expected sizes %s but got %s" name
         (string_of_list string_of_int expected)
         (string_of_list string_of_int (List.map List.length regs)))

(*
A circuit that takes in nothing and outputs an empty register.
*)
let circuit_empty =
  {
    in_sizes = [];
    out_sizes = [0];
    prep_size = 0;
    flag_size = 0;
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_empty" [] in_regs;
          ( {
              name = "empty";
              in_regs;
              prep_reg = [];
              out_regs = [[]];
              flag_reg = [];
              garb_reg = [];
              gate = Identity;
            },
            used_wires )
      end;
  }

(*
The identity circuit operating on a given type.
*)
let circuit_identity (t : exprtype) : circuit_spec =
  {
    in_sizes = [type_size t];
    out_sizes = [type_size t];
    prep_size = 0;
    flag_size = 0;
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          ( {
              name = "identity";
              in_regs;
              prep_reg = [];
              out_regs = in_regs;
              flag_reg = [];
              garb_reg = [];
              gate = Identity;
            },
            used_wires )
      end;
  }

(*
Circuit corresponding to a single-qubit gate.
*)
let circuit_u3 (theta : real) (phi : real) (lambda : real) : circuit_spec =
  {
    in_sizes = [1];
    out_sizes = [1];
    prep_size = 0;
    flag_size = 0;
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          expect_sizes "circuit_u3" [1] in_regs;
          let bit = List.hd (List.hd in_regs) in
            ( {
                name = "u3";
                in_regs;
                prep_reg = [];
                out_regs = in_regs;
                flag_reg = [];
                garb_reg = [];
                gate = U3Gate (bit, theta, phi, lambda);
              },
              used_wires )
      end;
  }

(*
Circuit corresponding to a global phase gate.
*)
let circuit_gphase (t : exprtype) (theta : real) : circuit_spec =
  let size = type_size t in
    {
      in_sizes = [size];
      out_sizes = [size];
      prep_size = 0;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            expect_sizes "circuit_gphase" [size] in_regs;
            ( {
                name = "gphase";
                in_regs;
                prep_reg = [];
                out_regs = in_regs;
                flag_reg = [];
                garb_reg = [];
                gate = GphaseGate theta;
              },
              used_wires )
        end;
    }

(*
Lemma H.1
*)
let circuit_left (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  let in_size = type_size t0 in
  let out_size = type_size (SumType (t0, t1)) in
  let prep_size = out_size - in_size in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      prep_size;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg = expect_single_in_reg "circuit_left" in_regs in_size in
            let prep, used_wires = fresh_int_list used_wires prep_size in
              ( {
                  name = "left";
                  in_regs;
                  prep_reg = prep;
                  out_regs = [(List.hd prep :: in_reg) @ List.tl prep];
                  flag_reg = [];
                  garb_reg = [];
                  gate = Identity;
                },
                used_wires )
        end;
    }

(*
Lemma H.1
*)
let circuit_right (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  let in_size = type_size t1 in
  let out_size = type_size (SumType (t0, t1)) in
  let prep_size = out_size - in_size in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      prep_size;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg =
              expect_single_in_reg "circuit_right" in_regs in_size
            in
            let prep, used_wires = fresh_int_list used_wires prep_size in
              ( {
                  name = "right";
                  in_regs;
                  prep_reg = prep;
                  out_regs = [(List.hd prep :: in_reg) @ List.tl prep];
                  flag_reg = [];
                  garb_reg = [];
                  gate = gate_paulix (List.hd prep);
                },
                used_wires )
        end;
    }

(*
Circuit implementing the share gate, which copies classical data from the
input register. This is not quantum cloning (which is impossible), it will
only duplicate the input when it is in a basis state.
*)
let circuit_share (size : int) : circuit_spec =
  {
    in_sizes = [size];
    out_sizes = [size; size];
    prep_size = size;
    flag_size = 0;
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          let in_reg = expect_single_in_reg "circuit_share" in_regs size in
          let prep, used_wires = fresh_int_list used_wires size in
            ( {
                name = "share";
                in_regs;
                prep_reg = prep;
                out_regs = [in_reg; prep];
                flag_reg = [];
                garb_reg = [];
                gate = gate_share in_reg prep;
              },
              used_wires )
      end;
  }

(*
Circuit that takes in a value of type t0 and a value of type t1 and outputs
a value of type t0 * t1 (functionally this is just the identity circuit).
*)
let circuit_pair (t0 : exprtype) (t1 : exprtype) : circuit_spec =
  {
    in_sizes = [type_size t0; type_size t1];
    out_sizes = [type_size t0 + type_size t1];
    prep_size = 0;
    flag_size = 0;
    circ_fun =
      begin
        fun in_regs used_wires _ ->
          let in_reg0, in_reg1 =
            match in_regs with
            | [in_reg0; in_reg1]
              when List.length in_reg0 = type_size t0
                   && List.length in_reg1 = type_size t1 ->
                (in_reg0, in_reg1)
            | _ -> failwith "Invalid input regs"
          in
            ( {
                name = "pair";
                in_regs;
                prep_reg = [];
                out_regs = [in_reg0 @ in_reg1];
                flag_reg = [];
                garb_reg = [];
                gate = Identity;
              },
              used_wires )
      end;
  }

(*
Takes the adjoint of a circuit specification. To do this, we need to instantiate
the circuit with fresh wires and then do a rewiring using the provided input
registers.
*)
let circuit_adjoint (cs : circuit_spec) : circuit_spec =
  {
    in_sizes = cs.out_sizes;
    out_sizes = cs.in_sizes;
    prep_size = cs.flag_size;
    flag_size = cs.prep_size;
    circ_fun =
      begin
        fun in_regs used_wires reset_garb ->
          expect_sizes "circuit_adjoint" cs.out_sizes in_regs;
          let temp_regs, temp_used_wires =
            fresh_int_lists used_wires cs.in_sizes
          in
          let temp_used_wires = IntSet.union used_wires temp_used_wires in
          let circ, circ_used_wires =
            build_circuit cs temp_regs temp_used_wires reset_garb
          in
            if circ.garb_reg <> [] then
              failwith
                "Expected garbage register to be empty when taking adjoint";
            let rewiring =
              IntMap.of_seq
                (List.to_seq
                   (List.combine
                      (List.flatten circ.out_regs)
                      (List.flatten in_regs)))
            in
            let used_wires =
              IntSet.diff circ_used_wires
                (IntSet.of_list (List.flatten circ.out_regs))
            in
            let rewire_list =
              List.map (fun i -> int_map_find_or_keep i rewiring)
            in
              ( {
                  name = "adjoint";
                  in_regs;
                  prep_reg = rewire_list circ.flag_reg;
                  out_regs = List.map rewire_list temp_regs;
                  flag_reg = rewire_list circ.prep_reg;
                  garb_reg = [];
                  gate = gate_adjoint (gate_rewire circ.gate rewiring);
                },
                used_wires )
      end;
  }

(*
Circuit implementing the direct sum of two circuits.
*)
let circuit_dirsum (cs0 : circuit_spec) (cs1 : circuit_spec) : circuit_spec =
  let in_size0, in_size1, out_size0, out_size1 =
    match (cs0.in_sizes, cs1.in_sizes, cs0.out_sizes, cs1.out_sizes) with
    | [in_size0], [in_size1], [out_size0], [out_size1] ->
        (in_size0, in_size1, out_size0, out_size1)
    | _ -> assert false
  in
  let in_size = 1 + max in_size0 in_size1 in
  let out_size = 1 + max out_size0 out_size1 in
  let min_in_size = min in_size0 in_size1 in
  let diff_in_size = max in_size0 in_size1 - min_in_size in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      prep_size = failwith "TODO";
      flag_size = failwith "TODO";
      circ_fun =
        begin
          fun in_regs used_wires reset_garb ->
            let in_reg =
              expect_single_in_reg "circuit_dirsum" in_regs in_size
            in
            let ctrlbit = List.hd in_reg in
            let in_reg = List.tl in_reg in

            let in_reg_min, in_reg_diff = list_split_at_i in_reg min_in_size in
            let prep_min, used_wires = fresh_int_list used_wires in_size0 in
            let prep_diff, used_wires =
              fresh_int_list used_wires diff_in_size
            in
            let circ0, circ1, used_wires =
              if in_size0 <= in_size1 then
                let circ0, used_wires =
                  build_circuit cs0 [in_reg_min] used_wires reset_garb
                in
                let circ1, used_wires =
                  build_circuit cs1
                    [prep_min @ in_reg_diff]
                    used_wires reset_garb
                in
                  (circ0, circ1, used_wires)
              else
                let circ0, used_wires =
                  build_circuit cs0
                    [prep_min @ in_reg_diff]
                    used_wires reset_garb
                in
                let circ1, used_wires =
                  build_circuit cs1 [in_reg_min] used_wires reset_garb
                in
                  (circ0, circ1, used_wires)
            in
              if circ0.garb_reg <> [] || circ1.garb_reg <> [] then
                failwith
                  "Expected garbage registers to be empty when taking direct \
                   sum";
              let out_reg0, out_reg1 =
                match (circ0.out_regs, circ1.out_regs) with
                | [out_reg0], [out_reg1] -> (out_reg0, out_reg1)
                | _ -> assert false
              in
              let out_reg_min_in_min, out_reg_min_in_max, out_reg_diff =
                if out_size0 <= out_size1 then
                  let out_reg_min_in_min = out_reg0 in
                  let out_reg_min_in_max, out_reg_diff =
                    list_split_at_i out_reg1 out_size0
                  in
                    (out_reg_min_in_min, out_reg_min_in_max, out_reg_diff)
                else
                  let out_reg_min_in_min = out_reg1 in
                  let out_reg_min_in_max, out_reg_diff =
                    list_split_at_i out_reg0 out_size1
                  in
                    (out_reg_min_in_min, out_reg_min_in_max, out_reg_diff)
              in
                ( {
                    name = "dirsum";
                    in_regs;
                    prep_reg =
                      int_list_union prep_min
                        (int_list_union circ0.prep_reg
                           (int_list_union circ1.prep_reg prep_diff));
                    out_regs = [(ctrlbit :: out_reg_min_in_max) @ out_reg_diff];
                    flag_reg =
                      out_reg_min_in_min @ circ0.flag_reg @ circ1.flag_reg
                      @ prep_diff;
                    garb_reg = [];
                    gate =
                      Controlled
                        ( [ctrlbit],
                          [not (in_size0 <= in_size1)],
                          gate_swap_regs in_reg_diff prep_diff )
                      @& Controlled
                           ( [ctrlbit],
                             [in_size0 <= in_size1],
                             gate_swap_regs in_reg_min prep_min )
                      @& controlled_circ [ctrlbit] [false] circ0
                      @& controlled_circ [ctrlbit] [true] circ1
                      @& Controlled
                           ( [ctrlbit],
                             [not (out_size0 <= out_size1)],
                             gate_swap_regs out_reg_min_in_min
                               out_reg_min_in_max );
                  },
                  used_wires )
        end;
    }

(*
Associativity isomorphism in Lemma H.6. Takes (t0 + t1) + t2 and outputs
t0 + (t1 + t2).
*)
let circuit_assoc (t0 : exprtype) (t1 : exprtype) (t2 : exprtype) :
    circuit_spec =
  (* 1 + max(1 + max(s0, s1), s2) *)
  let in_size = type_size (SumType (SumType (t0, t1), t2)) in
  (* 1 + max(s0, 1 + max(s1, s2)) *)
  let out_size = type_size (SumType (t0, SumType (t1, t2))) in
  let total_size =
    2 + max (type_size t0) (max (type_size t1) (type_size t2))
  in
    {
      in_sizes = [in_size];
      out_sizes = [out_size];
      prep_size = total_size - in_size;
      flag_size = total_size - out_size;
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg =
              expect_single_in_reg "circuit_assoc" in_regs in_size
            in
            let signal_01v2 = List.hd in_reg in
            let signal_0v1 = List.hd (List.tl in_reg) in
            let rest_reg = List.tl (List.tl in_reg) in
            let prep_reg, used_wires =
              fresh_int_list used_wires (total_size - in_size)
            in
            let out_reg, flag_reg =
              list_split_at_i
                ((signal_0v1 :: signal_01v2 :: rest_reg) @ prep_reg)
                out_size
            in
              ( {
                  name = "assoc";
                  in_regs;
                  prep_reg;
                  out_regs = [out_reg];
                  flag_reg;
                  garb_reg = [];
                  gate =
                    Controlled
                      ( [signal_01v2],
                        [true],
                        gate_rshift ((signal_0v1 :: rest_reg) @ prep_reg) )
                    @& gate_cnot signal_01v2 signal_0v1
                    @& Controlled
                         ( [signal_0v1],
                           [false],
                           gate_lshift ((signal_01v2 :: rest_reg) @ prep_reg)
                         );
                },
                used_wires )
        end;
    }

(*
Left distributivity isomorphism in Lemma H.7. Takes t * (t0 + t1) and outputs
(t * t0) + (t * t1).
*)
let circuit_distr_left (t : exprtype) (t0 : exprtype) (t1 : exprtype) :
    circuit_spec =
  let in_sizes = [type_size t; type_size (SumType (t0, t1))] in
  let out_size = type_size (SumType (ProdType (t, t0), ProdType (t, t1))) in
    {
      in_sizes;
      out_sizes = [out_size];
      prep_size = 0;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let t_reg, rest_reg =
              match in_regs with
              | [t_reg; rest_reg] when List.map List.length in_regs = in_sizes
                ->
                  (t_reg, rest_reg)
              | _ -> failwith "Invalid input regs"
            in
            let out_reg = (List.hd rest_reg :: t_reg) @ List.tl rest_reg in
              ( {
                  name = "distr_left";
                  in_regs;
                  prep_reg = [];
                  out_regs = [out_reg];
                  flag_reg = [];
                  garb_reg = [];
                  gate = Identity;
                },
                used_wires )
        end;
    }

(*
Right distributivity isomorphism. Takes (t0 + t1) * t and outputs
(t0 * t) + (t1 * t).
*)
let circuit_distr_right (t0 : exprtype) (t1 : exprtype) (t : exprtype) :
    circuit_spec =
  let in_sizes = [type_size (SumType (t0, t1)); type_size t] in
  let out_size = type_size (SumType (ProdType (t0, t), ProdType (t1, t))) in
    {
      in_sizes;
      out_sizes = [out_size];
      prep_size = 0;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            expect_sizes "circuit_distr_right" in_sizes in_regs;
            let sum_reg, t_reg =
              match in_regs with
              | [sum_reg; t_reg] when List.map List.length in_regs = in_sizes
                ->
                  (sum_reg, t_reg)
              | _ -> assert false
            in
            let ctrlbit = List.hd sum_reg in
            let t0t1_reg = List.tl sum_reg in
            let out_reg = sum_reg @ t_reg in
              ( {
                  name = "distr_right";
                  in_regs;
                  prep_reg = [];
                  out_regs = [out_reg];
                  flag_reg = [];
                  garb_reg = [];
                  gate =
                    begin
                      if type_size t0 > type_size t1 then
                        let t1_reg, rest_reg =
                          list_split_at_i t0t1_reg (type_size t1)
                        in
                        let perm = t1_reg @ t_reg @ rest_reg in
                          Controlled
                            ( [ctrlbit],
                              [true],
                              gate_permute (t0t1_reg @ t_reg) perm )
                      else if type_size t1 > type_size t0 then
                        let t0_reg, rest_reg =
                          list_split_at_i t0t1_reg (1 + type_size t0)
                        in
                        let perm = t0_reg @ t_reg @ rest_reg in
                          Controlled
                            ( [ctrlbit],
                              [false],
                              gate_permute (t0t1_reg @ t_reg) perm )
                      else
                        Identity
                    end;
                },
                used_wires )
        end;
    }

(*
Lemma H.8
*)
let circuit_mixed_error_handling (cs : circuit_spec) : circuit_spec =
  let in_sizes = cs.in_sizes in
  let out_size = 1 + List.fold_left ( + ) 0 cs.out_sizes in
    {
      in_sizes;
      out_sizes = [out_size];
      prep_size = out_size + cs.prep_size;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires reset_garb ->
            expect_sizes "circuit_mixed_error_handling" in_sizes in_regs;
            let new_prep, used_wires = fresh_int_list used_wires out_size in
            let circ, used_wires =
              build_circuit cs in_regs used_wires reset_garb
            in
            let signal_bit = List.hd new_prep in
            let fresh_prep = List.tl new_prep in
            let garb_reg =
              int_list_union circ.flag_reg
                (int_list_union circ.garb_reg fresh_prep)
            in
            let out_reg = signal_bit :: List.concat circ.out_regs in
            let used_wires =
              begin
                if reset_garb then
                  IntSet.diff used_wires (IntSet.of_list garb_reg)
                else
                  used_wires
              end
            in
              ( {
                  name = "mixed_err";
                  in_regs;
                  prep_reg =
                    int_list_union (signal_bit :: circ.prep_reg) fresh_prep;
                  out_regs = [out_reg];
                  flag_reg = [];
                  garb_reg;
                  gate =
                    circ.gate
                    @& begin
                         if circ.flag_reg = [] then
                           Identity
                         else
                           gate_paulix signal_bit
                           @& gate_swap_regs
                                (List.concat circ.out_regs)
                                fresh_prep
                           @& Controlled
                                ( circ.flag_reg,
                                  list_constant false
                                    (List.length circ.flag_reg),
                                  gate_paulix signal_bit
                                  @& gate_swap_regs
                                       (List.concat circ.out_regs)
                                       fresh_prep )
                       end
                    @& begin
                         if reset_garb then
                           gate_reset_reg garb_reg
                         else
                           Identity
                       end;
                },
                used_wires )
        end;
    }

(*
Lemma H.9
*)
let circuit_pure_error_handling (cs : circuit_spec) : circuit_spec =
  let in_sizes = cs.in_sizes in
  let out_size = 1 + max (List.fold_left ( + ) 0 cs.out_sizes) cs.flag_size in
    {
      in_sizes;
      out_sizes = [out_size];
      prep_size = 1 + cs.prep_size;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires reset_garb ->
            expect_sizes "circuit_pure_error_handling" in_sizes in_regs;
            let signal_bit, used_wires = fresh_int used_wires in
            let circ, used_wires =
              build_circuit cs in_regs used_wires reset_garb
            in
              if circ.garb_reg <> [] then
                failwith
                  "Expected empty garbage register for pure error handling";
              ( {
                  name = "pure_err";
                  in_regs;
                  prep_reg = signal_bit :: circ.prep_reg;
                  out_regs =
                    [(signal_bit :: List.concat circ.out_regs) @ circ.flag_reg];
                  flag_reg = [];
                  garb_reg = [];
                  gate =
                    circ.gate @& gate_paulix signal_bit
                    @& begin
                         if circ.flag_reg = [] then
                           Identity
                         else
                           Controlled
                             ( circ.flag_reg,
                               list_constant false (List.length circ.flag_reg),
                               gate_paulix signal_bit )
                       end;
                },
                used_wires )
        end;
    }

let circuit_rphase (t : exprtype) (cs : circuit_spec) (r0 : real) (r1 : real) :
    circuit_spec =
  let cs_adj = circuit_adjoint cs in
  let cs_ef = circuit_pure_error_handling cs_adj in
  let cs_ef_adj = circuit_adjoint cs_ef in
  let size = type_size t in
    {
      in_sizes = [size];
      out_sizes = [size];
      prep_size = cs_ef.prep_size;
      flag_size = cs_ef_adj.flag_size;
      circ_fun =
        begin
          fun in_regs used_wires reset_garb ->
            expect_sizes "circuit_rphase" [size] in_regs;
            let circ_ef, used_wires =
              build_circuit cs_ef in_regs used_wires reset_garb
            in
            let ef_out = List.hd circ_ef.out_regs in
            let signal_bit = List.hd ef_out in
            let circ_ef_adj, used_wires =
              build_circuit cs_ef_adj [ef_out] used_wires reset_garb
            in
              ( {
                  name = "rphase";
                  in_regs;
                  prep_reg = circ_ef.prep_reg;
                  out_regs = circ_ef_adj.out_regs;
                  flag_reg = circ_ef_adj.flag_reg;
                  garb_reg = [];
                  gate =
                    circ_ef.gate
                    @& Controlled ([signal_bit], [false], GphaseGate r0)
                    @& Controlled ([signal_bit], [true], GphaseGate r1)
                    @& circ_ef_adj.gate;
                },
                used_wires )
        end;
    }

(*
Discards the input by putting the input register into the garbage register.
*)
let circuit_discard (size : int) =
  {
    in_sizes = [size];
    out_sizes = [];
    prep_size = 0;
    flag_size = 0;
    circ_fun =
      begin
        fun in_regs used_wires reset_garb ->
          let garb_reg = List.concat in_regs in
          let used_wires =
            begin
              if reset_garb then
                IntSet.diff used_wires (IntSet.of_list garb_reg)
              else
                used_wires
            end
          in
            ( {
                name = "discard";
                in_regs;
                prep_reg = [];
                out_regs = [];
                flag_reg = [];
                garb_reg;
                gate =
                  begin
                    if reset_garb then gate_reset_reg garb_reg else Identity
                  end;
              },
              used_wires )
      end;
  }

(*
Purification of a circuit - ensures that when the circuit is built, no
garbage wires are reset or reused.
*)
let circuit_purify (cs : circuit_spec) : circuit_spec =
  let garb_size =
    begin
      let in_regs, used_wires = fresh_int_lists IntSet.empty cs.in_sizes in
      let temp_circ, _ = build_circuit cs in_regs used_wires true in
        List.length temp_circ.garb_reg
    end
  in
    {
      in_sizes = cs.in_sizes;
      out_sizes = cs.out_sizes @ [garb_size];
      prep_size = cs.prep_size;
      flag_size = cs.flag_size;
      circ_fun =
        begin
          fun in_regs used_wires _ -> build_circuit cs in_regs used_wires false
        end;
    }

(*
Given a context and a register containing something in the space associated
to the context, splits the register into smaller registers mapped to each
variable in the context.
*)
let context_reg_split (d : context) (reg : int list) : int list StringMap.t =
  let rec iter (cur : int list StringMap.t) (reg_rest : int list)
      (bind_rest : (string * exprtype) list) : int list StringMap.t =
    match bind_rest with
    | [] -> cur
    | (x, t) :: tl -> begin
        let newreg, newrest = list_split_at_i reg_rest (type_size t) in
          iter (StringMap.add x newreg cur) newrest tl
      end
  in
    iter StringMap.empty reg (StringMap.bindings d)

(*
Partition a register corresponding to a context into a part corresponding to
those variables that are in a specified set and those that are not.
*)
let context_reg_partition (d : context) (reg : int list) (fv : StringSet.t) :
    int list * int list =
  let reg_map = context_reg_split d reg in
  let reg_map_in = StringMap.filter (fun x _ -> StringSet.mem x fv) reg_map in
  let reg_map_out =
    StringMap.filter (fun x _ -> not (StringSet.mem x fv)) reg_map
  in
    assert (
      StringSet.inter (map_dom reg_map_in) (map_dom reg_map_out)
      = StringSet.empty);
    let flatten_bindings = List.fold_left (fun l (_, l') -> l @ l') [] in
      ( flatten_bindings (StringMap.bindings reg_map_in),
        flatten_bindings (StringMap.bindings reg_map_out) )

(*
Creates a circuit that partitions a register corresponding to a context into a
part corresponding to those variables that are in a specified set and those that
are not. These two new registers form the output registers of the circuit.
*)
let circuit_context_partition (d : context) (fv : StringSet.t) : circuit_spec =
  let in_size = context_size d in
  let d0, d1 = map_partition d fv in
    {
      in_sizes = [in_size];
      out_sizes = [context_size d0; context_size d1];
      prep_size = 0;
      flag_size = 0;
      circ_fun =
        begin
          fun in_regs used_wires _ ->
            let in_reg =
              expect_single_in_reg "circuit_context_partition" in_regs in_size
            in
            let out0, out1 = context_reg_partition d in_reg fv in
              ( {
                  name = "context_partition";
                  in_regs;
                  prep_reg = [];
                  out_regs = [out0; out1];
                  flag_reg = [];
                  garb_reg = [];
                  gate = Identity;
                },
                used_wires )
        end;
    }

(*
A circuit that merges two disjoint (important!) contexts into one. Since the
contexts are disjoint, this merge is simply the adjioint of the
partition circuit.
*)
let circuit_context_merge (d0 : context) (d1 : context) : circuit_spec =
  let d =
    match map_merge false d0 d1 with
    | SomeE d -> d
    | NoneE err -> failwith err
  in
  let fv0 = map_dom d0 in
    circuit_adjoint (circuit_context_partition d fv0)

(*
Given a circuit and an intermediate representation command, as well as an
intermediate representation valuation, applies the command's operator to the
registers of the circuit. Returns an updated circuit and valuation (which
has the new variables corresponding to the output registers of the circuit).
*)
let rec compile_inter_com_to_circuit (circ : circuit) (used_wires : IntSet.t)
    (iv : inter_valuation) (ic : inter_com) :
    circuit * IntSet.t * inter_valuation =
  let target, op, args = ic in
  let argset = StringSet.of_list args in
    if
      StringSet.inter (StringSet.of_list target)
        (StringSet.diff (map_dom iv) argset)
      <> StringSet.empty
    then
      failwith "Attempted to store new value in existing variable"
    else
      let ev_regs =
        List.map
          (fun x ->
            match StringMap.find_opt x iv with
            | Some reg -> reg
            | None -> failwith (Printf.sprintf "Variable %s not found" x))
          args
      in
      let op_cs = compile_inter_op_to_circuit op in
      let op_circ, used_wires = build_circuit op_cs ev_regs used_wires true in
      let res_circ =
        {
          name = circ.name;
          in_regs = circ.in_regs;
          prep_reg = int_list_union circ.prep_reg op_circ.prep_reg;
          out_regs = op_circ.out_regs;
          flag_reg = int_list_union circ.flag_reg op_circ.flag_reg;
          garb_reg = int_list_union circ.garb_reg op_circ.garb_reg;
          gate = circ.gate @& op_circ.gate;
        }
      in
        if debug_mode then
          Printf.printf
            "Apply: target = %s, args = %s, out_regs = %s, ev_regs = %s\n"
            (string_of_list (fun s -> s) target)
            (string_of_list (fun s -> s) args)
            (string_of_list (string_of_list string_of_int) op_circ.out_regs)
            (string_of_list (string_of_list string_of_int) ev_regs);
        let iv' =
          StringMap.filter (fun x _ -> not (StringSet.mem x argset)) iv
        in
        let iv_add = List.combine target op_circ.out_regs in
        let iv'' =
          match
            map_merge false iv' (StringMap.of_seq (List.to_seq iv_add))
          with
          | SomeE iv'' -> iv''
          | NoneE err -> failwith err
        in
          (res_circ, used_wires, iv'')

(*
Applies a list of intermediate representation commands, in series, to a circuit,
updating the valuation each time.
*)
and compile_inter_com_list_to_circuit (circ : circuit) (used_wires : IntSet.t)
    (iv : inter_valuation) (iel : inter_com list) :
    circuit * IntSet.t * inter_valuation =
  match iel with
  | [] -> (circ, used_wires, iv)
  | ielh :: ielt -> begin
      let circ', used_wires, iv' =
        compile_inter_com_to_circuit circ used_wires iv ielh
      in
        compile_inter_com_list_to_circuit circ' used_wires iv' ielt
    end

(*
Converts an intermediate representation operator to a circuit, calling the
previously defined circuits for most cases and calling the above functions
for the user-defined ILambda case.
*)
and compile_inter_op_to_circuit (op : inter_op) : circuit_spec =
  match op with
  | IEmpty -> circuit_empty
  | IIdentity t -> circuit_identity t
  | IU3 (theta, phi, lambda) -> circuit_u3 theta phi lambda
  | IRphase (t, op, r0, r1) ->
      if r0 = r1 then
        circuit_gphase t r0
      else
        circuit_rphase t (compile_inter_op_to_circuit op) r0 r1
  | ILeft (t0, t1) -> circuit_left t0 t1
  | IRight (t0, t1) -> circuit_right t0 t1
  | IPair (t0, t1) -> circuit_pair t0 t1
  | IShare t -> circuit_share (type_size t)
  | IContextShare d -> circuit_share (context_size d)
  | IAdjoint op' -> circuit_adjoint (compile_inter_op_to_circuit op')
  | IDirsum (op0, op1) ->
      circuit_dirsum
        (compile_inter_op_to_circuit op0)
        (compile_inter_op_to_circuit op1)
  | IAssoc (t0, t1, t2) -> circuit_assoc t0 t1 t2
  | IDistrLeft (t, t0, t1) -> circuit_distr_left t t0 t1
  | IDistrRight (t0, t1, t) -> circuit_distr_right t0 t1 t
  | IMixedErr op ->
      circuit_mixed_error_handling (compile_inter_op_to_circuit op)
  | IPureErr op -> circuit_pure_error_handling (compile_inter_op_to_circuit op)
  | IDiscard t -> circuit_discard (type_size t)
  | IContextDiscard d -> circuit_discard (context_size d)
  | IPurify op' -> circuit_purify (compile_inter_op_to_circuit op')
  | IContextPartition (d, fv) -> circuit_context_partition d fv
  | IContextMerge (d0, d1) -> circuit_context_merge d0 d1
  | ILambda (args, iel, ret) -> begin
      let arg_names = List.map fst args in
      let arg_sizes = List.map snd args in
      let ret_names = List.map fst ret in
      let ret_sizes = List.map snd ret in
        circuit_spec_find_unknown_sizes arg_sizes ret_sizes
          begin
            fun in_regs used_wires _ ->
              if debug_mode then
                Printf.printf
                  "ILambda: arg_names = %s, in_regs = %s, arg_sizes = %s, \
                   ret_sizes = %s\n"
                  (string_of_list (fun s -> s) arg_names)
                  (string_of_list (string_of_list string_of_int) in_regs)
                  (string_of_list string_of_int arg_sizes)
                  (string_of_list string_of_int ret_sizes);
              let iv =
                StringMap.of_seq (List.to_seq (List.combine arg_names in_regs))
              in
              let init_circ =
                {
                  name = "init";
                  in_regs;
                  prep_reg = [];
                  out_regs = in_regs;
                  flag_reg = [];
                  garb_reg = [];
                  gate = Identity;
                }
              in
              let circ, used_wires, iv =
                compile_inter_com_list_to_circuit init_circ used_wires iv iel
              in
              let ret_regs =
                List.map (fun x -> StringMap.find x iv) ret_names
              in
                ({ circ with out_regs = ret_regs }, used_wires)
          end
    end

(*
Compilation of a pure expression into the intermediate representation.
This creates a circuit that takes in two registers, corresponding to the
classical and quantum contexts.
*)
let rec compile_pure_expr_to_inter_op (g : context) (d : context) (e : expr) :
    inter_op =
  let t =
    match pure_type_check g d e with
    | SomeE t -> t
    | NoneE err -> failwith err
  in
  let gsize = context_size g in
  let dsize = context_size d in
  let tsize = type_size t in
    match e with
    | Null -> begin
        inter_lambda
          [("g", gsize); ("d", dsize)]
          []
          [("g", gsize); ("d", tsize)]
      end
    | Var x -> begin
        match StringMap.bindings d with
        | [] -> begin
            let xreg, grest = map_partition g (StringSet.singleton x) in
              inter_lambda
                [("g", gsize); ("d", dsize)]
                [
                  inter_letapp ["x"; "rest"]
                    (IContextPartition (g, StringSet.singleton x))
                    ["g"];
                  inter_letapp ["x"; "res"] (IShare t) ["x"];
                  inter_letapp ["g"]
                    (IContextMerge (xreg, grest))
                    ["x"; "rest"];
                ]
                [("g", gsize); ("res", tsize)]
          end
        | [(x', _)] when x' = x -> begin
            inter_lambda
              [("g", gsize); ("d", dsize)]
              []
              [("g", gsize); ("d", tsize)]
          end
        | _ ->
            failwith
              "Variable not found or irrelevant variables in quantum context"
      end
    | Qpair (e0, e1) -> begin
        let t0, t1 =
          match t with
          | ProdType (t0, t1) -> (t0, t1)
          | _ -> failwith "Expected product type"
        in
        let fv0 = free_vars e0 in
        let fv1 = free_vars e1 in
        let fv01 = StringSet.inter fv0 fv1 in
        let d01, d_xor = map_partition d fv01 in
        let d0 = map_restriction d fv0 in
        let d1 = map_restriction d fv1 in
        let d0', d1' = map_partition d_xor fv0 in
        let op0 = compile_pure_expr_to_inter_op g d0 e0 in
        let op1 = compile_pure_expr_to_inter_op g d1 e1 in
          inter_lambda
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["d01"; "d_xor"] (IContextPartition (d, fv01)) ["d"];
              inter_letapp ["d0*"; "d1*"]
                (IContextPartition (d_xor, fv0))
                ["d_xor"];
              inter_letapp ["d01"; "d01*"] (IContextShare d01) ["d01"];
              inter_letapp ["d01_0"] (IContextMerge (d01, d0')) ["d01"; "d0*"];
              inter_letapp ["d01*_1"]
                (IContextMerge (d01, d1'))
                ["d01*"; "d1*"];
              inter_letapp ["g"; "t0"] op0 ["g"; "d01_0"];
              inter_letapp ["g"; "t1"] op1 ["g"; "d01*_1"];
              inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
            ]
            [("g", gsize); ("res", tsize)]
      end
    | Ctrl _ -> failwith "TODO"
    | Try _ -> failwith "Try is not a pure expression"
    | Apply (f, e') -> begin
        let e'_op = compile_pure_expr_to_inter_op g d e' in
        let f_op = compile_pure_prog_to_inter_op f in
          inter_lambda
            [("g", gsize); ("d", dsize)]
            [
              inter_letapp ["g"; "t*"] e'_op ["g"; "d"];
              inter_letapp ["res"] f_op ["t*"];
            ]
            [("g", gsize); ("res", tsize)]
      end

(*
Compilation of a mixed expression into the intermediate representation.
This creates a circuit that takes in one register, corresponding to the
quantum context.
*)
and compile_mixed_expr_to_inter_op (d : context) (e : expr) : inter_op =
  let t =
    match mixed_type_check d e with
    | SomeE t -> t
    | NoneE err -> failwith err
  in
  let dsize = context_size d in
  let tsize = type_size t in
    match pure_type_check StringMap.empty d e with
    | SomeE _ -> begin
        let e_op = compile_pure_expr_to_inter_op StringMap.empty d e in
          inter_lambda
            [("d", dsize)]
            [
              inter_letapp ["g"] IEmpty [];
              inter_letapp ["g"; "res"] e_op ["g"; "d"];
              inter_letapp [] (IAdjoint IEmpty) ["g"];
            ]
            [("res", tsize)]
      end
    | _ -> begin
        match e with
        | Qpair (e0, e1) -> begin
            let t0, t1 =
              match t with
              | ProdType (t0, t1) -> (t0, t1)
              | _ -> failwith "Expected product type"
            in
            let fv0 = free_vars e0 in
            let fv1 = free_vars e1 in
            let fv01 = StringSet.inter fv0 fv1 in
            let d01, d_xor = map_partition d fv01 in
            let d0 = map_restriction d fv0 in
            let d1 = map_restriction d fv1 in
            let d0' = map_restriction d_xor fv0 in
            let d1' = map_restriction d_xor fv1 in
            let op0 = compile_mixed_expr_to_inter_op d0 e0 in
            let op1 = compile_mixed_expr_to_inter_op d1 e1 in
              inter_lambda
                [("d", dsize)]
                [
                  inter_letapp ["d01"; "d_xor"]
                    (IContextPartition (d, fv01))
                    ["d"];
                  inter_letapp ["d0*"; "d1*"]
                    (IContextPartition (d_xor, fv0))
                    ["d_xor"];
                  inter_letapp ["d01"; "d01*"] (IContextShare d01) ["d01"];
                  inter_letapp ["d01_0"]
                    (IContextMerge (d01, d0'))
                    ["d01"; "d0*"];
                  inter_letapp ["d01*_1"]
                    (IContextMerge (d01, d1'))
                    ["d01*"; "d1*"];
                  inter_letapp ["t0"] op0 ["d01_0"];
                  inter_letapp ["t1"] op1 ["d01*_1"];
                  inter_letapp ["res"] (IPair (t0, t1)) ["t0"; "t1"];
                ]
                [("res", tsize)]
          end
        | Try (e0, e1) -> begin
            let fv0 = free_vars e0 in
            let fv1 = free_vars e1 in
            let d0 = map_restriction d fv0 in
            let d1 = map_restriction d fv1 in
            let op0 = compile_mixed_expr_to_inter_op d0 e0 in
            let op1 = compile_mixed_expr_to_inter_op d1 e1 in
              inter_lambda
                [("d", dsize)]
                [
                  inter_letapp ["d0"; "d1"] (IContextPartition (d, fv0)) ["d"];
                  inter_letapp ["t+c0"] (IMixedErr op0) ["d0"];
                  inter_letapp ["t+c1"] (IMixedErr op1) ["d1"];
                  inter_letapp ["t*(t+c)+(t+c)"]
                    (IDistrRight (t, Qunit, SumType (t, Qunit)))
                    ["t+c0"; "t+c1"];
                  inter_letapp ["(t*(t+c)+t)+c"]
                    (IAdjoint
                       (IAssoc (ProdType (t, SumType (t, Qunit)), t, Qunit)))
                    ["t*(t+c)+(t+c)"];
                  inter_letapp ["t*(t+c)+t"]
                    (IAdjoint
                       (ILeft
                          (SumType (ProdType (t, SumType (t, Qunit)), t), Qunit)))
                    ["(t*(t+c)+t)+c"];
                  inter_letapp ["t"; "(t+c)+c"]
                    (IAdjoint (IDistrLeft (t, SumType (t, Qunit), Qunit)))
                    ["t*(t+c)+t"];
                  inter_letapp []
                    (IDiscard (SumType (SumType (t, Qunit), Qunit)))
                    ["(t+c)+c"];
                ]
                [("t", tsize)]
          end
        | Apply (f, e') -> begin
            let e'_op = compile_mixed_expr_to_inter_op d e' in
            let f_op = compile_mixed_prog_to_inter_op f in
              inter_lambda
                [("d", dsize)]
                [
                  inter_letapp ["t*"] e'_op ["d"];
                  inter_letapp ["res"] f_op ["t*"];
                ]
                [("res", tsize)]
          end
        | _ -> failwith "Error in mixed expression compilation"
      end

(*
Compilation of a pure program into the intermediate representation.
*)
and compile_pure_prog_to_inter_op (f : prog) : inter_op =
  match f with
  | U3 (theta, phi, lambda) -> IU3 (theta, phi, lambda)
  | Left (t0, t1) -> ILeft (t0, t1)
  | Right (t0, t1) -> IRight (t0, t1)
  | Lambda (e, t, e') -> begin
      let t' =
        match prog_type_check f with
        | SomeE (Coherent (_, t')) -> t'
        | SomeE (Channel _) ->
            failwith "Attempted pure compilation for mixed program"
        | NoneE err -> failwith err
      in
        match context_check StringMap.empty t e with
        | NoneE err -> failwith err
        | SomeE d -> begin
            let e_op = compile_pure_expr_to_inter_op StringMap.empty d e in
            let e'_op = compile_pure_expr_to_inter_op StringMap.empty d e' in
              inter_lambda
                [("t", type_size t)]
                [
                  inter_letapp ["g"] IEmpty [];
                  inter_letapp ["g"; "d"] (IAdjoint e_op) ["g"; "t"];
                  inter_letapp ["g"; "res"] e'_op ["g"; "d"];
                  inter_letapp [] (IAdjoint IEmpty) ["g"];
                ]
                [("res", type_size t')]
          end
    end
  | Rphase (t, e, r0, r1) -> (
      match context_check StringMap.empty t e with
      | NoneE err -> failwith err
      | SomeE d -> begin
          let e_op = compile_pure_expr_to_inter_op StringMap.empty d e in
          let e_op_no_g =
            inter_lambda
              [("d", context_size d)]
              [
                inter_letapp ["g"] IEmpty [];
                inter_letapp ["g"; "res"] e_op ["g"; "d"];
                inter_letapp [] (IAdjoint IEmpty) ["g"];
              ]
              [("res", type_size t)]
          in
            IRphase (t, e_op_no_g, r0, r1)
        end)

(*
Compilation of a mixed program into the intermediate representation.
*)
and compile_mixed_prog_to_inter_op (f : prog) : inter_op =
  match (f, prog_type_check f) with
  | _, NoneE err -> failwith err
  | _, SomeE (Coherent _) -> compile_pure_prog_to_inter_op f
  | Lambda (e, t, e'), SomeE (Channel (_, t')) -> begin
      match context_check StringMap.empty t e with
      | NoneE err -> failwith err
      | SomeE d ->
          let fve' = free_vars e' in
          let d', d0 = map_partition d fve' in
          let e_op = compile_mixed_expr_to_inter_op d e in
          let e'_op = compile_mixed_expr_to_inter_op d' e' in
            inter_lambda
              [("t", type_size t)]
              [
                inter_letapp ["d"] (IAdjoint e_op) ["t"];
                inter_letapp ["d*"; "d0"] (IContextPartition (d, fve')) ["d"];
                inter_letapp ["res"] e'_op ["d*"];
                inter_letapp [] (IContextDiscard d0) ["d0"];
              ]
              [("res", type_size t')]
    end
  | _ -> failwith "Error in mixed program compilation"

(*
Main procedure for compiling a Qunity expression into a low-level quantum
gate: into the intermediate representation, then compiling the intermediate
representation to a circuit, then applying several postprocessing steps, and
outputting the circuit's gate and additional relevant information.
*)
let expr_compile (annotate : bool) (e : expr) :
    gate * int * int list * int list =
  annotation_mode := annotate;
  let op = compile_mixed_expr_to_inter_op StringMap.empty e in
  let cs = compile_inter_op_to_circuit op in
  let circ, _ = build_circuit cs [[]] IntSet.empty true in
  let nqubits = List.length circ.prep_reg in
  let rewiring =
    IntMap.of_seq
      (List.to_seq
         (List.combine circ.prep_reg (range (List.length circ.prep_reg))))
  in
  let gate =
    gate_combine_controls
      (gate_distribute_controls
         (gate_remove_identities (gate_rewire circ.gate rewiring)))
  in
  let out_reg =
    match circ.out_regs with
    | [out_reg] -> out_reg
    | _ -> failwith "Expected single out reg"
  in
    (gate, nqubits, out_reg, circ.flag_reg)
