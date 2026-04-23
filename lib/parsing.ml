open Util
open Syntax
open Metaprogramming

module I = Parser.MenhirInterpreter
(** Source:
    {:https://baturin.org/blog/declarative-parse-error-reporting-with-menhir/}
*)

exception Syntax_error of ((int * int) option * string)

let get_lexing_position lexbuf =
  let p = Lexing.lexeme_start_p lexbuf in
  let line_number = p.Lexing.pos_lnum in
  let column = p.Lexing.pos_cnum - p.Lexing.pos_bol + 1 in
    (line_number, column)

let get_parse_error env =
  match I.top env with
  | None -> "Invalid syntax"
  | Some (I.Element (state, _, _, _)) -> (
      try
        let message = Parser_messages.message (I.number state) in
          if message = "<YOUR SYNTAX ERROR MESSAGE HERE>\n" then
            "Invalid syntax"
          else
            message
      with
      | Not_found -> "Invalid syntax")

let rec parse lexbuf (checkpoint : 'a I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let token = Lexer.read lexbuf in
      let startp = lexbuf.lex_start_p and endp = lexbuf.lex_curr_p in
      let checkpoint = I.offer checkpoint (token, startp, endp) in
        parse lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
      let checkpoint = I.resume checkpoint in
        parse lexbuf checkpoint
  | I.HandlingError _env ->
      let line, pos = get_lexing_position lexbuf in
      let err = get_parse_error _env in
        raise (Syntax_error (Some (line, pos), err))
  | I.Accepted v -> v
  | I.Rejected -> raise (Syntax_error (None, "Invalid syntax"))

(** Parses from a string. *)
let parse_string parse_fun (s : string) =
  let lexbuf = Lexing.from_string s in
    parse lexbuf (parse_fun lexbuf.lex_curr_p)

(** Parses from a file. *)
let parse_file parse_fun (filename : string) =
  let file = open_in filename in
  let lexbuf = Lexing.from_channel (open_in filename) in
  let result = parse lexbuf (parse_fun lexbuf.lex_curr_p) in
    close_in file;
    result

let parse_string_qunityfile = parse_string Parser.Incremental.qunityfile
let parse_file_qunityfile = parse_file Parser.Incremental.qunityfile
let parse_string_qunitylib = parse_string Parser.Incremental.qunitylib
let parse_file_qunitylib = parse_file Parser.Incremental.qunitylib

(** Given a parsing function (from a string or a file), outputs a parsing
    result if there are no syntax errors. Otherwise, outputs a syntax error
    message. *)
let parse_with_err parse_fun s : 'a optionE =
  try SomeE (parse_fun s) with
  | Syntax_error (loc, err) -> begin
      let s =
        Printf.sprintf "Syntax error: %s\n%s" err
          begin match loc with
          | Some (line, pos) -> Printf.sprintf "At line %d, col %d" line pos
          | _ -> ""
          end
      in
        NoneE s
    end
  | Lexer.Lexing_error err -> NoneE err

(** Reads a file and outputs a Qunity expression. *)
let get_expr_from_file (prog_filename : string) :
    (expr * defmap * xtype) optionE =
  let stdlib_filename = "qunitylib/stdlib.qunity" in
    match parse_with_err parse_file_qunitylib stdlib_filename with
    | NoneE err -> NoneE (err ^ "\nin " ^ stdlib_filename)
    | SomeE stdlib_dm ->
        begin match parse_with_err parse_file_qunityfile prog_filename with
        | NoneE err -> NoneE (err ^ "\nin " ^ prog_filename)
        | SomeE (dm, xe) -> begin
            let dm = combine_defmaps stdlib_dm dm in
              match xexpr_eval dm StringMap.empty None xe with
              | SomeE (e, xt, _) -> SomeE (e, dm, xt)
              | NoneE err ->
                  NoneE (Printf.sprintf "Preprocessing error: %s" err)
          end
        end
