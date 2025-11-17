(*
A regarder:
    - https://www.cse.chalmers.se/~abela/talkMcGill2012Normalization.pdf
    - https://flaviomoura.info/files/wollic2010.pdf
    - https://en.wikipedia.org/wiki/Occurs_check
 *)

exception TypeError of string

(* Défintion inductive d'un terme *)
type terme =
  | Var of int
  | Type of int
  | Const of string
  (* Pi(A -> B) = Pi(A) => Pi(B) *)
  | Pi of terme * terme (* argument / sortie *)
  | Lambda of terme * terme (* type de l'argument / corps *)
  | App of terme * terme (* décrit une composition: (x . X) Y *)
  (* Ne fonctionne pas *)
  | Let of terme * terme * terme (* let x : A := t in u *)

and env = (string * terme) list

and annotation =
  | Locale of char * terme (* x : A *)
  | Def of char * terme * terme (* x := t : A *)

and contexte = annotation list

let type_of_locale (ctx : contexte) (n : int) : terme option =
  match List.nth_opt ctx n with
  | Some (Locale (_, t)) | Some (Def (_, t, _)) -> Some t
  | None -> None

and type_of_const (env : env) (n : string) : terme option = List.assoc_opt n env

and string_of_terme (t : terme) : string =
  let rec s (i : int) : terme -> string = function
    | Var n -> Printf.sprintf "v%d" n
    | Type k -> Printf.sprintf "T%d" k
    | Pi (a, b) -> Printf.sprintf "(π %s -> %s)" (s i a) (s (i + 1) b)
    | Lambda (a, b) ->
        Printf.sprintf "(λ v%d : %s . %s)" i (s i a) (s (i + 1) b)
    | App (lhs, rhs) -> Printf.sprintf "(%s %s)" (s i lhs) (s i rhs)
    | Const n -> n
    | Let (a, t, u) ->
        Printf.sprintf "(let : %s := %s in %s)" (s i a) (s i t) (s (i + 1) u)
  in
  s 0 t

(* Décalage des indices de variables de De Bruijn *)
let rec shift (d : int) (c : int) = function
  | Var k -> if k >= c then Var (k + d) else Var k
  | Pi (a, b) -> Pi (shift d c a, shift d (c + 1) b)
  | Lambda (arg_t, corps) -> Lambda (shift d c arg_t, shift d (c + 1) corps)
  | App (lhs, rhs) -> App (shift d c lhs, shift d c rhs)
  | Let (a, t, u) -> Let (shift d c a, shift d c t, shift d (c + 1) u)
  | x -> x

(* Beta-reduction: [s\t] substitution de s par t *)
let ( </> ) (t : terme) (s : terme) =
  (* Opérateur . [s\v_i]: Beta-reduction sur les variables de De Bruijn *)
  let rec aux (s : terme) (i : int) : terme -> terme =
    let ( <~ ) = fun s a -> aux s i a in
    let ( <~~ ) = fun s a -> aux s (i + 1) a in

    function
    | Var k -> if k = i then s else if k > i then Var (k - 1) else Var k
    | Pi (a, b) -> Pi (s <~ a, shift 1 0 s <~~ b)
    | Lambda (arg_t, corps) -> Lambda (s <~ arg_t, shift 1 0 s <~~ corps)
    | App (lhs, rhs) -> App (s <~ lhs, s <~ rhs)
    | Let (a, t, u) -> Let (s <~ a, s <~ t, shift 1 0 s <~~ u)
    | x -> x
  in
  shift 1 0 s |> aux t 0 |> shift (-1) 0

(* forme normale faible: https://j-hui.com/pages/normal-forms/ *)
let rec whnf (env : env) (ctx : contexte) : terme -> terme = function
  | App (lhs, rhs) -> (
      match whnf env ctx lhs with
      | Lambda (_, corps) -> whnf env ctx (corps </> rhs)
      | lhs' -> App (lhs', rhs))
  | Let (_, v, u) -> whnf env ctx (u </> v)
  | x -> x

(* alpha-equivalence: https://inria.hal.science/hal-01354360/file/RR-LIG-013.pdf *)
(* Test l'alpha equivalence entre deux termes *)
let alpha_equiv (env : env) (ctx : contexte) (x : terme) (y : terme) =
  let rec aux (ctx : contexte) (t : terme) : terme =
    match whnf env ctx t with
    | App (lhs, rhs) -> App (aux ctx lhs, aux ctx rhs)
    (* | Pi (a, b) -> Pi (aux ctx a, aux (Locale ('_', a) :: ctx) b) *)
    | Lambda (arg_t, corps) ->
        Lambda (aux ctx arg_t, aux (Locale ('_', corps) :: ctx) corps)
    | x -> x
  in
  aux ctx x = aux ctx y

(* Infere le type d'un terme *)
let rec inferer (env : env) (ctx : contexte) : terme -> terme = function
  | Type k -> Type (k + 1)
  | Var n -> (
      match type_of_locale ctx n with
      | Some t -> t
      | None ->
          raise
            (TypeError (Printf.sprintf "variable sortie de son contexte: v%d" n))
      )
  | Const n -> (
      match type_of_const env n with
      | Some t -> t
      | None -> raise (TypeError ("constante non définie: " ^ n)))
  | App (lhs, rhs) -> (
      match inferer env ctx lhs |> whnf env ctx with
      | Pi (arg_t, res_t) ->
          check env ctx arg_t rhs;
          res_t </> rhs
      | x -> raise (TypeError ("On ne peut pas annoter: " ^ string_of_terme x)))
  | Pi (a, b) ->
      check env ctx (Type 0) a;
      check env (Locale ('_', a) :: ctx) (Type 0) b;
      Type 0
  | Let (v_t, v, body) ->
      check env ctx v_t v;
      inferer env (Def ('_', v_t, v) :: ctx) body
  | _ -> raise (TypeError "Pas d'annotation")

and check (env : env) (ctx : contexte) (expected : terme) : terme -> unit =
  let ( <~> ) = fun x y -> alpha_equiv env ctx x y in

  function
  | Lambda (arg_t, corps) -> (
      match whnf env ctx expected with
      | Pi (arg_t', res_t) ->
          if not (arg_t <~> arg_t') then
            raise (TypeError "Le type diffère de l'annotation")
          else check env (Locale ('_', arg_t') :: ctx) res_t corps
      | _ -> raise (TypeError "On voulait une fonction"))
  | target ->
      let target_t = inferer env ctx target in
      if not (target_t <~> expected) then
        raise
          (TypeError
             ("Erreur: " ^ string_of_terme target ^ ", "
            ^ string_of_terme expected))
      else ()

and ( |~ ) =
 fun (target : terme) (expected : terme) : unit -> check [] [] expected target

let demo1 () =
  print_endline "Démonstration (A -> A)";

  (* Construction du type : A -> A ainsi que du lambda terme *)
  let preuve = Pi (Type 0, Type 0) in
  let lambda = Lambda (Type 0, Var 0) in

  match lambda |~ preuve with
  | () ->
      Printf.printf "  Termes ...: %s\n  Type .....: %s\n  C/C ......: OK\n"
        (string_of_terme lambda) (string_of_terme preuve)
  | exception TypeError msg -> Printf.printf "  erreur: %s\n" msg

let demo2 () =
  let t_A = Type 0 in

  (* id = λ (x : A) . x *)
  let id_t = Lambda (t_A, Var 0) in

  (* let id : A -> A := id_t in id A *)
  let def = Let (Pi (t_A, t_A), id_t, App (Var 0, t_A)) in

  match inferer [] [] def with
  | ty ->
      Printf.printf "  Terme ....: %s\n" (string_of_terme def);
      Printf.printf "  whnf .....: %s\n" (string_of_terme (whnf [] [] def));
      Printf.printf "  Type .....: %s\n" (string_of_terme ty);
      print_endline "  C/C ......: OK"
  | exception TypeError msg -> Printf.printf "  erreur: %s\n" msg

let () =
  while true do
    print_string "<µ> ";
    flush stdout;
    let l = try read_line () with End_of_file -> "bye!" in
    match String.trim l with
    | "demo1" -> demo1 ()
    | "demo2" -> demo2 ()
    (* TODO: autres tests *)
    | _ -> ()
  done
