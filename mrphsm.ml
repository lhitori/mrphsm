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
  | Const of string (* constante *)
  | Pi of terme * terme (* (x : A) -> B *)
  | Lambda of terme * terme (* Vu comme type / corps *)
  | App of terme * terme
  | Let of terme * terme * terme (* let x : A := t in u *)

and env = (string * terme) list

and annotation =
  | Locale of string * terme (* x : A *)
  | Def of string * terme * terme (* x := t : A *)

and contexte = annotation list

let type_of_locale (ctx : contexte) (n : int) : terme option =
  match List.nth_opt ctx n with
  | Some (Locale (_, t)) | Some (Def (_, t, _)) -> Some t
  | None -> None

let type_of_const (env : env) (n : string) : terme option = List.assoc_opt n env

let ajoute_locale (ctx : contexte) (n : string) (t : terme) : contexte =
  Locale (n, t) :: ctx

let ajoute_def (ctx : contexte) (n : string) (t : terme) (v : terme) : contexte
    =
  Def (n, t, v) :: ctx

let string_of_terme (t : terme) : string =
  let rec s i = function
    | Var n -> Printf.sprintf "v%d" n
    | Type k -> Printf.sprintf "T%d" k
    | Pi (a, b) -> Printf.sprintf "(π (%s -> %s))" (s i a) (s (i + 1) b)
    | Lambda (a, b) ->
        Printf.sprintf "(λ (v%d : %s) . %s)" i (s i a) (s (i + 1) b)
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
  | Lambda (a, b) -> Lambda (shift d c a, shift d (c + 1) b)
  | App (lhs, rhs) -> App (shift d c lhs, shift d c rhs)
  | Let (a, t, u) -> Let (shift d c a, shift d c t, shift d (c + 1) u)
  | x -> x

(* Beta-reduction: [s\t] *)
let ( </> ) (t : terme) (s : terme) =
  (* Opérateur . [s\v_i]: Beta-reduction sur les variables de De Bruijn *)
  let rec aux (i : int) (s : terme) : terme -> terme =
    let ( <~ ) = fun s a -> aux i s a in
    let ( <~~ ) = fun s a -> aux (i + 1) s a in

    function
    | Var k -> if k = i then s else if k > i then Var (k - 1) else Var k
    | Pi (a, b) -> Pi (s <~ a, shift 1 0 s <~~ b)
    | Lambda (a, b) -> Lambda (s <~ a, shift 1 0 s <~~ b)
    | App (lhs, rhs) -> App (s <~ lhs, s <~ rhs)
    | Let (a, t, u) -> Let (s <~ a, s <~ t, shift 1 0 s <~~ u)
    | x -> x
  in

  shift (-1) 0 (aux 0 (shift 1 0 s) t)

(* forme normale faible: https://j-hui.com/pages/normal-forms/ *)
let rec whnf (env : env) (ctx : contexte) : terme -> terme = function
  | App (lhs, rhs) -> (
      match whnf env ctx lhs with
      | Lambda (_, y) -> whnf env ctx (y </> rhs)
      | lhs' -> App (lhs', rhs))
  | Let (_, v, u) -> whnf env ctx (u </> v)
  | x -> x

(* alpha-equivalence: https://inria.hal.science/hal-01354360/file/RR-LIG-013.pdf *)
let alpha_equiv (env : env) (ctx : contexte) (t : terme) (u : terme) =
  let rec aux (ctx : contexte) (t : terme) : terme =
    match whnf env ctx t with
    | App (f, x) -> App (aux ctx f, aux ctx x)
    | Pi (a, b) -> Pi (aux ctx a, aux (Locale ("_", a) :: ctx) b)
    | Lambda (a, b) -> Lambda (aux ctx a, aux (Locale ("_", b) :: ctx) b)
    | x -> x
  in
  aux ctx t = aux ctx u

let unwrap : string option -> unit = function
  | Some err -> failwith err
  | None -> ()

let rec inferer (env : env) (ctx : contexte) : terme -> terme = function
  | Type k -> Type (k + 1)
  | Var n -> (
      match type_of_locale ctx n with
      | Some t -> t
      | None ->
          raise
            (TypeError
               (Printf.sprintf "variable sortie de son contextee: v%d" n)))
  | Const n -> (
      match type_of_const env n with
      | Some t -> t
      | None -> raise (TypeError (Printf.sprintf "constante non définie: %s" n))
      )
  | App (f, a) -> (
      let f_t = inferer env ctx f in
      match whnf env ctx f_t with
      | Pi (arg_t, res_t) ->
          unwrap (check env ctx arg_t a);
          res_t </> a
      | other ->
          raise
            (TypeError
               (Printf.sprintf "On ne peut pas annoter: %s"
                  (string_of_terme other))))
  | Lambda (ann, body) -> raise (TypeError "Pas d'annotation")
  | Pi (a, b) ->
      unwrap (check env ctx (Type 0) a);
      unwrap (check env (ajoute_locale ctx "_" a) (Type 0) b);
      Type 0
  | Let (v_t, v, body) ->
      unwrap (check env ctx v_t v);
      inferer env (ajoute_def ctx "_" v_t v) body

and check (env : env) (ctx : contexte) (expected : terme) :
    terme -> string option =
  let ( <~> ) = fun x y -> alpha_equiv env ctx x y in
  function
  | Lambda (ann, body) -> (
      match whnf env ctx expected with
      | Pi (arg_t, res_t) ->
          if not (ann <~> arg_t) then Some "Le type diffère de l'annotation"
          else check env (Locale ("_", arg_t) :: ctx) res_t body
      | _ -> Some "On voulait une fonction")
  | u ->
      let u_t = inferer env ctx u in
      if not (u_t <~> expected) then
        Some
          (Printf.sprintf "On a %s mais on voulait %s" (string_of_terme u_t)
             (string_of_terme expected))
      else None

and ( |~ ) = fun x y -> check [] [] y x

let demo1 () =
  print_endline "Démonstration (A -> A)";

  (* Construction du type : A -> A ainsi que du lambda terme *)
  let preuve = Pi (Type 0, Type 0) in
  let lambda = Lambda (Type 0, Var 0) in

  match lambda |~ preuve with
  | None ->
      Printf.printf "  Termes ...: %s\n  Type .....: %s\n  C/C ......: OK\n"
        (string_of_terme lambda) (string_of_terme preuve)
  | Some msg -> Printf.printf "  Erreur: %s\n" msg

let demo2 () =
  let t_A = Type 0 in

  (* id = λ (x : A) . x *)
  let id_t = Lambda (t_A, Var 0) in

  (* let id : A -> A := id_t in id A *)
  let def = Let (Pi (t_A, t_A), id_t, App (Var 0, t_A)) in

  match inferer [] [] def with
  | ty ->
      Printf.printf "  Terme ....: %s\n" (string_of_terme def);
      Printf.printf "  WHNF .....: %s\n" (string_of_terme (whnf [] [] def));
      Printf.printf "  Type .....: %s\n" (string_of_terme ty);
      print_endline "  C/C ......: OK"
  | exception TypeError msg -> Printf.printf "  erreur: %s\n" msg

let () =
  while true do
    print_string "mrphsm> ";
    flush stdout;
    let l = try read_line () with End_of_file -> "bye!" in
    match String.trim l with
    | "demo1" -> demo1 ()
    | "demo2" -> demo2 ()
    (* TODO: autres tests *)
    | _ -> ()
  done
