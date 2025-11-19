exception TypeError of string

(* Défintion inductive d'un terme *)
type terme =
  | Var of int
  | Type of int
  | Pi of terme * terme
  | Lambda of terme * terme
  | App of terme * terme

and env = (string * terme) list
and annotation = Locale of char * terme (* x : A *)
and contexte = annotation list

let type_of_locale (ctx : contexte) (n : int) : terme option =
  match List.nth_opt ctx n with Some (Locale (_, t)) -> Some t | None -> None

and type_of_const (env : env) (n : string) : terme option = List.assoc_opt n env

and string_of_terme (t : terme) : string =
  let rec s (i : int) : terme -> string = function
    | Var n -> Printf.sprintf "v%d" n
    | Type k -> Printf.sprintf "T%d" k
    | Pi (a, b) -> Printf.sprintf "(π %s -> %s)" (s i a) (s (i + 1) b)
    | Lambda (a, b) ->
        Printf.sprintf "(λ v%d : %s . %s)" i (s i a) (s (i + 1) b)
    | App (lhs, rhs) -> Printf.sprintf "(%s %s)" (s i lhs) (s i rhs)
  in
  s 0 t

(* Décalage des indices de variables de De Bruijn *)
let rec shift (d : int) (c : int) = function
  | Var k -> if k >= c then Var (k + d) else Var k
  | Pi (a, b) -> Pi (shift d c a, shift d (c + 1) b)
  | Lambda (arg_t, corps) -> Lambda (shift d c arg_t, shift d (c + 1) corps)
  | App (lhs, rhs) -> App (shift d c lhs, shift d c rhs)
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
    | x -> x
  in
  shift 1 0 s |> aux t 0 |> shift (-1) 0

(* Forme normale faible: https://j-hui.com/pages/normal-forms/ *)
let rec whnf (env : env) (ctx : contexte) : terme -> terme = function
  | App (lhs, rhs) -> (
      match whnf env ctx lhs with
      | Lambda (_, corps) -> whnf env ctx (corps </> rhs)
      | lhs' -> App (lhs', rhs))
  | x -> x

(* alpha-equivalence: https://inria.hal.science/hal-01354360/file/RR-LIG-013.pdf *)
(* Test l'alpha equivalence entre deux termes *)
let alpha_equiv (env : env) (ctx : contexte) (x : terme) (y : terme) =
  let rec aux (ctx : contexte) (t : terme) : terme =
    match whnf env ctx t with
    | App (lhs, rhs) -> App (aux ctx lhs, aux ctx rhs)
    | Pi (a, b) -> Pi (aux ctx a, aux (Locale ('_', a) :: ctx) b)
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

let () =
  while true do
    print_string "<µ> ";
    flush stdout;
    let l = try read_line () with End_of_file -> "bye!" in
    match String.trim l with
    | "demo1" -> demo1 ()
    (* TODO: autres tests *)
    | _ -> ()
  done
