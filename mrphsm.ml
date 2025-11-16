exception TypeError of string

(* Défintion inductive d'un terme *)
type terme =
  | Var of int
  | Type of int
  | Const of string (* constante *)
  | Pi of terme * terme (* (x : A) -> B *)
  | Lambda of terme * terme (* Lambda (x : A) . X *)
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
    | App (f, x) -> Printf.sprintf "(%s %s)" (s i f) (s i x)
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
  | App (f, x) -> App (shift d c f, shift d c x)
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
    | App (f, x) -> App (s <~ f, s <~ x)
    | Let (a, t, u) -> Let (s <~ a, s <~ t, shift 1 0 s <~~ u)
    | x -> x
  in

  shift (-1) 0 (aux 0 (shift 1 0 s) t)

(* forme normale faible: https://j-hui.com/pages/normal-forms/ *)
let rec whnf (env : env) (ctx : contexte) : terme -> terme = function
  | App (f, a) -> (
      match whnf env ctx f with
      | Lambda (_, u) -> whnf env ctx (u </> a)
      | f' -> App (f', a))
  | Let (_, v, u) -> whnf env ctx (u </> v)
  | x -> x

let alpha_equiv (env : env) (ctx : contexte) (t : terme) (u : terme) =
  let rec aux (ctx : contexte) (t : terme) =
    match whnf env ctx t with
    | App (f, a) -> App (aux ctx f, aux ctx a)
    | Pi (a, b) -> Pi (aux ctx a, aux (Locale ("_", a) :: ctx) b)
    | Lambda (x, y) -> Lambda (aux ctx x, aux (Locale ("_", x) :: ctx) y)
    | x -> x
  in
  aux ctx t = aux ctx u

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
          check env ctx a arg_t;
          res_t </> a
      | other ->
          raise
            (TypeError
               (Printf.sprintf "On ne peut pas annoter: %s"
                  (string_of_terme other))))
  | Lambda (ann, body) -> raise (TypeError "Pas d'annotation")
  | Pi (a, b) ->
      check env ctx a (Type 0);
      check env (ajoute_locale ctx "_" a) b (Type 0);
      Type 0
  | Let (ty, v, body) ->
      check env ctx v ty;
      inferer env (ajoute_def ctx "_" ty v) body

and check (env : env) (ctx : contexte) (t : terme) (expected : terme) : unit =
  let ( === ) = fun x y -> alpha_equiv env ctx x y in
  match t with
  | Lambda (ann, body) -> (
      match whnf env ctx expected with
      | Pi (arg_ty, res_ty) ->
          if not (ann === arg_ty) then
            raise (TypeError "Le type diffère de l'annotation ");
          check env (Locale ("_", arg_ty) :: ctx) body res_ty
      | _ -> raise (TypeError "On voulait une fonction"))
  | _ ->
      let t_ty = inferer env ctx t in
      if not (t_ty === expected) then
        raise
          (TypeError
             (Printf.sprintf "On a %s mais on voulait %s" (string_of_terme t_ty)
                (string_of_terme expected)))

and ( <?> ) = fun x y -> check [] [] x y

let demo1 () =
  print_endline "Démonstration (A -> A)";
  let t_A = Type 0 in

  (* Construction du type : A -> A ainsi que du lambda terme *)
  let tpe = Pi (t_A, t_A) in
  let trm = Lambda (t_A, Var 0) in

  (* On vérifie que id_terme prouve bien id_type *)
  match check [] [] trm tpe with
  | () ->
      Printf.printf "  Termes .: %s\n  Type ...: %s\n  C/C ....: OK\n"
        (string_of_terme trm) (string_of_terme tpe)
  | exception TypeError msg -> Printf.printf "  ERREUR: %s\n" msg

let demo2 () =
  print_endline "Démonstration (A -> B -> A) : fonction constante";
  let t_A = Type 0 in
  let t_B = Type 0 in
  let tpe = Pi (t_A, Pi (t_B, t_A)) in
  let trm = Lambda (t_A, Lambda (t_B, Var 1)) in
  match trm <?> tpe with
  | () ->
      Printf.printf "  Termes .: %s\n  Type ...: %s\n  C/C ....: OK\n"
        (string_of_terme trm) (string_of_terme tpe)
  | exception TypeError msg -> Printf.printf "  ERREUR: %s\n" msg

let demo3 () =
  print_endline "Démonstration ((A -> B) -> (B -> C) -> (A -> C)) : composition";
  let t_A = Type 0 in
  let t_B = Type 0 in
  let t_C = Type 0 in
  let tpe = Pi (Pi (t_A, t_B), Pi (Pi (t_B, t_C), Pi (t_A, t_C))) in
  let trm =
    Lambda
      ( Pi (t_A, t_B),
        Lambda (Pi (t_B, t_C), Lambda (t_A, App (Var 1, App (Var 2, Var 0)))) )
  in
  match trm <?> tpe with
  | () ->
      Printf.printf "  Termes .: %s\n  Type ...: %s\n  C/C ....: OK\n"
        (string_of_terme trm) (string_of_terme tpe)
  | exception TypeError msg -> Printf.printf "  %s\n" msg

let demo4 () =
  print_endline "Démonstration avec erreur attendue";
  let t_A = Type 0 in
  let t_B = Type 1 in
  (* Mauvais type : prétend retourner A, mais retourne argument de type B *)
  let tpe = Pi (t_B, t_A) in
  let trm = Lambda (t_B, Var 0) in
  match trm <?> tpe with
  | () -> Printf.printf "  gros probleme\n"
  | exception TypeError msg -> Printf.printf "  rejet avec message: %s\n" msg

let demo5 () =
  print_endline "Démonstration : application ( (λ (x:A). x) A )";

  let app = App (Lambda (Type 0, Var 0), Type 0) in
  match inferer [] [] app with
  | ty ->
      Printf.printf "  Terme .: %s\n  WHNF ..: %s\n  Type ..: %s\n"
        (string_of_terme app)
        (string_of_terme (whnf [] [] app))
        (string_of_terme ty)
  | exception TypeError msg -> Printf.printf "  ERREUR: %s\n" msg

let () =
  while true do
    print_string "mrphsm> ";
    flush stdout;
    let l = try read_line () with End_of_file -> "bye!" in
    match String.trim l with
    | "demo1" -> demo1 ()
    | "demo2" -> demo2 ()
    | "demo3" -> demo3 ()
    | "demo4" -> demo4 ()
    | "demo5" -> demo5 ()
    | _ -> ()
  done
