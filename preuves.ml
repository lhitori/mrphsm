(* Merci Mr Hutin <3 <3 *)

type vrai = unit
type faux = |
type ('a, 'b) et = 'a * 'b
type ('a, 'b) ou = Gauche of 'a | Droite of 'b
type ('a, 'b) impl = 'a -> 'b
type 'a non = 'a -> faux

(* p -> p*)
let f (x : 'a) : 'a = x

(* ((p->q)->(q->r))->(p->r) *)
let f (f1 : 'a -> 'b) (f2 : 'b -> 'c) : 'a -> 'c = fun x -> f2 (f1 x)

(* (p /\ q) = (q /\ p)*)
let f (e : ('a, 'b) et) : ('b, 'a) et =
  let x, y = e in
  (y, x)

(* (p \/ q) = (q \/ p) *)
let f (o : ('a, 'b) ou) : ('b, 'a) ou =
  match o with Gauche a -> Droite a | Droite b -> Gauche b

(* (p -> q) -> (non p -> non q) *)
let f (f1 : 'a -> 'b) : 'b non -> 'a non =
 fun (x : 'b non) -> fun (a : 'a) -> x (f1 a)
