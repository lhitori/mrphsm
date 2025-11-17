type vrai = unit
type faux = |

type ('a, 'b) et = 'a * 'b
type ('a, 'b) ou = Gauche of 'a | Droite of 'b
type ('a, 'b) impl = 'a -> 'b
type 'a non = 'a -> faux


let f x = x


let f (f1: 'a -> 'b) (f2: 'b -> 'c) : 'a -> 'c =
  fun x -> f2 (f1 x)

let f (e : ('a, 'b) et) : ('b, 'a) et = 
  let (x, y) = e in (y, x)


let f (o : ('a, 'b) ou) : ('b, 'a) ou = 
  match o with
  | Gauche a -> Droite a
  | Droite b -> Gauche b


let f (f1: 'a -> 'b) : 'b non -> 'a non =
  fun (x : 'b non) ->
    fun (a : 'a) ->
      x (f1 a)