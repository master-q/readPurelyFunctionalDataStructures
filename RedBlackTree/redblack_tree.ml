type color = R | B
type 'a tree = E | T of color * 'a tree * 'a * 'a tree

let rec member x t = match t with
  | E             -> false
  | T(_, a, y, b) ->
    if x < y then member x a
    else if x > y then member x b else true

let balance c t1 x t2 = match (c, t1, x, t2) with
  | (B, T(R, T(R,a,x,b), y, c), z, d) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (B, T(R, a, x, T(R,b,y,c)), z, d) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (B, a, x, T(R, T(R,b,y,c), z, d)) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (B, a, x, T(R, b, y, T(R,c,z,d))) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (c, t1, x, t2) -> T(c, t1, x, t2)

let insert x s =
  let rec ins q = match q with
    | E -> T(R, E, x, E)
    | T(color, a, y, b) as s ->
      if x < y then balance color (ins a) y b
      else if x > y then balance color a y (ins b) else s in
  match ins s with
    | T(_, a, y, b) -> T(B, a, y, b)
    | E -> assert false
