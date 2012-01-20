type color = R | B
type 'a tree = E | T of color * 'a tree * 'a * 'a tree
type way = Left | Right | DontKnow

let rec member x t = match t with
  | E             -> false
  | T(_, a, y, b) ->
    if x < y then member x a
    else if x > y then member x b else true

let lbalance c wt1 x t2 = match (c, wt1, x, t2) with
  | (B, (Left, T(R, T(R,a,x,b), y, c)), z, d) ->
    T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (B, (Right, T(R, a, x, T(R,b,y,c))), z, d) ->
    T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (c, (_, t1), x, t2) -> T(c, t1, x, t2)

let rbalance c t1 x wt2 = match (c, t1, x, wt2) with
  | (B, a, x, (Left, T(R, T(R,b,y,c), z, d))) ->
    T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (B, a, x, (Right, T(R, b, y, T(R,c,z,d)))) ->
    T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (c, t1, x, (_, t2)) -> T(c, t1, x, t2)

let insert x s =
  let rec ins q = match q with
    | E -> (DontKnow, T(R, E, x, E))
    | T(color, a, y, b) as s ->
      if x < y then (Left, lbalance color (ins a) y b) else
	if x > y then (Right, rbalance color a y (ins b))
	else (DontKnow, s) in
  match ins s with
    | (_, T(_, a, y, b)) -> T(B, a, y, b)
    | (_, E) -> assert false
