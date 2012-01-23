type color = R | B
type 'a tree = E | T of color * 'a tree * 'a * 'a tree

let rec member x t = match t with
  | E             -> false
  | T(_, a, y, b) ->
    if x < y then member x a
    else if x > y then member x b else true

let llbalance c t1 x t2 = match (c, t1, x, t2) with
  | (B, T(R, T(R,a,x,b), y, c), z, d) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (c, t1, x, t2) -> T(c, t1, x, t2)

let lrbalance c t1 x t2 = match (c, t1, x, t2) with
  | (B, T(R, a, x, T(R,b,y,c)), z, d) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (c, t1, x, t2) -> T(c, t1, x, t2)

let rlbalance c t1 x t2 = match (c, t1, x, t2) with
  | (B, a, x, T(R, T(R,b,y,c), z, d)) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (c, t1, x, t2) -> T(c, t1, x, t2)

let rrbalance c t1 x t2 = match (c, t1, x, t2) with
  | (B, a, x, T(R, b, y, T(R,c,z,d))) -> T(R, T(B,a,x,b), y, T(B,c,z,d))
  | (c, t1, x, t2) -> T(c, t1, x, t2)

let insert x s =
  let rec insl q = match q with
    | T(color, E, v, r) -> T(color, T(R, E, x, E), v, r)
    | T(color, (T(_, _, v2, _) as l), v, r) as n ->
      if x < v2 then llbalance color (insl l) v r
      else if x > v2 then lrbalance color (insr l) v r else n
    | E -> T(R, E, x, E)
  and insr q = match q with
    | T(color, l, v, E) -> T(color, l, v, T(R, E, x, E))
    | T(color, l, v, (T(_, _, v2, _) as r)) as n ->
      if x < v2 then rlbalance color l v (insl r)
      else if x > v2 then rrbalance color l v (insr r) else n
    | E -> T(R, E, x, E) in
  let ins q = match q with
    | E -> T(R, E, x, E)
    | T(_, _, v, _) as n ->
      if x < v then insl n
      else if x > v then insr n else n in
  match ins s with
    | T(_, a, y, b) -> T(B, a, y, b)
    | E -> assert false
