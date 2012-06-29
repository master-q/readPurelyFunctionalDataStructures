open List
open Small_stream

exception RealTimeQueue_notmatch
exception RealTimeQueue_empty

module RTQueue = struct
  type 'a tree = 'a stream * 'a list * 'a stream

  let empty = (lazy SSnil, [], lazy SSnil)
  let isEmpty = function
    | (lazy SSnil, _, _) -> true
    | _                  -> false
  let rec rotate = function
    | (lazy SSnil, y :: _, a) -> lazy (SScons(y, a))
    | (lazy (SScons(x, xs)), y :: ys, a)
      -> lazy (SScons(x, rotate (xs, ys, (lazy (SScons(y, a))))))
    | _ -> raise RealTimeQueue_notmatch
  let exec = function
    | (f, r, lazy (SScons(x, s))) -> (f, r, s)
    | (f, r, lazy SSnil) -> let f' = rotate (f, r, lazy SSnil) in (f', [], f')
  let snoc (f, r, s) x = exec (f, x :: r, s)
  let head = function
    | (lazy SSnil, r, s)          -> raise RealTimeQueue_empty
    | (lazy (SScons(x, f)), r, s) -> x
  let tail = function
    | (lazy SSnil, r, s)          -> raise RealTimeQueue_empty
    | (lazy (SScons(x, f)), r, s) -> exec (f, r, s)
end
