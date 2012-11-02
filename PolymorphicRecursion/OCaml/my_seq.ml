type 'a my_seq = Nil | Cons of 'a * ('a * 'a) my_seq

let rec sizeS : 'a. 'a my_seq -> int = function
  | Nil -> 0
  | Cons (_, my_seq) -> 1 + 2 * sizeS my_seq
