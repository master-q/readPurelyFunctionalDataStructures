type comparison = Less | Equal | Greater

module type OrderedType =
  sig
    type t
    val compare: t -> t -> comparison
  end

module BinomialHeap =
  functor (Ord : OrderedType) ->
    struct
      type elt = Ord.t
      type tree = Node of int * elt * tree list
      type heap = tree list

      let empty = []
      let isEmpty ts = ts = []
      let rank = function
          Node(r, _, _) -> r
      let root = function
          Node(_, x, _) -> x
    end

module OrderedInt =
  struct
    type t = int
    let compare x y = if x = y then Equal else if x < y then Less else Greater
  end
 
module BinomialHeapInt = BinomialHeap(OrderedInt)

let hoge = 1
