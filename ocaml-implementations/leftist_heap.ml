type 'a heap =
  | Empty
  | Node of 'a * int * 'a heap * 'a heap

let rank = function
  | Empty -> 0
  | Node (_, r, _, _) -> r

let make_node x left right =
  if rank left >= rank right then
    Node (x, 1 + rank right, left, right)
  else
    Node (x, 1 + rank left, right, left)


(* My merge logic: we have 2 heaps of type a,_,l1,r1 and b,_,l2,r2 when we merge them we compare a,b, if a<b new root=a so new tree will be a,_,l1,merge(r1,heap2) *)
let rec merge h1 h2 =
  match (h1, h2) with
  | (Empty, h) -> h
  | (h, Empty) -> h
  | (Node (a, _, l1, r1), Node (b, _, l2, r2)) ->
      if a <= b then
        make_node a l1 (merge r1 h2)
      else
        make_node b l2 (merge r2 h1)

let singleton x = make_node x Empty Empty

let insert x heap = merge (singleton x) heap

let delete_min heap =
  match heap with
  | Empty -> failwith "empty heap!"
  | Node (x, _, left, right) -> (x,merge left right)