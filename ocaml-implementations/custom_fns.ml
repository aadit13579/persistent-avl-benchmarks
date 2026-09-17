type direction =
  | Left
  | Right

type shape=
  | Circle of int 
  | Rectangle of int*int

let mynewrectangle= Rectangle(5,5)
let mynewcircle= Circle 1

let area shape =
  match shape with
  | Circle r -> float_of_int r *. 3.14 *. float_of_int r
  | Rectangle (len, breadth) -> float_of_int (len * breadth)

let perimeter shape=
  match shape with
  | Circle r -> float_of_int 2 *. 3.14 *. float_of_int r
  | Rectangle (len, breadth) -> float_of_int 2 *. float_of_int (len + breadth)

let describe shape=
  match shape with
  | Circle r -> "This is a circle"
  | Rectangle (len, breadth) -> "This is a rectangle"

let is_circle shape=
  match shape with
  | Circle r -> true
  | _-> false

let larger_area shape1 shape2=
  let area1 = area  shape1 in 
  let area2 =area shape2 in
  if area1>area2 then area1
  else area2

let rec count_circles shapelist=
  match shapelist with
  | []-> 0
  | u::v -> 
    match u with
    | Circle _ -> 1+ count_circles v
    | _-> count_circles v

let total_area shape1 shape2 =
  let area1 = area shape1 in
  let area2 = area shape2 in
  area1 +. area2

type 'a mylist =
  | Empty
  | Cons of 'a * 'a mylist

type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t =
  Node (10,
    Node (5, Node(1,Empty,Empty), Empty),
    Node (20, Empty, Empty))

