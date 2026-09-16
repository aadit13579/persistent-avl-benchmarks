type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t =
  Node (10,
    Node (5, Empty, Empty),
    Node (20, Node(12,Node(11,Empty,Empty),Empty), Empty))

let rec insert x tree =
  match tree with
  | Empty -> Node(x,Empty,Empty)
  | Node (value, left, right) ->
      if x < value then
        Node(value, insert x left, right)
      else if x > value then
        Node(value, left, insert x right)
      else tree

let rec search x tree=	
  match tree with
  | Empty -> false
  | Node (value, left, right) -> 
    if x<value then 
      search x left
    else if x>value then
      search x right
    else true

let rec find_min tree=
    match tree with 
    | Empty -> failwith "empty tree !"
    | Node(value,Empty, _)-> value
    | Node(_,left,_)-> find_min left

let rec delete_node x tree=
    match tree with 
    | Empty -> Empty
    | Node (value, left, right) -> 
    if x<value then 
      Node (value, delete_node x left, right)
    else if x>value then
      Node( value, left, delete_node x right)
    else 
    match (left, right) with
    | (Empty, Empty) -> Empty
    | (Empty, right) -> right
    | (left, Empty) -> left
    | (left,right)->
        let successor= find_min right in 
        Node (successor, left, delete_node successor right)
