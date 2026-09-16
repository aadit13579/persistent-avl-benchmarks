type 'a tree =
  | Empty
  | Node of 'a * int * 'a tree * 'a tree

let height = function
  | Empty->0
  | Node(_,h,_,_)->h

let make_node x left right =
  let h= 1+ max (height left) (height right) in
  Node(x,h,left,right)

let left_rotate node=
  match node with
  | Node(x,_,a,Node(y,_,b,c))->
      make_node y (make_node x a b) c
  | _ -> failwith "rotate_left: invalid rotation"

let right_rotate node=
  match node with
  | Node(x,_,Node(y,_,a,b),c)->
      make_node y a (make_node x b c) 
  | _ -> failwith "rotate_right: invalid rotation"

let balance_factor node =
  match node with
  | Empty -> 0
  | Node (_, _, l, r) -> height l - height r

let balance node=
  match node with
  | Empty->Empty
  | Node(x,_,l,r)->
      let bf= balance_factor node in 
      if bf=2 then 
        (match l with 
          |Node(_,_,_,_) when balance_factor l<0->
            let new_left = left_rotate l in 
            right_rotate(make_node x new_left r)
          | _->right_rotate node)
      else if bf= -2 then
        (match r with 
          |Node(_,_,_,_) when balance_factor r>0->
            let new_right=right_rotate r in
            left_rotate(make_node x l new_right)
          | _->left_rotate node)
      else
        node

let rec insert x tree =
  match tree with
  | Empty -> make_node x Empty Empty
  | Node (value, _, left, right) ->
      if x < value then
        balance (make_node value (insert x left) right)
      else if x > value then
        balance (make_node value left (insert x right))
      else
        tree

let rec find_min tree =
  match tree with
  | Empty -> failwith "empty tree!"
  | Node (value, _, Empty, _) -> value
  | Node (_, _, left, _) -> find_min left

let rec delete x tree =
  match tree with
  | Empty -> Empty
  | Node (value, _, left, right) ->
      if x < value then
        balance (make_node value (delete x left) right)
      else if x > value then
        balance (make_node value left (delete x right))
      else
        match (left, right) with
        | (Empty, Empty) -> Empty
        | (Empty, r) -> r
        | (l, Empty) -> l
        | (l, r) ->
            let successor = find_min r in
            balance (make_node successor l (delete successor r))