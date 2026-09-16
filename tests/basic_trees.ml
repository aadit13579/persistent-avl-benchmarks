type 'a tree =
  | Empty
  | Node of 'a * 'a tree * 'a tree

let t =
  Node (10,
    Node (5, Node(1,Empty,Empty), Empty),
    Node (20, Node(2,Empty,Empty), Empty))
  

let rec size tree=
  match tree with
  | Empty->0
  | Node(value, left,right)-> 1+ size(left)+size(right)    

let rec height tree =
  match tree with
  | Empty -> -1
  | Node (_, left, right) ->
      1 + max (height left) (height right)

let rec inorder tree=
    match tree with 
    | Empty-> ()
    | Node(value, left,right)-> 
        inorder left;
        print_string (string_of_int value^" ");
        inorder right

let rec preorder tree=
    match tree with 
    | Empty-> ()
    | Node(value, left,right)-> 
        print_string (string_of_int value^" ");
        preorder left;
        preorder right

let rec postorder tree=
    match tree with 
    | Empty-> ()
    | Node(value, left,right)-> 
        postorder left;
        postorder right;
        print_string (string_of_int value^" ")

