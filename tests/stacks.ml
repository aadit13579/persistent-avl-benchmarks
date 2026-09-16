(*Implementation of stack in Ocaml*)

let push element stack=
  element::stack 

let pop stack =
  match stack with 
  | []-> failwith "empty stack"
  | [element]-> []
  | element::rest -> rest

let peek stack=
  match stack with 
  | []-> failwith "empty stack"
  | [element]-> element
  | element::rest -> element

let is_empty stack=
 match stack with 
 | []-> true
 | _-> false

let rec size_of stack =
  match stack with 
  | []-> 0  
  | element::rest -> 1+ size_of rest

let clear stack =[]

