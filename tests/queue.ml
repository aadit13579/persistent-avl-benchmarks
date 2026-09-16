(*Implementation of queue in Ocaml*)

let enqueue element queue =
  element::queue

let rec dequeue queue=
 match queue with 
 | []-> failwith "empty queue"
 | [element]-> [] 
 | x::rest-> x:: dequeue rest

let rec peek queue=
 match queue with 
 | []-> failwith "empty queue"
 | [element]-> element 
 | _::rest-> peek rest

let rec size queue=
  match queue with 
  | []-> 0
  | u::v-> 1+ size v

let clear queue=[]