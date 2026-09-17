let rec length xs =
  match xs with
  | [] -> 0
  | _ :: xs -> 1 + length xs

let rec sum xs =
  match xs with
  | [] -> 0
  | x :: xs -> x + sum xs

let rec product xs=
  match xs with
	| []->1
	| x::v -> x* product v 

let rec contains x xs =
  match xs with
  | [] -> false
  | y :: ys ->
      if x = y then true
      else contains x ys

let rec mapmyfn f xs=
	match xs with 
	| []->[]
	| x::v-> f x :: mapmyfn f v

let square x = x * x

let xfold fold x =
  let result = ref 1 in
  for i = 1 to fold do
    result := !result * x
  done;
  !result
module StringMap = Map.Make(String);;
let m =
  StringMap.empty
  |> StringMap.add "apple" 2
  |> StringMap.add "banana" 3
  |> StringMap.add "orange" 8;;

let even_numbers a=
	StringMap.filter (fun _ number -> number mod 2 = 0) a


let greater_than_100_and_even a=
	List.filter (fun x-> x>100 && x mod 2=0) a

let max a=
	let mymax= ref 0 in 
	List.iter (fun i ->
    if i > !mymax then mymax := i
  ) a;
	!mymax

let rec max_rec a =
  match a with
  | [] -> failwith "empty list"
  | [x] -> x
  | x :: xs ->
      let m = max xs in
      if x > m then x else m

let rec is_sorted xs =
  match xs with
  | [] -> failwith "empty list"
  | [_] -> true
  | x :: y :: rest ->
      if x <= y then
        is_sorted (y :: rest)
      else
        false

let rec insert_sorted element xs =
  match xs with
  | [] -> [element]
	| x::rest-> if element<=x then element::x::rest else x::insert_sorted element rest 

(*Merging two sorted lists*)
let rec merge_sorted list1 list2=
  match (list1,list2) with 
  | ([],[])->[]
	| (x::xs,[])->x::xs
	| ([],y::ys)->y::ys
	| (x::xs,y::ys)-> 
      if x<=y then x:: merge_sorted xs (y::ys)
      else y::merge_sorted (x::xs) ys 