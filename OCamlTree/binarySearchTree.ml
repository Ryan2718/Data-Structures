(* Binary Search Tree
   Ryan Forsyth
   May 10, 2015 *)
   
type 'a tree =
   | Empty
   | Node of 'a * 'a tree * 'a tree


(* Tree Traversals *)

let rec inOrder (t : 'a tree) : 'a list =
   match t with
      Empty -> []
   |  Node (n, left, right) -> inOrder left @ [n] @ inOrder right
   
let rec preOrder (t : 'a tree) : 'a list =
   match t with
      Empty -> []
   |  Node (n, left, right) -> [n] @ preOrder left @ preOrder right
   
let rec postOrder (t : 'a tree) : 'a list =
   match t with
      Empty -> []
   |  Node (n, left, right) -> postOrder left @ postOrder right @ [n]


(* Insertion *)

let rec insert (t : 'a tree) (e : 'a) : 'a tree =
   match t with
      Empty -> Node (e, Empty, Empty)
   |  Node (n, left, right) ->
      if e < n
      then Node (n, (insert left e), right)
      else Node (n, left, (insert right e))

let insertMultiple (t : 'a tree) (es : 'a list) : 'a tree =
   List.fold_left insert t es


(* Searching *)

let rec search (t : 'a tree) (e: 'a) : bool =
   match t with
      Empty -> false
   |  Node (n, left, right) ->
      if e < n
      then search left e
      else if e > n
      then search right e
      else true
