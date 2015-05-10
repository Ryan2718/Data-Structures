(** Binary Search Tree *)

(** Ryan Forsyth

    May 10, 2015 *)
    
(** Polymorphic Binary Search Tree *)
type 'a tree =
| Empty (** Empty tree *)
| Node of 'a * 'a tree * 'a tree (** Create a tree out of two subtrees *)


(** {9 Tree Traversals} *)

(** The in-order traversal of the tree *)
val inOrder : 'a tree -> 'a list

(** The pre-order traversal of the tree *)
val preOrder : 'a tree -> 'a list

(** The post-order traversal of the tree *)
val postOrder : 'a tree -> 'a list


(** {9 Insertion} *)

(** Insert an element into the tree *)
val insert : 'a tree -> 'a -> 'a tree

(** Insert multiple elements into the tree *)
val insertMultiple : 'a tree -> 'a list -> 'a tree

(** {9 Searching} *)

(** Search for an item in the tree *)
val search : 'a tree -> 'a -> bool
