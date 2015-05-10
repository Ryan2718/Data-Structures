open OUnit2
open BinarySearchTree

(* See http://ounit.forge.ocamlcore.org/api-ounit/ *)

let bst = insertMultiple Empty [1; 2; 34; 5; 1729; 20; 19; 3]

let testInOrder test_ctxt =
   assert_equal [1; 2; 3; 5; 19; 20; 34; 1729] (inOrder bst)
   
let testPreOrder test_ctxt =
   assert_equal [1; 2; 34; 5; 3; 20; 19; 1729] (preOrder bst)
   
let testPostOrder test_ctxt =
   assert_equal [3; 19; 20; 5; 1729; 34; 2; 1] (postOrder bst)

let testSearch1 test_ctxt = assert_equal true (search bst 20)
let testSearch2 test_ctxt = assert_equal false (search bst 42)

let suite =
"suite">:::
   ["testInOrder">:: testInOrder;
    "testPreOrder">:: testPreOrder;
    "testPostOrder">:: testPostOrder;
    "testSearch1">:: testSearch1;
    "testSearch2">:: testSearch2]
    
let () =
   run_test_tt_main suite
