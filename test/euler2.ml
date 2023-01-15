open! OUnit2 ;;

open! Collection2 ;;
open! Recursion2 ;;

let answers = [1 ; 7 ; 12 ; 17 ; 21 ; 26 ; 31 ; 36 ; 40 ; 45] ;;

let rec test_any question answers solution = 
  match answers with
  | [] -> ()
  | head :: tail -> assert_equal (solution question) head ; test_any (question + 1) tail solution
;;

let test_collection _ = 
  test_any 1 answers Collection2.euler2
;;

let test_recursion _ = 
  test_any 1 answers Recursion2.euler2
;;

let test_euler_2 = 
"test_euler_2" >:::
  [
    "test_collection" >:: test_collection ;
    "test_recursion" >:: test_recursion 
  ]
;;

let () = 
  run_test_tt_main test_euler_2
;;