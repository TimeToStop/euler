open! OUnit2
open! Collection
open! Recursion
open! Tailrecursion

let solve n =
  (n * n * (n + 1) * (n + 1) / 4) - (n * (n + 1) * ((2 * n) + 1) / 6)

let rec test_any n solution =
  match n with
  | -1 -> ()
  | value ->
      assert_equal (solve value) (solution value) ;
      test_any (value - 1) solution

let test_collection _ = test_any 10000 Collection.euler1

let test_recursion _ = test_any 10000 Recursion.euler1

let test_tail_recursion _ = test_any 10000 Tailrecursion.euler1

let test_euler_1 =
  "test_euler_1"
  >::: [ "test_collection" >:: test_collection
       ; "test_recursion" >:: test_recursion
       ; "test_tail_recursion" >:: test_tail_recursion ]

let () = run_test_tt_main test_euler_1
