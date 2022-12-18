open! Collection ;;
open! Recursion ;;
open! Tailrecursion ;;

let solve n = n * n * (n + 1) * (n + 1) / 4 - n * (n + 1) * (2 * n + 1) / 6;;

let test_solution_of_value value solution = 
  if solution value != solve value then begin
    print_string "Test " ;
    print_string " euler1(" ; 
    print_int value ;
    print_string ") = " ;
    print_int (solution value) ;
    print_string ", expected " ;
    print_int (solve value) ;
    print_string " FAILED" ;
    print_newline () ;
    exit 1  
  end else () 
;;

let rec test_solution_in_range a b solution = 
  test_solution_of_value a solution ;

  if a != b then
    test_solution_in_range (a + 1) b solution 
;;

let test_solution solution title = 
  print_string "Testing \"" ;
  print_string title ;
  print_string "\":" ;
  print_newline () ;
  test_solution_in_range 0 10000 solution ;
  print_string "PASSED" ;
  print_newline () ;
;;

let () = 
  test_solution Collection.euler1 "COLLECTION" ;
  test_solution Recursion.euler1 "RECURSION" ;
  test_solution Tailrecursion.euler1 "TAIL RECURSION" ;
;;