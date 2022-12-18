let square n = n * n ;;

let rec summ n = 
  match n with
  | 0 -> 0
  | n -> n + summ (n - 1)
;;

let rec sum_of_squares n =
  match n with
  | 0 -> 0
  | n -> n * n + sum_of_squares (n - 1)
;;

let euler1 n = 
  square (summ n) - sum_of_squares n
;;