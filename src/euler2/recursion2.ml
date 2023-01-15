let rec fib n = 
  match n with
  | 1 -> 1
  | 2 -> 1
  | n -> fib(n - 1) + fib(n - 2) 
;;

let rec digits_of_impl value = 
  if value = 0 then
    0
  else
    1 + digits_of_impl (value / 10)
;;

let digits_of value =
  if value = 0 then
    1
  else
    digits_of_impl value
;;

let sum a b = a + b ;;

let rec calculate_fib_with_digits_impl a b target_digits index =
  if digits_of b >= target_digits then
    1 + index
  else
    calculate_fib_with_digits_impl b (a + b) target_digits (1 + index)
;;

let calculate_fib_with_digits a b target_digits index = 
  if digits_of a >= target_digits then
    index
  else if digits_of b >= target_digits then
    index + 1
  else
    calculate_fib_with_digits_impl b (a + b) target_digits (index + 1)
;;

let euler2 n = calculate_fib_with_digits 1 1 n 1;;