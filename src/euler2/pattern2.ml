let rec generate_fibbonachi_list_impl a b = function
  | 0 -> []
  | size -> (a + b) :: generate_fibbonachi_list_impl b (a + b) (size - 1)

let generate_fibbonachi_list = function
  | 0 -> []
  | 1 -> [1]
  | 2 -> [1; 1]
  | n -> 1 :: 1 :: generate_fibbonachi_list_impl 1 1 (n - 2)

let rec map f = function [] -> [] | head :: tail -> f head :: map f tail

let rec digits_of_impl = function 0 -> 0 | x -> 1 + digits_of_impl (x / 10)

let digits_of = function 0 -> 1 | x -> digits_of_impl x

let rec find_number_in_array_impl target index = function
  | [] -> -1
  | head :: _ when head >= target -> index
  | _ :: tail -> find_number_in_array_impl target (index + 1) tail

let find_number_in_array target seq = find_number_in_array_impl target 0 seq

let find_number_with_digits target_digits seq =
  let digits_seq = map digits_of seq in
  find_number_in_array target_digits digits_seq

let rec find_fib_with_target_digits_impl n target_digits = function
  | -1 -> find_fib_with_target_digits (2 * n) target_digits
  | index -> 1 + index

and find_fib_with_target_digits n target_digits =
  let fib = generate_fibbonachi_list n in
  let index = find_number_with_digits target_digits fib in
  find_fib_with_target_digits_impl n target_digits index

let euler2 n = find_fib_with_target_digits 1 n
