let rec generate_fibbonachi_list_impl size a b =
  match size with
  | 0 -> []
  | n -> (a + b) :: generate_fibbonachi_list_impl (n - 1) b (a + b)

let generate_fibbonachi_list size =
  match size with
  | 0 -> []
  | 1 -> [1]
  | 2 -> [1; 1]
  | n -> 1 :: 1 :: generate_fibbonachi_list_impl (n - 2) 1 1

let rec map list f =
  match list with [] -> [] | head :: tail -> f head :: map tail f

let rec digits_of_impl value =
  if value = 0 then 0 else 1 + digits_of_impl (value / 10)

let digits_of value = if value = 0 then 1 else digits_of_impl value

let rec find_number_in_array_impl seq target index =
  match seq with
  | [] -> -1
  | head :: tail ->
      if head >= target then index
      else find_number_in_array_impl tail target (index + 1)

let find_number_in_array seq target = find_number_in_array_impl seq target 0

let find_number_with_digits seq target_digits =
  let digits_seq = map seq digits_of in
  find_number_in_array digits_seq target_digits

let rec find_fib_with_target_digits n target_digits =
  let fib = generate_fibbonachi_list n in
  let index = find_number_with_digits fib target_digits in
  if index = -1 then find_fib_with_target_digits (2 * n) target_digits
  else 1 + index

let euler2 n = find_fib_with_target_digits 1 n
