OCAML euler problems
=====

### First problem

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

### Implementation of the algorithm using tail recursion
```ocaml
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
```

### Implementations of the algorithm using recursion
```ocaml
let identity n = n ;;
let square n = n * n ;;

let rec sum a b func = 
  if a = b then
    func a
  else 
    sum a ((a + b) / 2) func + sum (1 + (a + b) / 2) b func
;;

let sum_of_n n = sum 0 n identity ;;
let sum_of_n_squared n = sum 0 n square ;;
let euler1 n = square (sum_of_n n) - sum_of_n_squared n ;;
```

### Implementations of the algorithm using collections
```ocaml
let id n = n ;;
let square n = n * n ;;

let rec generate_reversed_list size f =
  match size with
  | 0 -> []
  | n -> f (n - 1) :: generate_reversed_list (n - 1) f
;;

let rec map list f =
  match list with
  | [] -> []
  | head :: tail -> f(head) :: map tail f
;;

let rec sum list = 
  match list with
  | [] -> 0
  | head :: tail -> head + sum tail
;;

let euler1 n = 
  let linear = generate_reversed_list (n + 1) id in
  let squares = map linear square in
  square (sum linear) - sum squares
;;
```

### Second problem

What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

### Implementations of the algorithm using recursion
```ocaml
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
```

### Implementations of the algorithm using collection
```ocaml
let rec generate_fibbonachi_list_impl size a b = 
  match size with
  | 0 -> []
  | n -> (a + b) :: generate_fibbonachi_list_impl (n - 1) b (a + b)
;;

let generate_fibbonachi_list size = 
  match size with
  | 0 -> []
  | 1 -> [ 1 ]
  | 2 -> [ 1; 1 ]
  | n -> 1 :: 1 :: generate_fibbonachi_list_impl (n - 2) 1 1

let rec map list f =
  match list with
  | [] -> []
  | head :: tail -> f(head) :: map tail f
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

let rec find_number_in_array_impl seq target index = 
  match seq with
  | [] -> -1
  | head :: tail -> if head >= target then index else find_number_in_array_impl tail target (index + 1)
;;

let find_number_in_array seq target = find_number_in_array_impl seq target 0 ;;

let find_number_with_digits seq target_digits = 
  let digits_seq = map seq digits_of in
    find_number_in_array digits_seq target_digits
;;

let rec find_fib_with_target_digits n target_digits = 
  let fib = generate_fibbonachi_list n in 
    let index = find_number_with_digits fib target_digits in 
      if index = -1 then
        find_fib_with_target_digits (2 * n) target_digits
      else
        1 + index
;;

let euler2 n = find_fib_with_target_digits 1 n ;;
```


### Implementations of the algorithm using collection using only pattern matching
```ocaml
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

```
