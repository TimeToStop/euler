let id n = n

let square n = n * n

let rec generate_reversed_list size f =
  match size with
  | 0 -> []
  | n -> f (n - 1) :: generate_reversed_list (n - 1) f

let rec map list f =
  match list with [] -> [] | head :: tail -> f head :: map tail f

let rec sum list =
  match list with [] -> 0 | head :: tail -> head + sum tail

let euler1 n =
  let linear = generate_reversed_list (n + 1) id in
  let squares = map linear square in
  square (sum linear) - sum squares
