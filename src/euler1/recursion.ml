let identity n = n

let square n = n * n

let rec sum a b func =
  if a = b then func a
  else sum a ((a + b) / 2) func + sum (1 + ((a + b) / 2)) b func

let sum_of_n n = sum 0 n identity

let sum_of_n_squared n = sum 0 n square

let euler1 n = square (sum_of_n n) - sum_of_n_squared n
