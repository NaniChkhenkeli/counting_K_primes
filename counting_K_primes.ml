let p_factors n =
  let rec loop acc k n =
    if n <= 1 || k > n then 
      acc
    else if n mod k = 0 then
      loop (succ acc) k (n / k)
    else
      loop acc (succ k) n in
  loop 0 2 n

let countKprimes k start nd = 
  let rec loop acc i =
    if i > nd then List.rev acc
    else if p_factors i = k then
      loop (i :: acc) (succ i)
    else
      loop acc (succ i) in
  loop [] start

let puzzle s =
  let p7 = countKprimes 7 128 (s - 10) in
  let p3 = countKprimes 3 8 (s - 130) in
  let rec loop3 acc v = function
  | [] -> acc
  | p :: ps ->
    if v - p < 2 then acc
    else if p_factors (v - p) = 1 then
      loop3 (succ acc) v ps
    else
      loop3 acc v ps in
  List.fold_left (fun acc p -> loop3 acc (s - p) p3) 0 p7