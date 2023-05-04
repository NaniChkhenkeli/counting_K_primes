let put k v m = fun k' -> if k' = k then Some v else m k'
 
let remove k m = fun k' -> if k' = k then None else m k'

(* pure function aris carrys is*)   

(*tail recursion*) 

(*1*)
let fac' n = 
let rec impl n acc = 
  if n < 2 than acc else fac' (n-1) (acc*n) 
  in impl n 1 

(*2*)
let rem' a l =
let rec impl a l acc = function [] -> acc
|x::xs -> if x = a then rem' a xs else impl a xs (x::acc) 
in List.rev (impl a l [])  


(*3*)
    let partition f l =
      let rec impl acc1 acc2 = function
        | [] -> List.rev acc1, List.rev acc2
        | x::xs ->
          if f x then impl (x::acc1) acc2 xs
          else impl acc1 (x::acc2) xs
      in
      impl [] [] l

      (*lazy lists*)
type 'a llist = Cons of 'a * (unit -> 'a llist)

let rec lnat n = Cons (n, fun() -> lnat (n+1)) 

let rec ltake n (Cons (h,tl)) = if n = 0 then [] else h::ltake (n-1) (tl ())
  

let rec lfib () =
  let rec impl a b = Cons(a, fun() -> impl b (a+b))
  in fib 0 1



let rec lfilter p (Cons (h,tl))= if Cons(h,tl) -> if p h then Cons(h, fun() -> lfilter p (te())) else lfilter p (tl())
    