open Printf

module Bigint = struct              
  
      type sign     = Pos | Neg                           (*Used to determine the sign of the number*)
      type bigint   = Bigint of sign * int list          
                                             
let  radix    = 10
let  radixlen =  1

let car       = List.hd
let cdr       = List.tl

(*

let rec compare' list1 list2 currentOrder = match (list1, list2) with
  | [],[]                         -> currentOrder
  | list1, []                     -> 1
  | [], list2                     -> -1
  | car1::cdr1, car2::cdr2        ->
    let order =
    if car1 > car2 then 1
    else if car1 < car2 then -1
    else currentOrder
    in
    compare' cdr1 cdr2 order;;

    print_int(compare' [1] [] 0)

*)

(*
let rmLeadingZero list =
  let rec rmLeadingZero' list' = match list' with
      | []       -> []                      (*Returns an empty list*)
      | [0]      -> []                      (*Returns an empty list, when the list contaisn a 0*)
      | car::cdr ->                         (*When there are still elements in the list*)
           let cdr' = rmLeadingZero' cdr    (*Recusion through the next elements of the list*)
           in  match car, cdr' with         
              | 0, [] -> []                 (*Returns empty list when we hit the end of the list and the previous number is a 0*)
              | car, cdr' -> car::cdr'      (*Return the numbers*)
  in rmLeadingZero' list

  *)

  let rec add' list1 list2 carry = match (list1, list2, carry) with
  (*For when we hit the end of a list*)
| list1, [], 0       -> list1                                   (*returns the first list when the second list is empty and there is no carry*)
| [], list2, 0       -> list2                                   (*returns the second list when the first list is empty and there is no carry*)

  (*For when we hit the end of the list but there is still a carry value*)
| list1, [], carry   -> add' list1 [carry] 0                    (*calls add' on the first list and carry since the second list is empty*)
| [], list2, carry   -> add' [carry] list2 0                    (*calls add' on the second list and carry since the first list is empty*)

  (*For when the both lists contain a value*)
| car1::cdr1, car2::cdr2, carry ->                              (*Get the element from the list and any carry that was passed in from a previous call*)
let sum = car1 + car2 + carry                                 (*Assign sum to be the addition of the three values*)
in  sum mod radix :: add' cdr1 cdr2 (sum / radix)             (*Using that sum, add the remainder to the list, and recusivly call using the next element from both lists while*)
  (*passing the carry*)

(*Creates a new object containing the result of the addition between two int lists*)
(*Name: add, Accepts: 2 objects containg the sign and list of integers*)
let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
if neg1 = neg2                                              (*Checks to see if the two numbers have the same sign*)
then Bigint (neg1, add' value1 value2 0)                    (*Create the object containing the sign and the new int list that has been added together*)
else( (*There are two oppsing signs. +/-*) (*Compare the values, keep the sign of the greater value*)
if (compare' value1 value2 0)  = 1
then Bigint (neg1, rmLeadingZero(sub' value1 value2 0))
else Bigint (neg2, rmLeadingZero(sub' value2 value1 0))
)



let () = List.iter (printf "%d ") (rmLeadingZero a)