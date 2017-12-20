(*
Davie Truong
dtruong8@ucsc.edu
1524861
*)

(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf         (*Keeps the Printf module open so we don't need to call it every time*)

(*Bigint is a module used to create and manipulate a list of int*)
(*The list of int is ordered in lowest order digit to highest*)
module Bigint = struct              

    type sign     = Pos | Neg                           (*Used to determine the sign of the number*)
    type bigint   = Bigint of sign * int list           (*Tuple of type bigint that contains the sign of the number for the left element and 
                                                          a list that contains integers as the right element*)
    let  radix    = 10
    let  radixlen =  1

    (*Short-Cut module calls*)
    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    (*Turns a string to a list of characters*)
    (*Name: charlist_of_string , Accepts: string*)
    let charlist_of_string str =                            
        let last = strlen str - 1                           (*Gets the position of the last character in the string*)
        in  let rec charlist pos result =                   (*Recrusively go through the string while storing each char in a list called result*)
            if pos < 0                                      (*Stops when there is nothing left in the string*)
            then result                                     (*Return the list of char*)
            else charlist (pos - 1) (str.[pos] :: result)   (*Contine the recursive call with the nex position and the new updated list*)
        in  charlist last []

    (*Turns a string to a list of integers in reverse order*)
    (*Name: bigint_of_string , Accepts: string*)
    let bigint_of_string str =
        let len = strlen str                                                (*Assigns the length of the string to len in the following function "to_intlist"*)
        in  let to_intlist first =                                  
                let substr = strsub str first (len - first) in              (*Gets the substring of the string from the 0/1 position depending on sign to the end of the string*)
                let digit char = int_of_char char - int_of_char '0' in      (*A function called digit that accepts a char, turns the char into an int and subtracts it by 0*)
                map digit (reverse (charlist_of_string substr))             (*Calls the charlist_of_string on the substring to create a list of character from the string*)
                                                                            (*Reverse returns the list of characters in oposite order, Ex. [1,2,3] -> [3,2,1]*)
                                                                            (*This reversal is used to simplify arithmatic operations since they proceed from lowest order digit to highest*)
                                                                            (*map digit, goes through every char in the list and turns it into an int*)
            in  if   len = 0                                                (*Checks to see if the string contains values*)
                then zero                                                   (*Create a new object pair containg a positive sign and an empty list of type int*)
                else if   str.[0] = '_'                                     (*Checks to see if the number is negative or positive*)
                     then Bigint (Neg, to_intlist 1)                        (*Create a new object containg the sign and a list of integers*)
                     else Bigint (Pos, to_intlist 0)                        (*The 0/1 is used to determine the start index of the string*)

    (*Turns a list of int back into a string in the correct order*)
    (*Name: string_of_bigint , Accepts: Object that contains the sign of the number and a list of ints in reverse order*)
    let string_of_bigint (Bigint (sign, value)) =                      
        match value with                                                (*match works like a switch statement, in this case it will compare the list to determine the operation*)
        | []    -> "0"                                                  (*If the list is empty than the string is 0*)
        | value -> let reversed = reverse value                         (*Reverses the list of int into the correct highest to lowest order digit*)
                   in  strcat ""                                        (*Calls the string concatiantion module*)
                       ((if sign = Pos then "" else "-") ::             (*Starts by concating the sign*)
                        (map string_of_int reversed))                   (*then concats each element of the list after it gets turned into a string*)
    

    let rec cmp' list1 list2 currentOrder = match (list1, list2) with
        | [],[]                         -> currentOrder         (*Returns the current order when we hit the end of the lists*)
        | list1, []                     -> 1                    (*Returns 1 if list1 has a greater length*)
        | [], list2                     -> -1                   (*Retuns -1 if list2 has a greater length*)
        | car1::cdr1, car2::cdr2        ->                      (*Goes through the list*)
            let order =                                         (*Keeps track and determins which value is currently greater*)
            if car1 > car2 then 1
            else if car1 < car2 then -1
            else currentOrder
            in
            cmp' cdr1 cdr2 order                            (*Does the recursive call*)
 
    (*Compares the sign to determin which one is larger, if they have the same sign, we check the value*)
    (*Name: compare, Accepts: 2 objects containg the sign and list of integers*) 
    let cmp (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2                      (*Checks to see if the two signs are the same*)
        then cmp' value1 value2 0
        else if neg1 = Neg                  (*Check to see if the first list is negative*)
        then -1                             (*Returns -1 since list1 is less than list2*)
        else 1                              (*Returns 1 since list1 is greater than list2*)

    let trimzeros list =
        let rec trimzeros' list' = match list' with
            | []       -> []                      (*Returns an empty list*)
            | [0]      -> []                      (*Returns an empty list, when the list contaisn a 0*)
            | car::cdr ->                         (*When there are still elements in the list*)
                let cdr' = trimzeros' cdr    (*Recusion through the next elements of the list*)
                in  match car, cdr' with         
                    | 0, [] -> []                 (*Returns empty list when we hit the end of the list and the previous number is a 0*)
                    | car, cdr' -> car::cdr'      (*Return the numbers*)
        in trimzeros' list

    (*Recusivly add the numbers from the respective element of each list*)
    (*Name: add' , Accepts: 2 lists of ints and a value of type int,to hold the carry*)
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

         
    (*Recursively subtract numbers from the respective element of each list*)
    (*Name sub' , Accepts: 2 lists of ints and a value of time int to hold the borrow*)
    let rec sub' list1 list2 borrow  = match (list1, list2, borrow) with
                                                                        (*For when we hit the end of a list*)
        | list1, [], 0       -> list1                                   (*returns the first list when the second list is empty and there is no borrow*)
        | [], list2, 0       -> list2                                   (*returns the second list when the first list is empty and there is no borrow*)

                                                                        (*For when we hit the end of the list but there is still a borrow value*)
        | list1, [], borrow   -> trimzeros(sub' list1 [borrow] 0)                  (*calls sub' on the first list and borrow since the second list is empty*)
        | [], list2, borrow   -> trimzeros(sub' [borrow] list2 0)                    (*calls sub' on the second list and borrow since the first list is empty*)

                                                                        (*For when the both lists contain a value*)
        | car1::cdr1, car2::cdr2, borrow ->                              (*Get the element from the list and any borrow that was passed in from a previous call*)
            if (car1 - borrow) < car2                                   (*Determins if we need to borrow from the next number*)
            then let dif = (10 + car1) - (car2 + borrow)                (*Do the subtraction after adding in the borrow and continue the call*)
                 in dif mod radix :: trimzeros(sub' cdr1 cdr2 1)
            else let dif = car1 - car2 - borrow                         (*Subtract like normal and do the call*)
                 in dif mod radix :: trimzeros(sub' cdr1 cdr2 0)

    (*Used to double the number given*)                                                                    
    let doubleNumber number =
        add' number number 0

    (*Recusively multiplies numbers from the respective element of each list*)
    let rec mul' (multiplier, powerof2, multiplicand') = 
        if (cmp' powerof2 multiplier 0) = 1
        then multiplier, []
        else 
            let remainder, product = mul' (multiplier, doubleNumber powerof2, doubleNumber multiplicand')
            in if (cmp' powerof2 remainder 0) = 1
               then remainder, product
               else (trimzeros (sub' remainder powerof2 0)), (add' product multiplicand' 0)

    (*Creates a new object containing the result of the addition between two int lists*)
    (*Name: add, Accepts: 2 objects containg the sign and list of integers*)
    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2                                              (*Checks to see if the two numbers have the same sign*)
        then Bigint (neg1, add' value1 value2 0)                    (*Create the object containing the sign and the new int list that has been added together*)
        else( (*There are two oppsing signs. +/-*) (*Compare the values, keep the sign of the greater value*)
            if (cmp' value1 value2 0)  = 1
            then Bigint (neg1, trimzeros(sub' value1 value2 0))
            else Bigint (neg2, trimzeros(sub' value2 value1 0))
        )
      
    (*Creates a new object containing the result of the subtraction between two int lists*)
    (*Name: sub, Accepts: 2 objects containg the sign and list of integers*)
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then (
            if (cmp' value2 value1 0) = 1       
            then Bigint (Neg, trimzeros(sub' value2 value1 0))
            else Bigint (Pos, trimzeros(sub' value1 value2 0))
        )
        else (                                  (*When they are different signs we add*)
            if (cmp' value1 value2 0) = 1
            then Bigint (neg2, add' value1 value2 0)
            else Bigint (neg1, add' value2 value1 0)
        )

    (*Creates a new object containing the result of the multiplication between two int lists*)
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        let _, product = mul' (value1, [1], value2)
        in if neg1 = neg2
           then Bigint (Pos, product)
           else Bigint (Neg, product)

    let rec divrem' (dividend, powerof2, divisor') = 
        if (cmp' divisor' dividend 0) = 1
        then [0], dividend
        else let quotient, remainder = divrem' (dividend, doubleNumber powerof2, doubleNumber divisor')
             in if (cmp' divisor' remainder 0) = 1
                then quotient, remainder
                else (add' quotient powerof2 0), (trimzeros(sub' remainder divisor' 0))

    let divrem ( (Bigint (neg1, value1)) , (Bigint (neg2, value2)) ) = 
        let quotient, remainder = divrem' (value1, [1], value2)
        in if neg1 = neg2
            then Bigint (Pos, quotient), Bigint (Pos, remainder)
            else Bigint (Neg, quotient), Bigint (Pos, remainder)

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        let quotient,_ = divrem ( (Bigint (neg1, value1)) , (Bigint (neg2, value2)) )
        in quotient

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        let _,remainder = divrem ((Bigint (neg1, value1)), (Bigint (neg2, value2)))
        in remainder

        
    let even (Bigint (neg1, value1)) = 
        let _, remainder = rem (Bigint (neg1, value1)), (Bigint(Pos, [2]))
        in (cmp remainder zero) = 0

    let rec pow' ( (Bigint (neg1, base)) , (Bigint (neg2, expt)) , (Bigint (neg3, result)) ) = match (Bigint (neg2, expt)) with
    | (Bigint (neg2, expt)) when (cmp (Bigint (neg2, expt)) zero) = 0         ->  (Bigint (neg3, result))
    | (Bigint (neg2, expt)) when (even (Bigint (neg2, expt)))                 ->  
            pow' ( mul (Bigint (neg1, base)) (Bigint (neg1, base)), div (Bigint (neg2, expt)) (Bigint(Pos, [2])), (Bigint (neg3, result)) )
    | (Bigint (neg2, expt))                                                   ->
            pow' ( Bigint(neg1, base), (sub (Bigint (neg2, expt)) (Bigint(Pos, [1]))), (mul (Bigint(neg1, base)) (Bigint(neg3, result))) )
        
    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) = (*Where value1 is base and value2 is exponent*)
        if (cmp (Bigint (neg2, value2)) zero) = -1
        then pow' ( (div(Bigint (Pos, [1])) (Bigint (neg1, value1))) , (Bigint (Neg, value2)) , (Bigint (Pos, [1])) )
        else pow' ( (Bigint(neg1, value1)) , (Bigint(neg2, value2)) , (Bigint(Pos, [1])) )


end

