exception Foo of string

(*name -> var*)
type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT
type command = ADD | SUB | MUL | DIV | PUSH of stackValue | POP | REM | NEG | SWAP | 
                CAT | AND | OR | NOT | EQUAL | LESS_THAN | BIND | IF | LET | END |
                TO_STRING | PRINTLN | QUIT
                (* except push, pop, swap, println,  toString, Let, and End all other commands deal with bind. *)

let interpreter ((input: string), (output: string)): unit = 
    let ic = open_in input in

    let oc = open_out output in 
    let file_write str_val = Printf.fprintf oc "%s\n" str_val in

    let rec loop_read acc =
        try 
            let l = String.trim(input_line ic)  in loop_read (l::acc)
        with
        | End_of_file -> List.rev acc in
    
    let strList = loop_read [] in



    (* take a rest string to stackValue *)   
    let str2stkval restStr = 
        match restStr with
        | ":error:" -> ERROR
        | ":unit:" -> UNIT
        | ":true:" -> BOOL(true)
        | ":false:" -> BOOL(false)
        | _ -> 
            match restStr.[0] with
            | '"' -> STRING(restStr)
            | '_' -> NAME(restStr)
            | 'a'..'z' -> NAME(restStr)
            | 'A'..'Z' -> NAME(restStr)
            | _ -> INT(int_of_string restStr)
        in

    let str2com s = 
        match s with 
        | "add" -> ADD
        | "sub" -> SUB
        | "mul" -> MUL
        | "div" -> DIV
        | "pop" -> POP
        | "rem" -> REM
        | "neg" -> NEG
        | "swap" -> SWAP
        | "toString" -> TO_STRING
        | "println" -> PRINTLN
        | "quit" -> QUIT
        | _ -> 
            match String.sub s 0 4, String.sub s 5 (String.length s - 5) with
            | "push", rest -> PUSH(str2stkval(rest))
            | _ -> raise (Foo "push, error. ")

    in
    
    (* do str2com for each element of strList *)
    let comList = List.map str2com strList in

    let strOfStackValue stackValue = 
        match stackValue with
        | INT(x) -> string_of_int x
        | BOOL(x) -> 
            (match x with 
            | false -> ":false:"
            | true -> ":true:")
        | ERROR -> ":error:"
        | UNIT -> ":unit:"
        | STRING(x) -> x
        | NAME(x) -> x
    in

    (* let unwrapSome (some_x) = 
        match some_x with
        | Some stackValue -> stackValue
    in *)

    (* let unwrapStackValue stackValue = 
        match stackValue with
        | INT(x) -> x
        | BOOL(x) -> x
        | STRING(x) -> x
        | NAME(x) -> x
        | ERROR -> raise (Foo "unwrapStackValue, ERROR, error. ")
        | UNIT -> raise (Foo "unwrapStackValue, UNIT, error. ")
    in *)

    let rec processor (comList: command list) (stack: stackValue list list) (heap: (stackValue * stackValue) list list) = 
        match (comList, stack, heap) with 
        | (PUSH(STRING(a))::restOfCommands, curStack::restStacks, heap) -> processor restOfCommands ((STRING(String.sub a 1 (String.length a - 2))::curStack)::restStacks) heap
        | (PUSH(INT(a))::restOfCommands, curStack::restStacks, heap) -> processor restOfCommands ((INT(a)::curStack)::restStacks) heap
            (* (match a with
            | 0 -> processor restOfCommands (INT(0)::stack) heap
            | a -> processor restOfCommands (INT(a)::stack) heap) *)
        | (PUSH(NAME(a))::restOfCommands, curStack::restStacks, heap) -> processor restOfCommands ((NAME(a)::curStack)::restStacks) heap
        | (PUSH(BOOL(a))::restOfCommands, curStack::restStacks, heap) -> processor restOfCommands ((BOOL(a)::curStack)::restStacks) heap
        | (PUSH(ERROR)::restOfCommands, curStack::restStacks, heap) -> processor restOfCommands ((ERROR::curStack)::restStacks) heap
        | (PUSH(UNIT)::restOfCommands, curStack::restStacks, heap) -> processor restOfCommands ((UNIT::curStack)::restStacks) heap
        | (POP::restOfCommands, (a::curStack)::restStacks, heap) -> processor restOfCommands (curStack)::restStacks heap
        | (POP::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap 

        | (ADD::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some INT(x), Some INT(y) -> processor restOfCommands (INT(x + y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps
            )
        | (ADD::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some INT(x), b -> processor restOfCommands (INT(x + b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (ADD::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | a, Some INT(y) -> processor restOfCommands (INT(a + y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (ADD::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (INT(a+b)::restCurStack)::restStacks heap
        | (ADD::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (SUB::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some INT(x), Some INT(y) -> processor restOfCommands (INT(y - x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (SUB::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some INT(x), b -> processor restOfCommands (INT(x-b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (SUB::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | a, Some INT(y) -> processor restOfCommands (INT(a-y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (SUB::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (INT(b-a)::restCurStack)::restStacks heap
        | (SUB::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (MUL::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some INT(x), Some INT(y) -> processor restOfCommands (INT(x * y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (MUL::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some INT(x), b -> processor restOfCommands (INT(x + b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (MUL::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | a, Some INT(y) -> processor restOfCommands (INT(a + y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (MUL::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (INT(a * b)::restCurStack)::restStacks heap
        | (MUL::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (DIV::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some INT(0), Some INT(y) -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps
            | Some INT(x), Some INT(y) -> processor restOfCommands (INT(y / x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (DIV::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some INT(0), b -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps
            | Some INT(x), b -> processor restOfCommands (INT(b / x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (DIV::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | 0, Some INT(y) -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps
            | a, Some INT(y) -> processor restOfCommands (INT(y / a)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (DIV::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> 
            (match (a, b) with 
            | 0, b -> processor restOfCommands ERROR::restOfStack heap
            | a, b -> processor restOfCommands INT(b / a)::restOfStack heap
            | _ -> processor restOfCommands ERROR::restOfStack heap)
        | (DIV::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (REM::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some INT(0), Some INT(y) -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps
            | Some INT(x), Some INT(y) -> processor restOfCommands (INT(y mod x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (REM::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some INT(0), b -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps
            | Some INT(x), b -> processor restOfCommands (INT(b mod x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (REM::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | 0, Some INT(y) -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps
            | a, Some INT(y) -> processor restOfCommands (INT(y mod a)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (REM::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> 
            (match (a, b) with 
            | 0, b -> processor restOfCommands (ERROR::restCurStack)::restStacks heap
            | a, b -> processor restOfCommands (INT(b mod a)::restCurStack)::restStacks heap
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks heap)
        | (REM::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (NEG::restOfCommands, (NAME(a)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap) with 
            | Some INT(0) -> processor restOfCommands (INT(0)::restCurStack)::restStacks curHeap::restHeaps
            | Some INT(x) -> processor restOfCommands (INT(0-x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (NEG::restOfCommands, (INT(a)::restCurStack)::restStacks, heap) -> 
            (match a with 
            | 0 -> processor restOfCommands INT(0)::restOfStack heap
            | a -> processor restOfCommands INT(0-a)::restOfStack heap
            | _ -> processor restOfCommands ERROR::restOfStack heap)          
        | (NEG::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (SWAP::restOfCommands, (a::b::restCurStack)::restStacks, heap) -> processor restOfCommands (b::a::restCurStack)::restStacks heap
        | (SWAP::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (TO_STRING::restOfCommands, (a::restCurStack)::restStacks, heap) -> processor restOfCommands (STRING(strOfStackValue(a))::restCurStack)::restStacks heap
        | (TO_STRING::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (PRINTLN::restOfCommands, (a::restCurStack)::restStacks, heap) -> 
            (match a with
            | STRING(a) -> file_write a; processor restOfCommands (restCurStack)::restStacks heap
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks heap)
        | (PRINTLN::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (QUIT::restOfCommands, stack, heap) -> ()

        | (CAT::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some STRING(x), Some STRING(y) -> processor restOfCommands (STRING(y ^ x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (CAT::restOfCommands, (NAME(a)::STRING(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some STRING(x), b -> processor restOfCommands (STRING(b ^ x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (CAT::restOfCommands, (STRING(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | a, Some STRING(y) -> processor restOfCommands (STRING(y ^ a)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (CAT::restOfCommands, (STRING(a)::STRING(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (STRING(b ^ a)::restCurStack)::restStacks heap
        | (CAT::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (AND::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some BOOL(x), Some BOOL(y) -> processor restOfCommands (BOOL(x && y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (AND::restOfCommands, (NAME(a)::BOOL(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some BOOL(x), b -> processor restOfCommands (STRING(x && b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (AND::restOfCommands, (BOOL(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | a, Some BOOL(y) -> processor restOfCommands (BOOL(a && y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (AND::restOfCommands, (BOOL(a)::BOOL(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (BOOL(a && b)::restCurStack)::restStacks heap
        | (AND::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (OR::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some BOOL(x), Some BOOL(y) -> processor restOfCommands (BOOL(x || y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (OR::restOfCommands, (NAME(a)::BOOL(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some BOOL(x), b -> processor restOfCommands (BOOL(x || b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (OR::restOfCommands, (BOOL(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | Some BOOL(x), b -> processor restOfCommands (BOOL(x || b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (OR::restOfCommands, (BOOL(a)::BOOL(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (BOOL(a || b)::restCurStack)::restStacks heap
        | (OR::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        
        | (NOT::restOfCommands, (NAME(a)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match assoc_opt a curHeap with 
            | Some BOOL(x) -> processor restOfCommands (BOOL(not x)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (NOT::restOfCommands, (BOOL(a)::restCurStack)::restStacks, heap) -> processor restOfCommands (BOOL(not a)::restCurStack)::restStacks heap
        | (NOT::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (EQUAL::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some INT(x), Some INT(y) -> processor restOfCommands (BOOL(x == y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (EQUAL::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some INT(x), b -> processor restOfCommands (BOOL(x == b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (EQUAL::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | a, Some INT(y) -> processor restOfCommands (BOOL(x == y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (EQUAL::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (BOOL(a == b)::restCurStack)::restStacks heap
        | (EQUAL::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (LESS_THAN::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, assoc_opt b curHeap) with 
            | Some INT(x), Some INT(y) -> processor restOfCommands (BOOL(x < y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (LESS_THAN::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (assoc_opt a curHeap, b) with 
            | Some INT(x), b -> processor restOfCommands (BOOL(x < b)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (LESS_THAN::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (a, assoc_opt b curHeap) with 
            | a, Some INT(y) -> processor restOfCommands (BOOL(a < y)::restCurStack)::restStacks curHeap::restHeaps
            | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (LESS_THAN::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor restOfCommands (BOOL(a < b)::restCurStack)::restStacks heap
        | (LESS_THAN::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (BIND::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> 
          (match assoc_opt a curHeap with
          | Some x -> processor restOfCommands (UNIT::restCurStack)::restStacks ((NAME(b), x)::curHeap)::restHeaps
          | _ -> processor restOfCommands (ERROR::restCurStack)::restStacks curHeap::restHeaps)
        | (BIND::restOfCommands, (a::NAME(b)::restCurStack)::restStacks, curHeap::restHeaps) -> processor restOfCommands (UNIT::restCurStack)::restStacks ((NAME(b),a)::curHeap)::restHeaps
        | (BIND::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap

        | (IF::restOfCommands, (a::b::NAME(c)::restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match assoc_opt c curHeap with
            | Some BOOL(true) -> processor restOfCommands (a::restCurStack)::restStacks curHeap::restHeaps
            | Some BOOL(false) -> processor restOfCommands (b::restCurStack)::restStacks curHeap::restHeaps)
        | (IF::restOfCommands, (a::b::BOOL(c)::restCurStack)::restStacks, heap) -> 
            (match c with
            | true -> processor restOfCommands (a::restCurStack)::restStacks heap
            | false -> processor restOfCommands (b::restCurStack)::restStacks heap)
        | (IF::restOfCommands, stack, heap) -> processor restOfCommands (ERROR::restCurStack)::restStacks heap
    
        | (LET::restOfCommands, stack, heap) -> processor restOfCommands ([]::stack) ([]::heap)
        | (END::restOfCommands, (v::restCurStack)::restStacks, (curHeap)::restHeaps) ->  
            (match v with
            | stackValue -> processor restOfCommands (v::restCurStack)::restStacks (curHeap)::heap
            | _ -> processor restOfCommands restStacks restHeaps)

        | (comList, stack, heap) -> raise(Foo "no command")
    in


    processor comList []

;;
interpreter("Project-part1-Test-Inputs/input1.txt", "Project-part1-Test-Inputs1.txt")