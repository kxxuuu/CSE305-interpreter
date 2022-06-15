exception Foo of string



type stackValue = BOOL of bool | INT of int | ERROR | STRING of string | NAME of string | UNIT | 
                    CLOSURE of (stackValue * (command list) * ((stackValue * stackValue) list list))

and command = ADD | SUB | MUL | DIV | PUSH of stackValue | POP | REM | NEG | SWAP | 
                CAT | AND | OR | NOT | EQUAL | LESS_THAN | BIND | IF | LET | END |
                FUN of (stackValue * stackValue) | IN_OUT_FUN | RETURN | FUN_END | CALL | 
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
            | _ -> (
              match (String.contains restStr '.') with
              | true -> ERROR
              | false -> INT(int_of_string restStr)
            )
              
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
        | "cat" -> CAT
        | "and" -> AND
        | "or" -> OR
        | "not" -> NOT
        | "equal" -> EQUAL
        | "lessThan" -> LESS_THAN
        | "bind" -> BIND
        | "if" -> IF
        | "let" -> LET
        | "end" -> END
        | "inOutFun" -> IN_OUT_FUN
        | "return" -> RETURN
        | "funEnd" -> FUN_END
        | "call" -> CALL
        | _ -> 
            (match String.sub s 0 4, String.sub s 5 (String.length s - 5) with
            | "push", rest -> PUSH(str2stkval(rest))
            | _ -> 
                (match String.split_on_char ' ' s with
                | "fun"::name::arg::[] -> FUN(NAME(name), NAME(arg))
                | _ -> raise (Foo "push, error. ")
                )
            )
    in

    (* let com2str cv = 
        match cv with 
        | ADD -> "add"
        | SUB -> "sub"
        | MUL -> "mul"
        | DIV -> "div"
        | POP-> "pop"
        | REM -> "rem"
        | NEG -> "neg"
        | SWAP -> "swap"
        | TO_STRING -> "to str"
        | PRINTLN -> "println"
        | QUIT -> "quit"
        | CAT -> "cat"
        | AND -> "and"
        | OR -> "or"
        | NOT -> "not"
        | EQUAL -> "equal"
        | LESS_THAN -> "less than"
        | BIND -> "bind"
        | IF -> "if"
        | LET -> "let"
        | END -> "end"
        | IN_OUT_FUN -> "in out fun"
        | RETURN -> "return"
        | FUN_END -> "fun end"
        | CALL -> "call"
        | PUSH(x) -> "push"
        | FUN(x,y) -> "fun"
    in *)
    
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
        | CLOSURE(x) -> ":error:"
    in

    let rec searchHeaps stackValue heaps = 
      match  heaps with
      | (curHeap::restHeaps) -> 
        (match List.assoc_opt stackValue curHeap with
        | Some x -> Some x
        | None -> searchHeaps stackValue restHeaps
        )
      | [] -> None
      in


    let rec scanFun commands a = 
        match commands with
        | FUN(x)::restCommands -> scanFun restCommands a
        | FUN_END::restCommands -> (List.rev a, restCommands)
        | command::restCommands ->  scanFun restCommands (command::a)
        | _ -> raise (Foo "empty commands list. ")
    in

    (* let rec funBody2List funBody list = 
        match funBody with
        | [] -> list
        | sv::funBody -> sv::list
    in *)
      
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

    (* "abc" l = 5 (1,l-1)->(1,3)*)

    let rec processor (comList: command list list) (stack: stackValue list list) (heap: (stackValue * stackValue) list list) = 
        (* let ((c::cs)::css) = comList in print_string ((com2str c) ^ "\n"); *)
        match (comList, stack, heap) with 
        | ((PUSH(STRING(a))::currCommand)::restOfCommands, curStack::restStacks, heap) -> 
          processor restOfCommands (((STRING(String.sub a 1 (String.length a - 2)))::curStack)::restStacks) heap
        | ((PUSH(INT(a))::currCommand)::restOfCommands, curStack::restStacks, heap) -> processor (currCommand::restOfCommands) ((INT(a)::curStack)::restStacks) heap
        | ((PUSH(NAME(a))::currCommand)::restOfCommands, curStack::restStacks, heap) -> processor (currCommand::restOfCommands) ((NAME(a)::curStack)::restStacks) heap
        | ((PUSH(BOOL(a))::currCommand)::restOfCommands, curStack::restStacks, heap) -> processor (currCommand::restOfCommands) ((BOOL(a)::curStack)::restStacks) heap
        | ((PUSH(ERROR)::currCommand)::restOfCommands, curStack::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap
        | ((PUSH(UNIT)::currCommand)::restOfCommands, curStack::restStacks, heap) -> processor (currCommand::restOfCommands) ((UNIT::curStack)::restStacks) heap
        | ((POP::currCommand)::restOfCommands, (a::curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((curStack)::restStacks) heap
        | ((POP::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap 

        | ((ADD::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match ((searchHeaps (NAME(a)) heaps), searchHeaps (NAME(b)) heaps) with 
            | Some INT(x), Some INT(y) -> processor (currCommand::restOfCommands) ((INT(x + y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps)
            )
        | ((ADD::currCommand)::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some INT(x), b -> processor (currCommand::restOfCommands) ((INT(x + b)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps)) 
        | ((ADD::currCommand)::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some INT(y) -> processor (currCommand::restOfCommands) ((INT(a + y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((ADD::currCommand)::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((INT(a+b)::restCurStack)::restStacks) heap
        | ((ADD::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((SUB::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some INT(x), Some INT(y) -> processor (currCommand::restOfCommands) ((INT(y-x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((SUB::currCommand)::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some INT(x), b -> processor (currCommand::restOfCommands) ((INT(b-x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps))
        | ((SUB::currCommand)::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some INT(y) -> processor (currCommand::restOfCommands) ((INT(y-a)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((SUB::currCommand)::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> 
          processor (currCommand::restOfCommands) ((INT(b-a)::restCurStack)::restStacks) heap
        | ((SUB::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((MUL::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some INT(x), Some INT(y) -> processor (currCommand::restOfCommands) ((INT(x * y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((MUL::currCommand)::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some INT(x), b -> processor (currCommand::restOfCommands) ((INT(x * b)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps))
        | ((MUL::currCommand)::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some INT(y) -> processor (currCommand::restOfCommands) ((INT(a * y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((MUL::currCommand)::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((INT(a * b)::restCurStack)::restStacks) heap
        | ((MUL::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((DIV::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some INT(0), Some INT(y) -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) heaps
            | Some INT(x), Some INT(y) -> processor (currCommand::restOfCommands) ((INT(y / x)::restCurStack)::restStacks) heaps
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((DIV::currCommand)::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some INT(0), b -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps)
            | Some INT(x), b -> processor (currCommand::restOfCommands) ((INT(b / x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps))
        | ((DIV::currCommand)::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | 0, Some INT(y) -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps)
            | a, Some INT(y) -> processor (currCommand::restOfCommands) ((INT(y / a)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((DIV::currCommand)::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> 
            (match (a, b) with 
            | 0, b -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::INT(b)::restCurStack)::restStacks) heap
            | a, b -> processor (currCommand::restOfCommands) ((INT(b / a)::restCurStack)::restStacks) heap
            (* | _ -> processor (currCommand::restOfCommands) ((ERROR::restCurStack)::restStacks) heap *)
            )
        | ((DIV::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((REM::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some INT(0), Some INT(y) -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps)
            | Some INT(x), Some INT(y) -> processor (currCommand::restOfCommands) ((INT(y mod x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((REM::currCommand)::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some INT(0), b -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps)
            | Some INT(x), b -> processor (currCommand::restOfCommands) ((INT(b mod x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps))
        | ((REM::currCommand)::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | 0, Some INT(y) -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps)
            | a, Some INT(y) -> processor (currCommand::restOfCommands) ((INT(y mod a)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps)) 
            (* //fixed (ERROR::INT(a)::NAME(b)::restCurStack)*)
        | ((REM::currCommand)::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> 
            (match (a, b) with 
            | 0, b -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::INT(b)::restCurStack)::restStacks) heap
            | a, b -> processor (currCommand::restOfCommands) ((INT(b mod a)::restCurStack)::restStacks) heap
            (* | _ -> processor (currCommand::restOfCommands) ((ERROR::restCurStack)::restStacks) heap *)
            )
        | ((REM::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((NEG::currCommand)::restOfCommands, (NAME(a)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps) with 
            | Some INT(0) -> processor (currCommand::restOfCommands) ((INT(0)::restCurStack)::restStacks) (heaps)
            | Some INT(x) -> processor (currCommand::restOfCommands) ((INT(0-x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::restCurStack)::restStacks) (heaps)) 
            (* // fixed (ERROR::NAME(a)::restCurStack)*)
        | ((NEG::currCommand)::restOfCommands, (INT(a)::restCurStack)::restStacks, heap) -> 
            (match a with 
            | 0 -> processor (currCommand::restOfCommands) ((INT(0)::restCurStack)::restStacks) heap
            | a -> processor (currCommand::restOfCommands) ((INT(0-a)::restCurStack)::restStacks) heap
            )          
        | ((NEG::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((SWAP::currCommand)::restOfCommands, (a::b::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((b::a::restCurStack)::restStacks) heap
        | ((SWAP::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((TO_STRING::currCommand)::restOfCommands, (a::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((STRING(strOfStackValue(a))::restCurStack)::restStacks) heap
        | ((TO_STRING::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((PRINTLN::currCommand)::restOfCommands, (a::restCurStack)::restStacks, heap) -> 
            (match a with
            | STRING(a) -> file_write a; processor (currCommand::restOfCommands) ((restCurStack)::restStacks) heap
            | _ -> processor (currCommand::restOfCommands) ((ERROR::a::restCurStack)::restStacks) heap)
        | ((PRINTLN::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((QUIT::currCommand)::restOfCommands, stack, heap) -> (stack, heap)

        | ((CAT::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some STRING(x), Some STRING(y) -> processor (currCommand::restOfCommands) ((STRING(y ^ x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps)) 
        | ((CAT::currCommand)::restOfCommands, (NAME(a)::STRING(b)::restCurStack)::restStacks, heaps) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some STRING(x), b -> processor (currCommand::restOfCommands) ((STRING(b ^ x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::STRING(b)::restCurStack)::restStacks) (heaps))
        | ((CAT::currCommand)::restOfCommands, (STRING(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some STRING(y) -> processor (currCommand::restOfCommands) ((STRING(y ^ a)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::STRING(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((CAT::currCommand)::restOfCommands, (STRING(a)::STRING(b)::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((STRING(b ^ a)::restCurStack)::restStacks) heap
        | ((CAT::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((AND::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some BOOL(x), Some BOOL(y) -> processor (currCommand::restOfCommands) ((BOOL(x && y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((AND::currCommand)::restOfCommands, (NAME(a)::BOOL(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some BOOL(x), b -> processor (currCommand::restOfCommands) ((BOOL(x && b)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::BOOL(b)::restCurStack)::restStacks) (heaps))
        | ((AND::currCommand)::restOfCommands, (BOOL(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some BOOL(y) -> processor (currCommand::restOfCommands) ((BOOL(a && y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::BOOL(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((AND::currCommand)::restOfCommands, (BOOL(a)::BOOL(b)::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((BOOL(a && b)::restCurStack)::restStacks) heap
        | ((AND::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((OR::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some BOOL(x), Some BOOL(y) -> processor (currCommand::restOfCommands) ((BOOL(x || y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((OR::currCommand)::restOfCommands, (NAME(a)::BOOL(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some BOOL(x), b -> processor (currCommand::restOfCommands) ((BOOL(x || b)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::BOOL(b)::restCurStack)::restStacks) (heaps))
        | ((OR::currCommand)::restOfCommands, (BOOL(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some BOOL(y) -> processor (currCommand::restOfCommands) ((BOOL(a || y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::BOOL(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((OR::currCommand)::restOfCommands, (BOOL(a)::BOOL(b)::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((BOOL(a || b)::restCurStack)::restStacks) heap
        | ((OR::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((NOT::currCommand)::restOfCommands, (NAME(a)::restCurStack)::restStacks, (heaps)) -> 
            (match searchHeaps (NAME(a)) heaps with 
            | Some BOOL(x) -> processor (currCommand::restOfCommands) ((BOOL(not x)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::restCurStack)::restStacks) (heaps))
        | ((NOT::currCommand)::restOfCommands, (BOOL(a)::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((BOOL(not a)::restCurStack)::restStacks) heap
        | ((NOT::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((EQUAL::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some INT(x), Some INT(y) -> processor (currCommand::restOfCommands) ((BOOL(x == y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((EQUAL::currCommand)::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some INT(x), b -> processor (currCommand::restOfCommands) ((BOOL(x == b)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps))
        | ((EQUAL::currCommand)::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some INT(y) -> processor (currCommand::restOfCommands) ((BOOL(a == y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((EQUAL::currCommand)::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((BOOL(a == b)::restCurStack)::restStacks) heap
        | ((EQUAL::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((LESS_THAN::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, searchHeaps (NAME(b)) heaps) with 
            | Some INT(x), Some INT(y) -> processor (currCommand::restOfCommands) ((BOOL(x > y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((LESS_THAN::currCommand)::restOfCommands, (NAME(a)::INT(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (searchHeaps (NAME(a)) heaps, b) with 
            | Some INT(x), b -> processor (currCommand::restOfCommands) ((BOOL(x > b)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::INT(b)::restCurStack)::restStacks) (heaps))
        | ((LESS_THAN::currCommand)::restOfCommands, (INT(a)::NAME(b)::restCurStack)::restStacks, (heaps)) -> 
            (match (a, searchHeaps (NAME(b)) heaps) with 
            | a, Some INT(y) -> processor (currCommand::restOfCommands) ((BOOL(a > y)::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::INT(a)::NAME(b)::restCurStack)::restStacks) (heaps))
        | ((LESS_THAN::currCommand)::restOfCommands, (INT(a)::INT(b)::restCurStack)::restStacks, heap) -> 
            processor (currCommand::restOfCommands) ((BOOL(a > b)::restCurStack)::restStacks) heap
        | ((LESS_THAN::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((BIND::currCommand)::restOfCommands, (NAME(a)::NAME(b)::restCurStack)::restStacks, heaps(*curHeap::restHeaps*)) -> 
          (match (searchHeaps (NAME(a)) heaps), heaps with
          | (Some x), (curHeap::restHeaps) -> processor (currCommand::restOfCommands) ((UNIT::restCurStack)::restStacks) (((NAME(b), x)::curHeap)::restHeaps)
          | _ -> processor (currCommand::restOfCommands) ((ERROR::NAME(a)::NAME(b)::restCurStack)::restStacks) heaps)
          (* fixed search heaps *)
        | ((BIND::currCommand)::restOfCommands, (ERROR::NAME(b)::restCurStack)::restStacks, (curHeap::restHeaps)) -> 
            processor (currCommand::restOfCommands) ((ERROR::ERROR::NAME(b)::restCurStack)::restStacks) ((curHeap)::restHeaps)  
        | ((BIND::currCommand)::restOfCommands, (a::NAME(b)::restCurStack)::restStacks, (curHeap::restHeaps)) -> 
          processor (currCommand::restOfCommands) ((UNIT::restCurStack)::restStacks) (((NAME(b),a)::curHeap)::restHeaps)
        | ((BIND::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap

        | ((IF::currCommand)::restOfCommands, (a::b::NAME(c)::restCurStack)::restStacks, (heaps)) -> 
            (match searchHeaps (NAME(c)) heaps with
            | Some BOOL(true) -> processor (currCommand::restOfCommands) ((a::restCurStack)::restStacks) (heaps)
            | Some BOOL(false) -> processor (currCommand::restOfCommands) ((b::restCurStack)::restStacks) (heaps)
            | _ -> processor (currCommand::restOfCommands) ((ERROR::a::b::NAME(c)::restCurStack)::restStacks) (heaps))
        | ((IF::currCommand)::restOfCommands, (a::b::BOOL(c)::restCurStack)::restStacks, heaps) -> 
            (match c with
            | true -> processor (currCommand::restOfCommands) ((a::restCurStack)::restStacks) heap
            | false -> processor (currCommand::restOfCommands) ((b::restCurStack)::restStacks) heap)
        | ((IF::currCommand)::restOfCommands, (curStack)::restStacks, heap) -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) heap
    
        | ((LET::currCommand)::restOfCommands, stack, heap) -> processor (currCommand::restOfCommands) ([]::stack) ([]::heap)
        | ((END::currCommand)::restOfCommands, (curStack::nextStack::restStacks), (curHeap)::restHeaps) -> 
          (* processor (currCommand::restOfCommands) ((stackValue::nextStack)::restStacks) restHeaps *)
            (match (curStack, nextStack, restStacks) with
            (* | ([], nextStack, restStacks) -> processor (currCommand::restOfCommands) (nextStack::restStacks) restHeaps *)
            | ((stackValue::restCurStack), nextStack, restStacks) -> processor (currCommand::restOfCommands) ((stackValue::nextStack)::restStacks) restHeaps
            | _ -> processor (currCommand::restOfCommands) (nextStack::restStacks) restHeaps
            )
            (* CLOSURE of (stackValue->b * (command list) * ((stackValue * stackValue) list list))
             FUN | CALL | RETURN | FUN_END | IN_OUT_FUN | *)
        | (((FUN(funName, funArg)::currCommand)::restOfCommands), (restCurStack)::restStacks, curHeap::restHeaps) -> 
            (match (scanFun currCommand []) with 
            | (funBody, restCommands) -> 
                processor (restCommands::restOfCommands) ((UNIT::restCurStack)::stack) (((funName, CLOSURE(funArg, funBody, heap))::curHeap)::restHeaps)
            )
            
        | (((CALL::currCommand)::restOfCommands), ((NAME(a)::name::curStack)::restStacks), (heaps)) -> 
           (match (searchHeaps (NAME(a)) heap), (searchHeaps (name) heap) with
            | Some arg, Some CLOSURE(funArg, funBody, _closureHeap::_heap) -> 
                let (s, h) = processor (funBody::currCommand::restOfCommands) ([]::curStack::restStacks) (((funArg, arg)::_closureHeap)::_heap) in
                (match s, h with
                | ((hd::tl)::rest), returnH::restH -> 
                    processor (currCommand::restOfCommands) ((hd::curStack)::restStacks) (returnH::heaps)
                | ([]::rest) , returnH::restH -> processor (currCommand::restOfCommands) ((curStack)::restStacks) (returnH::heaps)
                | _ -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) (heaps)
                )  
            | _ -> processor ((currCommand)::restOfCommands) ((ERROR::NAME(a)::name::curStack)::restStacks) (heaps)
            )
            
            
        | (((CALL::currCommand)::restOfCommands), ((arg::name::curStack)::restStacks), (heaps)) -> 
           (match (searchHeaps (name) heap) with
            | Some CLOSURE(funArg, funBody, _closureHeap::_heap) -> 
                let (s, h) = processor (funBody::currCommand::restOfCommands) ([]::curStack::restStacks) (((funArg, arg)::_closureHeap)::_heap) in
                (match s, h with
                | ((hd::tl)::rest), returnH::restH -> 
                    processor (currCommand::restOfCommands) ((hd::curStack)::restStacks) (returnH::heaps)
                | ([]::rest) , returnH::restH -> processor (currCommand::restOfCommands) ((curStack)::restStacks) (returnH::heaps)
                | _ -> processor (currCommand::restOfCommands) ((ERROR::curStack)::restStacks) (heaps)
                )   
            | _ -> processor ((currCommand)::restOfCommands) ((ERROR::arg::name::curStack)::restStacks) (heaps)
            )

        | ((RETURN::currCommand)::restOfCommands, (s), heaps) -> (s, heaps)

        | (FUN_END::currCommand)::restOfCommands, (curStack::restStacks), heap -> 
            processor ((currCommand)::restOfCommands) ((ERROR::curStack)::restStacks) heap
            
        | (((IN_OUT_FUN::currCommand)::restOfCommands), (curStack::restStacks), curHeap::restHeaps) -> (stack, heap)

        (* | ((a::al)::comList, stack, heap) -> print_string(com2str(a))         *)

        | (comList, stack, heap) -> 
            (* print_string("## ");print_int(List.length comList); print_int(List.length stack); print_int(List.length heap);print_string(";  "); *)
            raise(Foo "no command")
    in


    let _ = processor [comList] [[]] [[]] in ()

;;
(* interpreter("ProjectPart3TestInputs/input/input10.txt", "ProjectPart3TestOutputs10.txt") *)
(* interpreter("ProjectPart3TestInputs/input/input9.txt", "ProjectPart3TestOutputs9.txt") *)
