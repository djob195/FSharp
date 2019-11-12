module Hamming

let distance (strand1: string) (strand2: string): int option = 
    if strand1.Length <> strand2.Length then
        None
    else
        let mutable result = 0
        let length = strand1.Length - 1 ;
        for i = 0 to length do
            if strand1.Chars i <> strand2.Chars i then
                result <- result + 1        
        Some result