module Pangram

let inline isInRange x = (x > 64 && x < 91 ) 
let inline charToInt c = int c

let isPangram (input: string): bool = 
    let mutable setAlphabet = Set.empty
    let length = input.Length - 1
    for i = 0 to length do
        let charInput = input.Chars i |> System.Char.ToUpper
        if isInRange (charInput |> charToInt ) then
            if not(setAlphabet.Contains charInput) then
                setAlphabet  <- setAlphabet.Add(charInput)
    
    setAlphabet.Count = 26