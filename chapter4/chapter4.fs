module chapter4

type info = string * uint32

type term = 
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term

let rec isNumericValue t = 
  match t with
  | TmZero (_) -> true
  | TmSucc (_, t1) -> isNumericValue t1 
  | _ -> false

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
