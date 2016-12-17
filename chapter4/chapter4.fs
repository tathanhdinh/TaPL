module chapter4

type info = string * int

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
  | TmZero(_) -> true
  | TmSucc(_, t1) -> isNumericValue t1 
  | _ -> false

exception NoRuleApplies

let dummyInfo = ("", -1)

let rec eval' t =
  match t with
  | TmIf(_, TmTrue(_), t2, t3) -> t2
  | TmIf(_, TmFalse(_), t2, t3) -> t3
  | TmIf(fi, t1, t2, t3) -> let t1' = eval' t1 
                            TmIf(fi, t1', t2, t3)
  | TmSucc(fi, t1) -> let t1' = eval' t1 
                      TmSucc(fi, t1')
  | TmPred(_, TmZero(_)) -> TmZero(dummyInfo)
  | TmPred(_, TmSucc(_, nv1)) when isNumericValue nv1 -> nv1
  | TmPred(fi, t1) -> let t1' = eval' t1
                      TmPred(fi, t1')
  | TmIsZero(_, TmZero(_)) -> TmTrue(dummyInfo)
  | TmIsZero(_, TmSucc(_, nv1)) when isNumericValue nv1 -> TmFalse(dummyInfo)
  | TmIsZero(fi, t1) -> let t1' = eval' t1 in TmIsZero(fi, t1')
  | _ -> raise NoRuleApplies

let rec eval t =
  try 
    let t' = eval' t
    eval t'
  with
    | :? NoRuleApplies -> Printf.printfn "cannot evaluate" 

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    0 // return an integer exit code
