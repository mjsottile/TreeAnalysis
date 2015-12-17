
#r "bin/debug/TreeAnalysis.dll"

// Define your library scripting code here

open TreeAnalysis

open TreeAnalysis.Yang
open TreeAnalysis.Types

let t1 = { label = "a"; children = [] }
let t2 = { label = "b"; children = [] }
let t3 = { label = "c"; children = [t1;t2] }

let t4 = { label = "c"; children = [t1;t1] }

let lblcmp (s1:string) (s2:string) = s1=s2

printfn "%A" t3
printfn "%A" t4

let d1 = treedist t3 t4 lblcmp
let d2 = treedist t3 t3 lblcmp

printfn "%A" d1
printfn "%A" d2

let (a,b) = treediff t3 t4 lblcmp

printfn "%A" (a,b)

0
