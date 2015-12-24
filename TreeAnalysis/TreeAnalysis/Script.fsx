
#r "bin/debug/TreeAnalysis.dll"

// Define your library scripting code here

open TreeAnalysis

open TreeAnalysis.Yang
open TreeAnalysis.Types

// blog post stuff

let yellow1 = { label = "yellow"; children = [] }
let yellow2 = { label = "yellow"; children = [] }

let rchild1 = { label = "rchild"; children = [] }
let red1 = { label = "red"; children = [rchild1] }
let rchild2 = { label = "rchild"; children = [] }
let red2 = { label = "red"; children = [rchild1] }
let green = {label = "green"; children = []}
let orange = {label = "orange"; children = []}

let purp1 = { label = "purple"; children = [orange;yellow1;red1]}
let purp2 = { label = "purple"; children = [yellow2;green;red2]}

let lblcmp (s1:string) (s2:string) = s1=s2

let (p1,p2) = treediff purp1 purp2 lblcmp
let d = treedist purp1 purp2 lblcmp

// other tests

let t1 = { label = "a"; children = [] }
let t2 = { label = "b"; children = [] }
let t3 = { label = "c"; children = [t1;t2] }

let t4 = { label = "c"; children = [t1;t1] }


printfn "%A" t3
printfn "%A" t4

let d1 = treedist t3 t4 lblcmp
let d2 = treedist t3 t3 lblcmp

printfn "%A" d1
printfn "%A" d2

let (a,b) = treediff t3 t4 lblcmp

printfn "%A" (a,b)

0
