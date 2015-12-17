namespace TreeAnalysis

//
// implementation of tree matching method from Yang's 1991 paper
// port of Haskell implementation to F#
//
// matt@sailfan.co // matt@galois.com
//

module Yang =
    open TreeAnalysis.Types

    type Direction = Left | Up | Diag

    type EOp = Keep | Delete

    type EditTree<'a> = ENode of 'a * ((EOp * EditTree<'a>) list)
                      | ELeaf of Tree<'a>
                      | ENil

    let rec yang ta tb lblcmp =
        let dirToOp = function
          | Up   -> (Some Delete, None)
          | Left -> (None,        Some Delete)
          | Diag -> (Some Keep,   Some Keep)
    

        let ak = Array.ofList (ta.children)
        let bk = Array.ofList (tb.children)

        let lena = Array.length ak
        let lenb = Array.length bk

        let maxByFirst = List.maxBy (fun (a,_,_) -> a)

        let ytable = Array2D.create (lena+1) (lenb+1) (0, Diag, (ENil, ENil))
        
        let (@!@) i j = let (a,_,_) = ytable.[i,j] in a
        let (@+@) i j = let (_,b,_) = ytable.[i,j] in b
        let (@%@) i j = let (_,_,c) = ytable.[i,j] in c

        for i = 0 to lena do
          for j = 0 to lenb do
            match (i,j) with
              | (0,0) -> ytable.[i,j] <- (0, Diag, (ENil, ENil))
              | (0,_) -> ytable.[i,j] <- (0, Left, (ENil, ELeaf (bk.[j-1])))
              | (_,0) -> ytable.[i,j] <- (0, Up,   (ELeaf (ak.[i-1]), ENil))
              | (_,_) -> let (ijscore, (ijl, ijr)) = yang (ak.[i-1]) (bk.[j-1]) lblcmp
                         let a = ( (i-1 @!@ j-1) + ijscore, Diag, (ijl, ijr) )
                         let b = ( (i-1 @!@ j  ),           Up,   (ijl, ijr) )
                         let c = ( (i   @!@ j-1),           Left, (ijl, ijr) )
                         ytable.[i,j] <- maxByFirst [a;b;c]

        let rec traceback = function
          | (0,0) -> []
          | (x,y) -> let move = x @+@ y
                     let (l,r) = x @%@ y
                     match move with
                     | Up    -> ((x,y), Up, (l,r)) :: (traceback ((x-1),y))
                     | Left  -> ((x,y), Left, (l,r)) :: (traceback (x,(y-1)))
                     | Diag  -> ((x,y), Diag, (l,r)) :: (traceback ((x-1),(y-1)))

        let score = if lblcmp ta.label tb.label then 1 + (lena @!@ lenb) else 0

        // Note: unrolled cascade of |>'s to individual lets to narrow down place where
        // bug appears.
        let tbl = traceback (lena, lenb)
        let rtbl = List.rev tbl

        // CRASH ORIGINATES HERE: the dirToOp call works, but then a crash occurs when the
        // return tuple is constructed.  [Print statements used to narrow this down removed]
        // Works fine w/ F# under Visual Studio 2015 on Windows 10.
        let mtbl =  List.map (fun ((a,b),d,(l,r)) -> let (opl, opr) = dirToOp d
                                                     ((opl,l),(opr,r))) rtbl 
        let (tba, tbb) = List.unzip mtbl

        let removeNones = List.choose (fun (x,y) -> match x with
                                                    | Some s -> Some (s,y)
                                                    | None   -> None)

        let aekids = tba |> removeNones
        let bekids = tbb |> removeNones

        let (reta, retb) = if (score = 0) then (ELeaf ta, ELeaf tb)
                                          else (ENode (ta.label,aekids),
                                                ENode (tb.label,bekids))

        (score, (reta, retb))

    let rec reconstruct = function
    | ELeaf t         -> t
    | ENil            -> failwith "Undefined behavior"
    | ENode (l, kids) -> {label = l; children = (List.map (fun (_,kid) -> reconstruct kid) kids)}
    
    let sanitize = function
    | (Keep, t)   -> (Keep, t)
    | (Delete, t) -> (Delete, ELeaf (reconstruct t))
    
    let cleaner = function
    | ELeaf t           -> ELeaf t
    | ENil              -> ENil
    | ENode (lbl, kids) -> ENode (lbl, (List.map sanitize kids))
    
    let treedist t1 t2 lblcmp = let (score, _) = yang t1 t2 lblcmp in score
    let treediff t1 t2 lblcmp = let (_,(y1,y2)) = yang t1 t2 lblcmp in (cleaner y1, cleaner y2)

    let rec etreeToLTree = function
    | ELeaf l -> l
    | ENode (l, kids) -> { label=l; children= (List.map (fun (_,kid) -> etreeToLTree kid) kids) }
    | ENil -> failwith "Fatal etreeToLTree error: enil"