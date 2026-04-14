module Rbtree.Tests

open Rbtree
open Xunit 

let rec blHeightInv tree = 
    match tree with 
    | Empty -> 0
    | Node(color, l, _, r) -> 
        let lH = blHeightInv l 
        let rH = blHeightInv r
        if lH = -1 || rH = -1 || lH <> rH then 
            -1
        else if color = Red then 
            lH 
        else 
            lH + 1

let rec heightInv tree =
    match tree with 
    | Empty -> 0
    | Node(_, l, _, r) -> 
        let lH = heightInv l + 1
        let rH = heightInv r + 1
        if lH = -1 || rH = -1 || lH <> rH then 
            -1
        else 
            lH




[<Fact>]
let oneElement() = 
    let t1 = Empty
    let t2 = insert t1 4
    Assert.True(contains t2 4)
    Assert.Equal(1, blHeightInv t2)
    Assert.Equal(1, heightInv t2)




