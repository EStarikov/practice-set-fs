type Color = 
    | Red
    | Black

type Tree<'T> = 
    | Empty
    | Node of color: Color * left: Tree<'T> * value: 'T * right: Tree<'T>

type Result<'T> =
    | Done of 'T
    | ToDo of 'T

//проверка на наличие
let rec contains tree v =
    match tree with 
    | Empty -> false 
    | Node(_, left, value, right ) ->
    if value > v then contains left v
    elif value < v then contains right v
    else true

//вставка
let balance tree  = 
    match tree with
    | Node(Black,Node(Red, Node(Red, a, x, b), y, c), z, d )
    | Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
    | Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
    | Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
        ToDo (Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d)))
    | Node(Black, a, x, b) as n -> Done (n)
    | _ -> ToDo (tree)
    
let rec insertRec tree v =
    match tree with 
    | Empty -> ToDo (Node(Red, Empty, v, Empty))
    | Node(color, left, value, right) ->
        if value > v then 
            let newLeft = insertRec left v 
            match newLeft with 
                | Done nl -> Done (Node(color, nl, value, right))
                | ToDo nl -> balance (Node(color, nl, value, right))
        elif value < v then
            let newRight = insertRec right v 
            match newRight with 
                | Done nr -> Done (Node(color, left, value, nr))
                | ToDo nr -> balance (Node(color, left, value, nr))
        else  
            Done (tree)

let insert tree v =
    let newTree = insertRec tree v
    let t= 
        match newTree with 
        | Done t -> t
        | ToDo t  -> t
    match t with 
    | Node(Red, a, x, b) -> Node(Black, a, x, b)
    | _ -> t

//удаление
let blacken tree =
    match tree with
    | Node(Red, a ,x, b) -> Done (Node(Black, a, x, b))
    | _ -> ToDo tree
    
let balanceDel tree =
    match tree with
    | Node(color,Node(Red, Node(Red, a, x, b), y, c), z, d )
    | Node(color, Node(Red, a, x, Node(Red, b, y, c)), z, d)
    | Node(color, a, x, Node(Red, Node(Red, b, y, c), z, d))
    | Node(color, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
        Done (Node(color, Node(Black, a, x, b), y, Node(Black, c, z, d)))
    | _ -> blacken tree

let rec eqL tree =
    match tree with 
    | Node(color, a, x, Node(Black, b, y, c)) -> balanceDel (Node(color, a, x, Node(Red, b, y, c)))
    | Node(color, a, x, Node(Red, b, y, c)) -> 
        let newLeft = eqL (Node(Red, a, x, b))
        match newLeft with 
        | Done nl -> Done (Node(Black, nl, y, c))
        | ToDo nl -> ToDo (Node(Black, nl, y, c))

let rec eqR tree =
    match tree with 
    | Node(color, Node(Black, a, x, b), y, c) -> balanceDel (Node(color, Node(Red, a, x, b), y, c))
    | Node(color, Node(Red, a, x, b), y, c) -> 
        let newRight = eqR (Node(Red, b, y, c))
        match newRight with
        | Done nr -> Done (Node(Black, a, x, nr))
        | ToDo nr -> ToDo (Node(Black, a, x, nr))

let rec delMin tree =
    match tree with 
    | Node(Red, Empty, x, b) -> (Done b, x)
    | Node(Black, Empty, x, b) -> (blacken b, x)
    | Node(color, a, x, b) -> 
        let (an, min) = delMin a
        match an with 
        | Done t -> (Done (Node(color, t, x, b)), min)
        | ToDo t -> (eqL (Node(color, t, x, b)), min)

let delCur tree =
    match tree with 
    | Node(Red, a, y, Empty) -> Done a
    | Node(Black, a, x, Empty) -> blacken a
    | Node(color, a, x, b) -> 
        let (bn, min) = delMin b
        match bn with 
        | Done t -> Done (Node(color, a, min, t))
        | ToDo t  -> eqR (Node(color, a, min, t))
    
let rec deleteRec tree v =
    match tree with
    | Empty -> Done (Empty)
    | Node(color, left,value , right) ->
        if  value > v then 
            let newLeft = deleteRec left v
            match newLeft with 
            | Done nl -> Done (Node(color, nl, value, right))
            | ToDo nl -> eqL (Node(color, nl, value, right))
        elif value < v then
            let newRight = deleteRec right v
            match newRight with 
            | Done nr -> Done (Node(color, left, value, nr))
            | ToDo nr -> eqR (Node(color, left, value, nr))
        else 
            delCur tree 

let delete tree v =
    let newTree = deleteRec tree v
    let t= 
        match newTree with 
        | Done t -> t
        | ToDo t  -> t
    match t with 
    | Node(Red, a, x, b) -> Node(Black, a, x, b)
    | _ -> t

//join
let justTree resultTree =
    match resultTree with
    | Done t -> t
    | ToDo t -> t

let rec blackHeight tree=
    match tree with 
    | Empty -> 0
    | Node(Red, l, _, _) -> blackHeight l
    | Node(Black, l, _, _) -> 1 + (blackHeight l) 

let rec joinLT t1 g t2 targetHeight currentHeight=
    if targetHeight = currentHeight then Node(Red, t1, g, t2)
    else 
        match t2 with 
        | Node(Red, l, x, r) -> 
            let newLeft = joinLT t1 g l targetHeight currentHeight
            justTree (balance (Node(Red, newLeft, x, r)))
        | Node(Black, l, x, r) -> 
            let newLeft = joinLT t1 g l targetHeight (currentHeight - 1)
            justTree (balance (Node(Black, newLeft, x, r)))

let rec joinRT t1 g t2 targetHeight currentHeight =
    if targetHeight = currentHeight then Node(Red, t1, g, t2)
    else 
        match t1 with 
        | Node(Red, l, x, r) -> 
            let newRight = joinRT t2 g r targetHeight currentHeight
            justTree (balance (Node(Red, l, x, newRight)))
        | Node(Black, l, x, r) -> 
            let newRight = joinRT t2 g r targetHeight (currentHeight - 1)
            justTree (balance (Node(Black, l, x, newRight)))

let join t1 g t2 =
    let h1 = blackHeight t1
    let h2 = blackHeight t2
    if h1 = 0 then insert t2 g 
    else if h2 = 0 then insert t1 g
    else 
        if h1 < h2 then 
            let t  =joinLT t1 g t2 h1 h2
            match t with 
            | Node(Red, a, x, b) -> Node(Black, a, x, b)
            | _ -> t
        else if h1 > h2 then 
            let t = joinRT t1 g t2 h2 h1
            match t with 
            | Node(Red, a, x, b) -> Node(Black, a, x, b)
            | _ -> t
        else 
            Node(Black, t1, g, t2)

let tree0 = Empty
let tree1 = insert tree0 5
let tree2 = insert tree1 3
let tree3 = insert tree2 7
let tree4 = insert tree3 1
let tree5 = insert tree4 9
let tree6 = insert tree5 4

printfn "Contains 5? %b" (contains tree6 5)
let tree7 = delete tree6 5
printfn "Contains 5? %b" (contains tree7 5)
printfn "Contains 6? %b" (contains tree7 6)
printfn "Contains 1? %b" (contains tree7 1)
