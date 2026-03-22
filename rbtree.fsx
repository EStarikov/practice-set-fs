type Color = 
    | Red
    | Black

type Tree<'T> = 
    | Empty
    | Node of color: Color * left: Tree<'T> * value: 'T * right: Tree<'T>

type Result<'T> =
    | Done of 'T
    | ToDo of 'T

let rec contains tree v =
    match tree with 
    | Empty -> false 
    | Node(_, left, value, right ) ->
    if value > v then contains left v
    elif value < v then contains right v
    else true

let balance tree  = 
    match tree with
    | Node(Black,Node(Red, Node(Red, a, x, b), y, c), z, d )
    | Node(Black, Node(Red, a, x, Node(Red, b, y, c)), z, d)
    | Node(Black, a, x, Node(Red, Node(Red, b, y, c), z, d))
    | Node(Black, a, x, Node(Red, b, y, Node(Red, c, z, d))) ->
        ToDo (Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d)))
    | Node(Black, a, x, b) as n -> Done (n)
    | Node(Red, a, x, b) as n -> ToDo (n)
    | Empty -> Done (Empty)


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

let tree0 = Empty
let tree1 = insert tree0 5
let tree2 = insert tree1 3
let tree3 = insert tree2 7
let tree4 = insert tree3 1
let tree5 = insert tree4 9
let tree6 = insert tree5 4

printfn "Contains 5? %b" (contains tree6 5)
printfn "Contains 6? %b" (contains tree6 6)
