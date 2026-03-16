open System

type Color = 
    | Red
    | Black

type Tree<'T> = 
    | Empty
    | Node of color: Color * left: Tree<'T> * value: 'T * right: Tree<'T>

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
        Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
    | _ -> tree 


let rec insertRec tree v =
    match tree with 
    | Empty -> Node(Red, Empty, v, Empty)
    | Node(color, left, value, right) ->
        if value > v then 
            let leftSubtree = insertRec left v 
            let newTree = Node(color, leftSubtree, value, right)
            balance newTree 
        elif value < v then
            let rightSubtree = insertRec right v
            let newTree = Node(color, left, value, rightSubtree)
            balance newTree
        else  
            tree

let insert tree v =
    let newTree = insertRec tree v
    match newTree with 
    | Node(Red, left, value, right) -> Node(Black, left, value, right) 
    | _ -> newTree

let tree0 = Empty
let tree1 = insert tree0 5
let tree2 = insert tree1 3
let tree3 = insert tree2 7
let tree4 = insert tree3 1
let tree5 = insert tree4 9
let tree6 = insert tree5 4

printfn "Contains 4? %b" (contains tree6 4)
printfn "Contains 6? %b" (contains tree6 6)
