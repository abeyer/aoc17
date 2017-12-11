open System


let lines : seq<string>  =
    Seq.initInfinite (fun _ -> Console.ReadLine()) |> Seq.takeWhile (fun x -> match x with null -> false | _ -> true)


let nums : seq<int[]> =
    lines |> Seq.map ((fun s -> s.Split(' ')) >> (fun arr -> arr |> Array.map Int32.Parse))


let checksums (f:int[]->int) : seq<int> =
    nums |> Seq.map f


let minMaxDifference arr = Array.max arr - Array.min arr


let twoCombinations (arr:'a[]) =
    let rec comb (acc:('a*'a) list) (l:'a list) (x:'a) (r:'a list) : ('a*'a) list =
        match r with
            | hd::tl -> (comb (acc @ (List.map (fun y -> (x, y)) l) @ (List.map (fun y -> (y, x)) l)) (x::l) hd tl)
            | _ -> acc @ (List.map (fun y -> (x, y)) l) @ (List.map (fun y -> (y, x)) l)
    let hd::tl = List.ofArray(arr)
    comb [] [] hd tl


let divideResult arr =
    let rec divide l =
        match l with
            | (x,y)::tl when x % y = 0 -> x / y
            | _::tl -> divide tl
    divide (twoCombinations arr)


[<EntryPoint>]
let main args =
    let total = (checksums divideResult) |> Seq.sum
    printfn "%i" total
    0
