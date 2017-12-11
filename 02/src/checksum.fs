open System


let lines : seq<string>  =
    Seq.initInfinite (fun _ -> Console.ReadLine()) |> Seq.takeWhile (fun x -> match x with null -> false | _ -> true)


let nums : seq<int[]> =
    lines |> Seq.map ((fun s -> s.Split(' ')) >> (fun arr -> arr |> Array.map Int32.Parse))


let checksums (f:int[]->int) : seq<int> =
    nums |> Seq.map f


let minMaxDifference arr = Array.max arr - Array.min arr


[<EntryPoint>]
let main args =
    let total = (checksums minMaxDifference) |> Seq.sum
    printfn "%i" total
    0
