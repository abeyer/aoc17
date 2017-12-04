open System

open Fuchu


let offsetList (lst : 'a list) (offset : int) : 'a list =
    if lst.Length = 0 then
        lst
    else
        let twice = lst @ lst
        let snd = lst |> Seq.take offset |> Seq.toList
        let fst = twice |> Seq.skip offset |> Seq.take (lst.Length - offset) |> Seq.toList

        fst @ snd


let sumOffsetEqual (nums : int list) (offset : int) : int =
    let offsetNums = offsetList nums offset
    let accumulateEquals acc a b = if a = b then acc + a else acc

    List.fold2 accumulateEquals 0 nums offsetNums


let intOfChar (c : char) : int =
    match c with
    | '0' -> 0
    | '1' -> 1
    | '2' -> 2
    | '3' -> 3
    | '4' -> 4
    | '5' -> 5
    | '6' -> 6
    | '7' -> 7
    | '8' -> 8
    | '9' -> 9
    | _ -> 0


let splitDigits (numString : string) : int list =
    [for c in numString do yield (intOfChar c)]


let captcha (numString : string) =
    sumOffsetEqual (splitDigits numString) 1


let captcha2 (numString : string) =
    let nums = splitDigits numString
    sumOffsetEqual nums (nums.Length / 2)


[<EntryPoint>]
let main args =
    if args.Length = 0 || args.[0] = "--test" then
        Assert.Equal("1,2,3 << 0 = 1,2,3", [1;2;3], offsetList [1;2;3] 0)
        Assert.Equal("1,2,3 << 1 = 2,3,1", [2;3;1], offsetList [1;2;3] 1)
        Assert.Equal("1,2,3 << 2 = 3,1,2", [3;1;2], offsetList [1;2;3] 2)
        Assert.Equal("1,2,3 << 3 = 1,2,3", [1;2;3], offsetList [1;2;3] 3)
        Assert.Equal("splitDigits", [1;2;3], splitDigits "123")
        Assert.Equal("'123' << 1 = 2,3,1", [2;3;1], offsetList (splitDigits "123") 1)
        Assert.Equal("'1234' << 1 = 2,3,4,1", [2;3;4;1], offsetList (splitDigits "1234") 1)
        Assert.Equal("1122 : 3", 3, captcha "1122")
        Assert.Equal("1111 : 4", 4, captcha "1111")
        Assert.Equal("1234 : 0", 0, captcha "1234")
        Assert.Equal("91212129 : 9", 9, captcha "91212129")
        Assert.Equal("'' : 0", 0, captcha "")
        Assert.Equal("1212 : 6", 6, captcha2 "1212")
        Assert.Equal("1221 : 0", 0, captcha2 "1221")
        Assert.Equal("123425 : 4", 4, captcha2 "123425")
        Assert.Equal("123123 : 12", 12, captcha2 "123123")
        Assert.Equal("12131415: 4", 4, captcha2 "12131415")
    else if args.[0] = "1" then
        printfn "%i" (captcha args.[1])
    else if args.[0] = "2" then
        printfn "%i" (captcha2 args.[1])
    else
        printfn "no matching captcha"
    0
