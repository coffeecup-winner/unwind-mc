module ArrayReader

open System.Text

let readUInt16 (bytes: byte[]) (index: int): uint16 =
    let mutable result = 0us
    result <- result + (uint16)bytes.[index + 1]
    result <- result <<< 8
    result <- result + (uint16)bytes.[index]
    result

let readUInt32 (self: byte[]) (index: int): uint32 =
    let mutable result = 0u
    result <- result + (uint32)self.[index + 3]
    result <- result <<< 8
    result <- result + (uint32)self.[index + 2]
    result <- result <<< 8
    result <- result + (uint32)self.[index + 1]
    result <- result <<< 8
    result <- result + (uint32)self.[index]
    result

let readZString (self: byte[]) (index: int): string =
    let sb = new StringBuilder()
    let mutable b = self.[index]
    let mutable index = index + 1
    while b <> 0uy do
        sb.Append((char)b) |> ignore
        b <- self.[index]
        index <- index + 1
    sb.ToString()
