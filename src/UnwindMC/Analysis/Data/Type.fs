module Type

open IL

type DataType =
    | Int32
    | Function // void (*)(void)
    | Pointer of DataType

let sizeOf (type_: DataType): int =
    Constants.RegisterSize

let rec isFunction (type_: DataType): bool =
    match type_ with
    | Function -> true
    | Pointer t -> isFunction t
    | _ -> false

type ResolvedOperand = ILOperand * int option
