module IImportResolver

type IImportResolver =
    abstract member IsImportAddress: uint64 -> bool
    abstract member GetImportName: uint64 -> string
