module ImportResolver

open System
open System.Collections.Generic
open IImportResolver

type ImportResolver(imageBase: uint64, importAddressTableBytes: ArraySegment<byte>, importBytes: ArraySegment<byte>) =
    let _imageBase: uint64 = imageBase
    let _importAddressTableBytes: ArraySegment<byte> = importAddressTableBytes
    let _importBytes: ArraySegment<byte> = importBytes
    let _imports: Dictionary<int, string> = new Dictionary<int, string>()

    interface IImportResolver with
        member self.IsImportAddress(address: uint64): bool =
            let virtualAddress = address - _imageBase
            virtualAddress >= (uint64)_importAddressTableBytes.Offset &&
                virtualAddress <= (uint64)(_importAddressTableBytes.Offset + _importAddressTableBytes.Count - 4)

        member self.GetImportName(address: uint64): string =
            let entryAddress = ArrayReader.readUInt32 _importAddressTableBytes.Array ((int)(address - _imageBase))
            let hint = ArrayReader.readUInt16 _importBytes.Array ((int)entryAddress)
            let hasValue, name = _imports.TryGetValue((int)hint)
            if hasValue then
                let name = ArrayReader.readZString _importBytes.Array ((int)entryAddress + 2)
                _imports.[(int)hint] <- name;
                name
            else
                name
