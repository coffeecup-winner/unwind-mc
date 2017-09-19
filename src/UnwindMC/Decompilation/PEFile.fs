module PEFile

open System
open System.Text
open PeNet
open PeNet.Structures

type T(file: byte[], pe: PeFile, text: IMAGE_SECTION_HEADER, rdata: IMAGE_SECTION_HEADER, data: IMAGE_SECTION_HEADER, rsrc: IMAGE_SECTION_HEADER) =
    let _file: byte[] = file
    let _pe: PeFile = pe
    let _text: IMAGE_SECTION_HEADER = text
    let _rdata: IMAGE_SECTION_HEADER = rdata
    let _data: IMAGE_SECTION_HEADER = data
    let _rsrc: IMAGE_SECTION_HEADER = rsrc

    let getSectionBytes (header: IMAGE_SECTION_HEADER ): ArraySegment<byte> =
        new ArraySegment<byte>(_file, int header.PointerToRawData, int header.SizeOfRawData)

    let getDataDirectory(index: Constants.DataDirectoryIndex): IMAGE_DATA_DIRECTORY =
        _pe.ImageNtHeaders.OptionalHeader.DataDirectory.[int index]

    let getDataDirectoryBytes(index: Constants.DataDirectoryIndex): ArraySegment<byte> =
        let data = getDataDirectory(index)
        new ArraySegment<byte>(_file, int data.VirtualAddress, int data.Size)

    member self.imageBase: uint64 = _pe.ImageNtHeaders.OptionalHeader.ImageBase
    member self.textSectionAddress: uint64 = self.imageBase + uint64 _text.VirtualAddress
    member self.entryPointAddress: uint64 = self.imageBase + uint64 _pe.ImageNtHeaders.OptionalHeader.AddressOfEntryPoint
    member self.getTextBytes(): ArraySegment<byte> = getSectionBytes(_text)
    member self.getImportBytes(): ArraySegment<byte> = getDataDirectoryBytes(Constants.DataDirectoryIndex.Import)
    member self.getImportAddressTableBytes(): ArraySegment<byte> = getDataDirectoryBytes(Constants.DataDirectoryIndex.IAT)

let load (filename: string): T =
    let file = System.IO.File.ReadAllBytes(filename)
    let pe = new PeFile(file)
    let mutable text: IMAGE_SECTION_HEADER = null
    let mutable rdata: IMAGE_SECTION_HEADER = null
    let mutable data: IMAGE_SECTION_HEADER = null
    let mutable rsrc: IMAGE_SECTION_HEADER = null
    for section in pe.ImageSectionHeaders do
        match (Encoding.ASCII.GetString(section.Name).TrimEnd('\000')) with
        | ".text" -> text <- section
        | ".rdata" -> rdata <- section
        | ".data" -> data <- section
        | ".rsrc" -> rsrc <- section
        | _ -> ()
    T(file, pe, text, rdata, data, rsrc)
