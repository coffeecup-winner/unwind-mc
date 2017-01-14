using PeNet;
using PeNet.Structures;
using System;
using System.IO;
using System.Text;

namespace UnwindMC
{
    public class PEFile
    {
        private readonly byte[] _file;
        private readonly PeFile _pe;
        private readonly IMAGE_SECTION_HEADER _text;
        private readonly IMAGE_SECTION_HEADER _rdata;
        private readonly IMAGE_SECTION_HEADER _data;
        private readonly IMAGE_SECTION_HEADER _rsrc;

        private PEFile(string filename)
        {
            _file = File.ReadAllBytes(filename);
            _pe = new PeFile(_file);
            foreach (var section in _pe.ImageSectionHeaders)
            {
                switch (Encoding.ASCII.GetString(section.Name).TrimEnd('\0'))
                {
                    case ".text": _text = section; break;
                    case ".rdata": _rdata = section; break;
                    case ".data": _data = section; break;
                    case ".rsrc": _rsrc = section; break;
                }
            }
        }

        public static PEFile Load(string filename)
        {
            return new PEFile(filename);
        }

        public ulong ImageBase => _pe.ImageNtHeaders.OptionalHeader.ImageBase;
        public ulong TextSectionAddress => ImageBase + _text.VirtualAddress;
        public ulong EntryPointAddress => ImageBase + _pe.ImageNtHeaders.OptionalHeader.AddressOfEntryPoint;

        public ArraySegment<byte> GetTextBytes() =>
            GetSectionBytes(_text);

        public ArraySegment<byte> GetImportBytes() =>
            GetDataDirectoryBytes(Constants.DataDirectoryIndex.Import);

        public ArraySegment<byte> GetImportAddressTableBytes() =>
            GetDataDirectoryBytes(Constants.DataDirectoryIndex.IAT);

        private IMAGE_DATA_DIRECTORY GetDataDirectory(Constants.DataDirectoryIndex index) =>
            _pe.ImageNtHeaders.OptionalHeader.DataDirectory[(int)index];

        private ArraySegment<byte> GetSectionBytes(IMAGE_SECTION_HEADER header) =>
            new ArraySegment<byte>(_file, (int)header.PointerToRawData, (int)header.SizeOfRawData);

        private ArraySegment<byte> GetDataDirectoryBytes(Constants.DataDirectoryIndex index)
        {
            var data = GetDataDirectory(index);
            return new ArraySegment<byte>(_file, (int)data.VirtualAddress, (int)data.Size);
        }
    }
}
