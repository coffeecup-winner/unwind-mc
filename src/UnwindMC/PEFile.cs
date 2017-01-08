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
        public uint TextOffset => _text.VirtualAddress;

        public ArraySegment<byte> GetTextBytes() =>
            GetSectionBytes(_text);

        private ArraySegment<byte> GetSectionBytes(IMAGE_SECTION_HEADER header) =>
            new ArraySegment<byte>(_file, (int)header.PointerToRawData, (int)header.SizeOfRawData);
    }
}
