using System;
using System.Collections.Generic;
using UnwindMC.Util;

namespace UnwindMC.Analysis
{
    public class ImportResolver
    {
        private readonly ulong _imageBase;
        private readonly ArraySegment<byte> _importAddressTableBytes;
        private readonly ArraySegment<byte> _importBytes;
        private readonly Dictionary<int, string> _imports = new Dictionary<int, string>();

        public ImportResolver(ulong imageBase, ArraySegment<byte> importAddressTableBytes, ArraySegment<byte> importBytes)
        {
            _imageBase = imageBase;
            _importAddressTableBytes = importAddressTableBytes;
            _importBytes = importBytes;
        }

        public bool IsImportAddress(ulong address)
        {
            var virtualAddress = address - _imageBase;
            return virtualAddress >= (uint)_importAddressTableBytes.Offset &&
                virtualAddress <= (uint)(_importAddressTableBytes.Offset + _importAddressTableBytes.Count - 4);
        }

        public string GetImportName(ulong address)
        {
            uint entryAddress = _importAddressTableBytes.Array.ReadUInt32((int)(address - _imageBase));
            ushort hint = _importBytes.Array.ReadUInt16((int)entryAddress);
            string name;
            if (!_imports.TryGetValue(hint, out name))
            {
                name = _importBytes.Array.ReadZString((int)entryAddress + 2);
                _imports[hint] = name;
            }
            return name;
        }
    }
}
