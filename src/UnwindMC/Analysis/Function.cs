using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace UnwindMC.Analysis
{
    public class Function
    {
        public Function(ulong offset)
        {
            Offset = offset;
        }

        public ulong Offset { get; }
    }
}
