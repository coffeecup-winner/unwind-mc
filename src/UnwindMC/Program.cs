using System;
using UnwindMC.Decompilation;

namespace UnwindMC
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: unwind-mc <project-root-path>");
                return;
            }
            new Decompiler(DecompilationProject.Load(args[0])).Decompile();
        }
    }
}
