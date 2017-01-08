using System;

namespace UnwindMC
{
    class Program
    {
        static void Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.WriteLine("Usage: unwind-mc <exe-file>");
                return;
            }
            new Decompiler().Decompile(PEFile.Load(args[0]));
        }
    }
}
