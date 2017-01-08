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
            var pe = PEFile.Load(args[0]);
            var decompiler = new Decompiler();
            var instructions = decompiler.Decompile(pe);
            Console.WriteLine(decompiler.Dump(instructions));
        }
    }
}
