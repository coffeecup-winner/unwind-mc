using BenchmarkDotNet.Running;

namespace UnwindMC.Benchmarks
{
    class Program
    {
        static void Main(string[] args)
        {
            //var b = new PatternMatchingBenchmark();
            //b.Index = 1;
            //b.Setup();
            //while (true)
            //{
            //    b.PatternMatching();
            //}
            BenchmarkRunner.Run<PatternMatchingBenchmark>();
        }
    }
}
