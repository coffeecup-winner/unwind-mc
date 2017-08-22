using BenchmarkDotNet.Running;

namespace UnwindMC.Benchmarks
{
    class Program
    {
        static void Main(string[] args)
        {
            BenchmarkRunner.Run<PatternMatchingBenchmark2>();
        }
    }
}
