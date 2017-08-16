using System;
using System.IO;

namespace UnwindMC.Tests.Helpers
{
    class TempDirectory : IDisposable
    {
        public TempDirectory(string dirPath)
        {
            Path = dirPath;
            Directory.CreateDirectory(Path);
        }
        public string Path { get; }

        public void Dispose()
        {
            Directory.Delete(Path, recursive: true);
        }
    }
}
