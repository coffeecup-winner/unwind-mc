using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Newtonsoft.Json;
using Newtonsoft.Json.Linq;
using UnwindMC.Tests.Helpers;

namespace UnwindMC.Tests.SourceTests
{
    static class VcTools
    {
        private static readonly string ClExePath;
        private static readonly string DumpBinPath;

        static VcTools()
        {
            var vswhereExePath = Path.Combine(
                Environment.GetEnvironmentVariable("ProgramFiles(x86)"),
                "Microsoft Visual Studio", "Installer", "vswhere.exe");
            var json = Run(null, vswhereExePath, "-format", "json");
            var vsPath = JsonConvert
                .DeserializeObject<JArray>(json)
                .First(o => o["installationVersion"].ToObject<string>().StartsWith("15."))
                ["installationPath"]
                .ToObject<string>();
            var toolsRootDir = Path.Combine(vsPath, "VC", "Tools", "MSVC");
            var toolsDir = Path.Combine(
                Directory.EnumerateDirectories(toolsRootDir).Last(),
                "bin", "HostX86", "x86");
            ClExePath = Path.Combine(toolsDir, "cl.exe");
            DumpBinPath = Path.Combine(toolsDir, "dumpbin.exe");
        }

        static string Run(string workingDirectory, string exePath, params string[] arguments)
        {
            using (var process = new Process())
            {
                process.StartInfo = new ProcessStartInfo(exePath, string.Join(" ", arguments))
                {
                    CreateNoWindow = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    WorkingDirectory = workingDirectory,
                };
                process.Start();
                process.WaitForExit();
                if (process.ExitCode != 0)
                {
                    throw new InvalidOperationException(process.StandardError.ReadToEnd());
                }
                return process.StandardOutput.ReadToEnd();
            }
        }

        public static string Compile(string filename)
        {
            Run(Path.GetDirectoryName(filename), ClExePath, "/nologo", "/c", "/Os", filename);
            return Path.ChangeExtension(filename, "obj");
        }

        public static string Disassemble(string filename)
        {
            var lines = Run(Path.GetDirectoryName(filename), DumpBinPath, "/disasm:bytes", filename)
                .Split(new [] { '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries)
                .Select(l => l.Trim())
                .SkipWhile(l => l != "File Type: COFF OBJECT")
                .Skip(2) // Skip file type and signature
                .TakeWhile(l => l != "Summary");
            return string.Join("\n", lines);
        }

        public static TempDirectory CreateTempDirectory()
        {
            var tempFileName = Path.GetTempFileName();
            File.Delete(tempFileName);
            return new TempDirectory(tempFileName);
        }
    }
}
