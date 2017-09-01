module rec VcTools

open System
open System.Diagnostics
open System.IO
open System.Linq
open Newtonsoft.Json
open Newtonsoft.Json.Linq

let private findVcToolsPath(): string =
    let vswhereExePath =
        Path.Combine(
            Environment.GetEnvironmentVariable("ProgramFiles(x86)"),
            "Microsoft Visual Studio", "Installer", "vswhere.exe")
    let json = run null vswhereExePath [| "-format"; "json" |]
    let vsPath =
        JsonConvert
            .DeserializeObject<JArray>(json)
            .FirstOrDefault(fun o -> o.["installationVersion"].ToObject<string>().StartsWith("15."))
            .["installationPath"]
            .ToObject<string>()
    let toolsRootDir = Path.Combine(vsPath, "VC", "Tools", "MSVC")
    Path.Combine(
        Directory.EnumerateDirectories(toolsRootDir).Last(),
        "bin", "HostX86", "x86")

let private vcToolsPath = findVcToolsPath()
let private ClExePath = Path.Combine(vcToolsPath, "cl.exe")
let private DumpBinPath = Path.Combine(vcToolsPath, "dumpbin.exe")

let private run (workingDirectory: string) (exePath: string) (arguments: string array): string =
    use process = new Process()
    process.StartInfo <- new ProcessStartInfo(exePath, System.String.Join(" ", arguments))
    process.StartInfo.CreateNoWindow <- true
    process.StartInfo.RedirectStandardOutput <- true
    process.StartInfo.RedirectStandardError <- true
    process.StartInfo.UseShellExecute <- false
    process.StartInfo.WorkingDirectory <- workingDirectory
    process.Start()
    process.WaitForExit()
    if process.ExitCode <> 0 then
        raise (new InvalidOperationException(process.StandardError.ReadToEnd()))
    process.StandardOutput.ReadToEnd()

let compile (filename: string): string =
    run (Path.GetDirectoryName(filename)) ClExePath [| "/nologo"; "/c"; "/Os"; filename |] |> ignore
    Path.ChangeExtension(filename, "obj")

let disassemble (filename: string): string =
    let lines =
        (run (Path.GetDirectoryName(filename)) DumpBinPath [| "/disasm:bytes"; filename |])
            .Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)
            .Select(fun l -> l.Trim())
            .SkipWhile(fun l -> l <> "File Type: COFF OBJECT")
            .Skip(2) // Skip file type and signature
            .TakeWhile(fun l -> l <> "Summary")
    System.String.Join("\n", lines)

let CreateTempDirectory(): TempDirectory.T =
    let tempFileName = Path.GetTempFileName()
    File.Delete(tempFileName)
    new TempDirectory.T(tempFileName)
