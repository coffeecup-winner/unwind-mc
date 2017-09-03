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
    use proc = new Process()
    proc.StartInfo <- new ProcessStartInfo(exePath, System.String.Join(" ", arguments))
    proc.StartInfo.CreateNoWindow <- true
    proc.StartInfo.RedirectStandardOutput <- true
    proc.StartInfo.RedirectStandardError <- true
    proc.StartInfo.UseShellExecute <- false
    proc.StartInfo.WorkingDirectory <- workingDirectory
    proc.Start() |> ignore
    proc.WaitForExit()
    if proc.ExitCode <> 0 then
        failwith (proc.StandardError.ReadToEnd())
    proc.StandardOutput.ReadToEnd()

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
