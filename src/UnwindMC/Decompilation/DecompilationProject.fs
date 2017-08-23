module DecompilationProject

open System.IO
open YamlDotNet.Serialization

[<Literal>]
let ProjectFileName = "project.yml"

type Config = {
    [<YamlMember(Alias = "exe")>]
    exePath: string
    [<YamlMember(Alias = "output")>]
    outputPath: string
}

type T = {
    rootPath: string
    config: Config
} with
    member self.exePath: string = Path.Combine(self.rootPath, self.config.exePath)
    member self.outputPath: string = Path.Combine(self.rootPath, self.config.outputPath)

let load (projectRootPath: string): T =
    let deserializer = (new DeserializerBuilder()).Build()
    {
        rootPath = projectRootPath
        config = deserializer.Deserialize<Config>(File.ReadAllText(Path.Combine(projectRootPath, ProjectFileName)))
    }
