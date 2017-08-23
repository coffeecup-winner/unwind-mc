using System.IO;
using YamlDotNet.Serialization;

namespace UnwindMC.Decompilation
{
    public class DecompilationProject
    {
        private const string ProjectFileName = "project.yml";

        private class Config
        {
            [YamlMember(Alias = "exe")]
            public string ExePath { get; set; }
            [YamlMember(Alias = "output")]
            public string OutputPath { get; set; }
        }

        private readonly string _projectRootPath;
        private readonly Config _config;

        private DecompilationProject(string projectRootPath)
        {
            _projectRootPath = projectRootPath;
            var deserializer = new DeserializerBuilder().Build();
            _config = deserializer.Deserialize<Config>(File.ReadAllText(Path.Combine(projectRootPath, ProjectFileName)));
        }

        public static DecompilationProject Load(string projectPath)
        {
            return new DecompilationProject(projectPath);
        }

        public string RootPath => _projectRootPath;
        public string ExePath => Path.Combine(RootPath, _config.ExePath);
        public string OutputPath => Path.Combine(RootPath, _config.OutputPath);
    }
}
