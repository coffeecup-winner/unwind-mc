using Moq;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using UnwindMC.Analysis;

namespace UnwindMC.Tests
{
    internal static class AnalysisHelper
    {
        public static Analyzer Analyze(string function)
        {
            var lines = function
                .Split(new[] { '\r', '\n' })
                .Where(l => !string.IsNullOrWhiteSpace(l))
                .Select(l => l.TrimStart())
                .ToList();

            var parts = lines
                .Select(l =>
                {
                    var data = l.Split(new[] {' '}, StringSplitOptions.RemoveEmptyEntries).ToArray();
                    data[0] = data[0].TrimEnd(':');
                    return data;
                })
                .ToArray();

            var address = ulong.Parse(parts[0][0], NumberStyles.HexNumber);
            var bytes = new List<byte>();
            var canonLines = new List<string>();
            foreach (var data in parts)
            {
                var hex = new StringBuilder();
                int i = 1;
                for (; i < data.Length; i++)
                {
                    string @byte = data[i];
                    if (!byte.TryParse(@byte, NumberStyles.HexNumber, CultureInfo.InvariantCulture.NumberFormat, out var b))
                    {
                        break;
                    }
                    bytes.Add(b);
                    hex.Append(@byte);
                }
                var line = $"{data[0]} {hex,20}".ToLower();
                canonLines.Add(line);
            }

            var analyzer = new Analyzer(new ArraySegment<byte>(bytes.ToArray()), address, Mock.Of<IImportResolver>());
            analyzer.AddFunction(address);
            analyzer.Analyze();

            Assert.That(analyzer.Graph.Instructions.Select(i => $"{i.Offset:x8} {i.Hex,20}"), Is.EqualTo(canonLines));

            return analyzer;
        }
    }
}
