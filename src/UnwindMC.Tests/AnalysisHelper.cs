using Moq;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
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
                .Select(l => l.Split(new[] { ' ' }, StringSplitOptions.RemoveEmptyEntries).Take(2).ToArray())
                .ToArray();

            var address = ulong.Parse(parts[0][0], NumberStyles.HexNumber);
            var bytes = new List<byte>();
            foreach (string b in parts.Select(p => p[1]))
            {
                string rest = b;
                while (rest.Length >= 2)
                {
                    string @byte = rest.Substring(0, 2);
                    rest = rest.Substring(2);
                    bytes.Add(byte.Parse(@byte, NumberStyles.HexNumber));
                }
            }

            var analyzer = new Analyzer(new ArraySegment<byte>(bytes.ToArray()), address, Mock.Of<IImportResolver>());
            analyzer.AddFunction(address);
            analyzer.Analyze();

            Assert.That(analyzer.Graph.Instructions.Select(i => string.Format("{0:x8} {1,20} {2}", i.Offset, i.Hex, i.Assembly)), Is.EqualTo(lines));

            return analyzer;
        }
    }
}
