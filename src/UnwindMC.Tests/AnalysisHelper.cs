﻿using Moq;
using NUnit.Framework;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using UnwindMC.Analysis;

namespace UnwindMC.Tests
{
    internal static class AnalysisHelper
    {
        private static readonly Regex NormalLineRegex = new Regex(
            @"^(?<address>[0-9A-F]{8}): (((?<bytes>[0-9A-F]{2})|  ) ){6} (?<asm>.+)$", RegexOptions.Compiled | RegexOptions.IgnoreCase);
        private static readonly Regex AdditionalBytesLineRegex = new Regex(
            @"^(?<bytes>[0-9A-F]{2})( (?<bytes>[0-9A-F]{2}))*$", RegexOptions.Compiled | RegexOptions.IgnoreCase);

        public static Analyzer Analyze(string function)
        {
            var lines = function
                .Split('\r', '\n')
                .SkipWhile(string.IsNullOrWhiteSpace)
                .Select(l => l.Trim())
                .ToArray();

            var match = NormalLineRegex.Match(lines[0]);
            var address = ulong.Parse(match.Groups["address"].Value, NumberStyles.HexNumber);
            var bytes = new List<byte>();
            var canonLines = new List<string>();
            for (var i = 0; i < lines.Length; i++)
            {
                string addressText = match.Groups["address"].Value;
                var hex = new StringBuilder();
                foreach (var capture in match.Groups["bytes"].Captures)
                {
                    byte b = byte.Parse(capture.ToString(), NumberStyles.HexNumber);
                    bytes.Add(b);
                    hex.Append($"{b:x2}");
                }
                if (i != lines.Length - 1)
                {
                    var next = lines[i + 1];
                    match = NormalLineRegex.Match(next);
                    if (!match.Success)
                    {
                        match = AdditionalBytesLineRegex.Match(next);
                        if (!match.Success)
                        {
                            throw new InvalidOperationException("Line in incorrect format: " + next);
                        }
                        foreach (var capture in match.Groups["bytes"].Captures)
                        {
                            byte b = byte.Parse(capture.ToString(), NumberStyles.HexNumber);
                            bytes.Add(b);
                            hex.Append($"{b:x2}");
                        }
                        if (i != lines.Length - 2)
                        {
                            ++i;
                            next = lines[i + 1];
                            match = NormalLineRegex.Match(next);
                            if (!match.Success)
                            {
                                throw new InvalidOperationException("Line in incorrect format: " + next);
                            }
                        }
                    }
                }
                canonLines.Add($"{addressText} {hex,20}".ToLower());
            }

            var analyzer = new Analyzer(new ArraySegment<byte>(bytes.ToArray()), address, Mock.Of<IImportResolver>());
            analyzer.AddFunction(address);
            analyzer.Analyze();

            Assert.That(analyzer.Graph.Instructions.Select(i => $"{i.Offset:x8} {i.Hex,20}"), Is.EqualTo(canonLines));

            return analyzer;
        }
    }
}
