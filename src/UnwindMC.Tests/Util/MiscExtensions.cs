using System;
using System.Linq;

namespace UnwindMC.Tests.Helpers
{
    public static class MiscExtensions
    {
        public static string StripIndent(this string str)
        {
            var lines = str.Replace("\r\n", "\n").Split('\n');
            int indent = lines
                .Skip(1)
                .Min(l => l.TakeWhile(c => c == ' ').Count());
            return lines[0] + Environment.NewLine +
                string.Join(Environment.NewLine, lines
                    .Skip(1)
                    .Select(l => l.Substring(indent)));
        }
    }
}
