using System.Collections.Generic;

namespace UnwindMC.Util
{
    public static class LinqExtensions
    {
        public static ISet<T> ToSet<T>(this IEnumerable<T> collection)
        {
            return new HashSet<T>(collection);
        }
    }
}
