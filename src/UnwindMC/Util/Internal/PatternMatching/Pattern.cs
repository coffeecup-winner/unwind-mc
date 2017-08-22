using System.Collections.Generic;

namespace UnwindMC.Util.Internal.PatternMatching
{
    public struct Pattern<T> : IPattern<T>
    {
        private readonly T _value;

        public Pattern(T value)
        {
            _value = value;
        }

        public bool Match(T var)
        {
            return EqualityComparer<T>.Default.Equals(_value, var);
        }

        public bool Match(object var)
        {
            if (!(var is T obj))
            {
                return false;
            }
            return EqualityComparer<T>.Default.Equals(_value, obj);
        }
    }
}
