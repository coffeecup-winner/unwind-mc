using System;

namespace UnwindMC.Util.Internal.PatternMatching
{
    public struct ConditionedPattern<TIn>
    {
        private struct Pattern : IPattern<TIn>
        {
            private readonly IPattern<TIn> _pattern;
            private readonly Func<bool> _predicate;

            public Pattern(IPattern<TIn> pattern, Func<bool> predicate)
            {
                _pattern = pattern;
                _predicate = predicate;
            }

            public bool Match(TIn var)
            {
                return _pattern.Match(var) && _predicate();
            }

            public bool Match(object var)
            {
                return _pattern.Match(var) && _predicate();
            }
        }

        private readonly IPattern<TIn> _pattern;

        public ConditionedPattern(IPattern<TIn> pattern, Func<bool> predicate)
        {
            _pattern = new Pattern(pattern, predicate);
        }

        public PatternExpression<TOut> Then<TOut>(Func<TOut> getResult)
        {
            return new PatternExpression<TOut>(_pattern, getResult);
        }
    }
}
