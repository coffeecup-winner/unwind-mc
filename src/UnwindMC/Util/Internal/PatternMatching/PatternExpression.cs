using System;

namespace UnwindMC.Util.Internal.PatternMatching
{
    public struct PatternExpression<TOut>
    {
        private readonly IPattern _pattern;
        private readonly Func<TOut> _getResult;

        public PatternExpression(IPattern pattern, Func<TOut> getResult)
        {
            _pattern = pattern;
            _getResult = getResult;
        }

        public bool TryMatch(object var, out TOut result)
        {
            if (_pattern.Match(var))
            {
                result = _getResult();
                return true;
            }
            result = default(TOut);
            return false;
        }
    }
}
