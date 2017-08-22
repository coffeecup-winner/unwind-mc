namespace UnwindMC.Util.Internal.PatternMatching
{
    public struct PatternedCapture<T> : IPattern<T>
    {
        private readonly Capture<T> _capture;
        private readonly IPattern _pattern;

        public PatternedCapture(Capture<T> capture, IPattern pattern)
        {
            _capture = capture;
            _pattern = pattern;
        }

        public bool Match(T var)
        {
            if (_pattern != null && !_pattern.Match(var))
            {
                return false;
            }
            return _capture.Match(var);
        }

        public bool Match(object var)
        {
            if (!(var is T obj))
            {
                return false;
            }
            if (_pattern != null && !_pattern.Match(var))
            {
                return false;
            }
            return _capture.Match(obj);
        }
    }
}
