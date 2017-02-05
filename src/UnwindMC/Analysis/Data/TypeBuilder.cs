using System;

namespace UnwindMC.Analysis.Data
{
    public class TypeBuilder
    {
        private bool _isFunction;
        private int _indirectionLevel;

        public void AddFunctionTrait()
        {
            _isFunction = true;
        }

        public void AddIndirectionLevel(int level = 1)
        {
            _indirectionLevel += level;
        }

        public Type Build()
        {
            return new Type(_isFunction, _indirectionLevel);
        }
    }
}
