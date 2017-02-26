using System;

namespace UnwindMC.Util
{
    public static class Either
    {
        public static Either<TLeft, TRight> Left<TLeft, TRight>(TLeft left)
        {
            return new Either<TLeft, TRight>(left);
        }

        public static Either<TLeft, TRight> Right<TLeft, TRight>(TRight right)
        {
            return new Either<TLeft, TRight>(right);
        }
    }

    public struct Either<TLeft, TRight>
    {
        private readonly bool _isLeft;
        private readonly TLeft _left;
        private readonly TRight _right;

        public Either(TLeft left)
        {
            _isLeft = true;
            _right = default(TRight);
            _left = left;
        }

        public Either(TRight right)
        {
            _isLeft = false;
            _left = default(TLeft);
            _right = right;
        }

        public bool IsLeft => _isLeft;
        public bool IsRight => !_isLeft;
        public TLeft Left
        {
            get
            {
                if (!_isLeft) throw new InvalidOperationException();
                return _left;
            }
        }
        public TRight Right
        {
            get
            {
                if (_isLeft) throw new InvalidOperationException();
                return _right;
            }
        }
    }
}
