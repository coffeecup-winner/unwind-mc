using System;

namespace UnwindMC.Util
{
    public static class Option
    {
        public static Option<T> Some<T>(T value) => new Option<T>(value);
    }

    public struct Option<T>
    {
        public static readonly Option<T> None = new Option<T>();

        private readonly T _value;
        private readonly bool _isSome;

        public Option(T value)
        {
            _value = value;
            _isSome = true;
        }

        public bool IsSome => _isSome;
        public bool IsNone => !_isSome;

        public bool TryGet(out T value)
        {
            if (_isSome)
            {
                value = _value;
                return true;
            }
            value = default(T);
            return false;
        }

        public Option<U> Map<U>(Func<T, U> map) =>
            _isSome ? Option.Some(map(_value)) : Option<U>.None;

        public Option<T> OrElse(Func<Option<T>> orElse) =>
            _isSome ? this : orElse();

        public override bool Equals(object obj)
        {
            if (!(obj is Option<T> that))
            {
                return false;
            }
            return (!_isSome && !that._isSome) || (_value.Equals(that._value));
        }

        public override int GetHashCode()
        {
            int hash = 17;
            hash = hash * 37 + _isSome.GetHashCode();
            if (_isSome)
            {
                hash = hash * 37 + _value.GetHashCode();
            }
            return hash;
        }

        public override string ToString()
        {
            return !_isSome ? "None" : "Some(" + _value.ToString() + ")";
        }
    }
}
