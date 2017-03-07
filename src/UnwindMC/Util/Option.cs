﻿using System;

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
        private readonly bool _hasValue;

        public Option(T value)
        {
            _value = value;
            _hasValue = true;
        }

        public bool HasValue => _hasValue;
        public T Value => _hasValue ? _value : throw new InvalidOperationException("Trying to get value from None");

        public Option<U> Map<U>(Func<T, U> map) =>
            _hasValue ? Option.Some(map(_value)) : Option<U>.None;

        public Option<T> OrElse(Func<Option<T>> orElse) =>
            _hasValue ? this : orElse();

        public override bool Equals(object obj)
        {
            if (!(obj is Option<T> that))
            {
                return false;
            }
            return (!_hasValue && !that._hasValue) || (_value.Equals(that._value));
        }

        public override int GetHashCode()
        {
            int hash = 17;
            hash = hash * 37 + _hasValue.GetHashCode();
            if (_hasValue)
            {
                hash = hash * 37 + _value.GetHashCode();
            }
            return hash;
        }

        public override string ToString()
        {
            return !_hasValue ? "None" : "Some(" + _value.ToString() + ")";
        }
    }
}
