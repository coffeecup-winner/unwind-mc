using System;
using System.Collections.Generic;
using UnwindMC.Util.Internal.PatternMatching;

namespace UnwindMC.Util
{
    public interface IPattern
    {
        bool Match(object var);
    }

    public interface IPattern<in T> : IPattern
    {
        bool Match(T var);
    }

    public static class PatternMatching
    {
        private struct Wildcard : IPattern<object>
        {
            public bool Match(object var) => true;
        }

        public static readonly IPattern<object> _ = new Wildcard();

        public static TOut MatchOrDefault<TIn, TOut>(TIn var, params PatternExpression<TOut>[] patterns)
        {
            foreach (var expression in patterns)
            {
                if (expression.TryMatch(var, out var result))
                {
                    return result;
                }
            }
            return default(TOut);
        }

        public static TOut Match<TIn, TOut>(TIn var, params PatternExpression<TOut>[] patterns)
        {
            foreach (var expression in patterns)
            {
                if (expression.TryMatch(var, out var result))
                {
                    return result;
                }
            }
            throw new Exception();
        }

        public static Capture<T> Capture<T>() => new Capture<T>();

        public static Pattern<T> C<T>(T value) => new Pattern<T>(value);

        public static ConditionedPattern<TIn> When<TIn>(this IPattern<TIn> pattern, Func<bool> predicate)
            => new ConditionedPattern<TIn>(pattern, predicate);

        public static PatternExpression<TOut> Then<TIn, TOut>(this IPattern<TIn> pattern, Func<TOut> getResult)
            => new PatternExpression<TOut>(pattern, getResult);

        public static PatternExpression<TOut> Then<TIn, TOut>(this IPattern<TIn> pattern, TOut result)
            => new PatternExpression<TOut>(pattern, () => result);

        public static PatternExpression<TOut> Otherwise<TOut>(Func<TOut> getResult)
            => _.Then(getResult);

        public static PatternExpression<TOut> Otherwise<TOut>(TOut result)
            => _.Then(result);
    }

    public class Capture<T> : IPattern<T>
    {
        private T _value;

        public T Value => _value;

        public bool Match(T var)
        {
            _value = var;
            return true;
        }

        public bool Match(object var) => var is T obj && Match(obj);

        public static implicit operator T(Capture<T> capture) => capture._value;

        public static IPattern<T> operator%(Capture<T> capture, IPattern pattern) => new PatternedCapture<T>(capture, pattern);
    }
}
