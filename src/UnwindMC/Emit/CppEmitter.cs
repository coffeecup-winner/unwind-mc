using System;
using System.Collections.Generic;
using System.Text;
using UnwindMC.Analysis.Ast;
using UnwindMC.Util;

namespace UnwindMC.Emit
{
    public class CppEmitter
    {
        private class ReturnNodeFinder : NodeVisitorBase
        {
            private ReturnNode _ret;

            public ReturnNode Return => _ret;

            public override void Visit(ReturnNode ret)
            {
                _ret = ret;
            }
        }

        private const int IndentSize = 2;

        private readonly string _name;
        IReadOnlyDictionary<string, Analysis.Data.Type> _types;
        private readonly int _parametersCount;
        private readonly ScopeNode _body;
        private readonly Dictionary<int, string> _indents;

        private int _indentLevel;
        private string _indent;
        private HashSet<string> _declaredVariables;

        public CppEmitter(string name, IReadOnlyDictionary<string, Analysis.Data.Type> types, int parametersCount, ScopeNode body)
        {
            _name = name;
            _types = types;
            _parametersCount = parametersCount;
            _body = body;
            _indents = new Dictionary<int, string> { { 0, "" } };
        }

        public string EmitSourceCode()
        {
            _indentLevel = 0;
            _indent = _indents[0];
            _declaredVariables = new HashSet<string>();
            var sb = new StringBuilder();
            EmitSignature(sb, _body.Visit(new ReturnNodeFinder()).Return);
            Emit(sb, _body);
            return sb.ToString();
        }

        private void EmitSignature(StringBuilder sb, ReturnNode ret)
        {
            EmitType(sb, ret.Var);
            sb.Append(_name)
                .Append("(");
            for (int i = 0; i < _parametersCount; i++)
            {
                if (i != 0)
                {
                    sb.Append(", ");
                }
                EmitDeclaration(sb, "arg" + i); // TODO: move argument names to function
            }
            sb.Append(")")
                .Append(Environment.NewLine);
        }

        private void EmitType(StringBuilder sb, Option<VarNode> var)
        {
            if (!var.HasValue)
            {
                sb.Append("void ");
                return;
            }
            var type = _types[var.Value.Name];
            if (type.IsFunction)
            {
                throw new NotImplementedException();
            }
            else
            {
                sb.Append("uint32_t ")
                    .Append(new string('*', type.IndirectionLevel));
            }
        }

        private void EmitDeclaration(StringBuilder sb, string name)
        {
            var type = _types[name];
            if (type.IsFunction)
            {
                sb.Append("void")
                    .Append(" ")
                    .Append("(")
                    .Append(new string('*', type.IndirectionLevel + 1))
                    .Append(name)
                    .Append(")")
                    .Append("()");
            }
            else
            {
                sb.Append("uint32_t ")
                    .Append(new string('*', type.IndirectionLevel))
                    .Append(name);
            }
        }

        private void Emit(StringBuilder sb, IStatementNode statement)
        {
            switch (statement)
            {
                case AssignmentNode assignment:
                    Emit(sb, assignment);
                    return;
                case DoWhileNode doWhile:
                    Emit(sb, doWhile);
                    return;
                case FunctionCallNode call:
                    Emit(sb, call);
                    return;
                case IfThenElseNode ifThenElse:
                    Emit(sb, ifThenElse);
                    return;
                case ReturnNode ret:
                    Emit(sb, ret);
                    return;
                case ScopeNode scope:
                    Emit(sb, scope);
                    return;
                case WhileNode whileLoop:
                    Emit(sb, whileLoop);
                    return;
                default: throw new NotSupportedException();
            }
        }

        private void Emit(StringBuilder sb, AssignmentNode assignment)
        {
            sb.Append(_indent);
            if (_declaredVariables.Add(assignment.Var.Name))
            {
                EmitDeclaration(sb, assignment.Var.Name);
            }
            else
            {
                sb.Append(assignment.Var.Name);
            }
            sb.Append(" = ");
            Emit(sb, assignment.Expression);
            sb.Append(";")
                .Append(Environment.NewLine);
        }

        private void Emit(StringBuilder sb, DoWhileNode whileLoop)
        {
            sb.Append(_indent)
                .Append("do")
                .Append(Environment.NewLine);
            Emit(sb, whileLoop.Body, skipNewline: true);
            sb.Append(" ")
                .Append("while")
                .Append(" ")
                .Append("(");
            Emit(sb, whileLoop.Condition);
            sb.Append(")")
                .Append(";")
                .Append(Environment.NewLine);
        }

        private void Emit(StringBuilder sb, FunctionCallNode call)
        {
            sb.Append(_indent);
            Emit(sb, call.Function);
            sb.Append("()")
                .Append(";")
                .Append(Environment.NewLine);
        }

        private void Emit(StringBuilder sb, IfThenElseNode ifThenElse)
        {
            sb.Append(_indent)
                .Append("if")
                .Append(" ")
                .Append("(");
            Emit(sb, ifThenElse.Condition);
            sb.Append(")")
                .Append(Environment.NewLine);
            Emit(sb, ifThenElse.TrueBranch);
            if (ifThenElse.FalseBranch.ChildrenCount > 0)
            {
                sb.Append(_indent)
                    .Append("else")
                    .Append(Environment.NewLine);
                Emit(sb, ifThenElse.FalseBranch);
            }
        }

        private void Emit(StringBuilder sb, ReturnNode ret)
        {
            sb.Append(_indent)
                .Append("return");
            if (ret.Var.HasValue)
            {
                sb.Append(" ")
                    .Append(ret.Var.Value.Name);
            }
            sb.Append(";")
                .Append(Environment.NewLine);
        }

        private void Emit(StringBuilder sb, ScopeNode scope, bool skipNewline = false)
        {
            sb.Append(_indent)
                .Append("{")
                .Append(Environment.NewLine);
            _indentLevel++;
            if (!_indents.TryGetValue(_indentLevel, out _indent))
            {
                _indent = new string(' ', _indentLevel * IndentSize);
                _indents[_indentLevel] = _indent;
            }

            foreach (var node in scope)
            {
                Emit(sb, node);
            }

            _indentLevel--;
            _indent = _indents[_indentLevel];
            sb.Append(_indent)
                .Append("}");
            if (!skipNewline)
            {
                sb.Append(Environment.NewLine);
            }
        }

        private void Emit(StringBuilder sb, WhileNode whileLoop)
        {
            sb.Append(_indent)
                .Append("while")
                .Append(" ")
                .Append("(");
            Emit(sb, whileLoop.Condition);
            sb.Append(")")
                .Append(Environment.NewLine);
            Emit(sb, whileLoop.Body);
        }

        private void Emit(StringBuilder sb, IExpressionNode expression)
        {
            switch (expression)
            {
                case BinaryOperatorNode binary:
                    Emit(sb, binary);
                    return;
                case DereferenceNode dereference:
                    Emit(sb, dereference);
                    return;
                case ValueNode value:
                    Emit(sb, value);
                    return;
                case VarNode var:
                    Emit(sb, var);
                    return;
                default: throw new NotSupportedException();
            }
        }

        private void Emit(StringBuilder sb, BinaryOperatorNode binary)
        {
            Emit(sb, binary.Left);
            sb.Append(" ");
            string op;
            switch (binary.Operator)
            {
                case Operator.Equal: op = "=="; break;
                case Operator.NotEqual: op = "!="; break;
                case Operator.Less: op = "<"; break;
                case Operator.LessOrEqual: op = "<="; break;
                case Operator.Greater: op = ">"; break;
                case Operator.GreaterOrEqual: op = ">="; break;
                case Operator.Or: op = "||"; break;
                case Operator.And: op = "&&"; break;
                case Operator.Add: op = "+"; break;
                case Operator.Subtract: op = "-"; break;
                case Operator.Multiply: op = "*"; break;
                case Operator.Divide: op = "/"; break;
                case Operator.Modulo: op = "%"; break;
                default: throw new NotSupportedException();
            }
            sb.Append(op)
                .Append(" ");
            Emit(sb, binary.Right);
        }

        private void Emit(StringBuilder sb, DereferenceNode dereference)
        {
            sb.Append("*")
                .Append("(");
            Emit(sb, dereference.Pointer);
            sb.Append(")");
        }

        private void Emit(StringBuilder sb, ValueNode value)
        {
            sb.Append(value.Value);
        }

        private void Emit(StringBuilder sb, VarNode var)
        {
            sb.Append(var.Name);
        }
    }
}
