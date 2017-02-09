using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using UnwindMC.Analysis.Ast;
using UnwindMC.Analysis.IL;

namespace UnwindMC.Emit
{
    public class CppEmitter
    {
        private const int IndentSize = 2;

        private readonly string _name;
        private readonly IReadOnlyDictionary<ILOperand, Analysis.Data.Type> _parameters;
        private readonly ScopeNode _body;
        private readonly Dictionary<int, string> _indents;

        private int _indentLevel;
        private string _indent;
        private HashSet<string> _declaredVariables;

        public CppEmitter(string name, IReadOnlyDictionary<ILOperand, Analysis.Data.Type> parameters, ScopeNode body)
        {
            _name = name;
            _parameters = parameters;
            _body = body;
            _indents = new Dictionary<int, string> { { 0, "" } };
        }

        public string EmitSourceCode()
        {
            _indentLevel = 0;
            _indent = _indents[0];
            _declaredVariables = new HashSet<string>();
            var sb = new StringBuilder();
            EmitSignature(sb);
            Emit(sb, _body);
            return sb.ToString();
        }

        private void EmitSignature(StringBuilder sb)
        {
            sb.Append("void")
                .Append(" ")
                .Append(_name)
                .Append("(");
            int index = 0;
            foreach (var pair in _parameters.OrderBy(p => p.Key.Offset))
            {
                if (index != 0)
                {
                    sb.Append(", ");
                }
                EmitParameter(sb, pair.Value, "arg" + index++); // TODO: move argument names to function
            }
            sb.Append(")")
                .Append(Environment.NewLine);
        }

        private static void EmitParameter(StringBuilder sb, Analysis.Data.Type value, string name)
        {
            if (value.IsFunction)
            {
                sb.Append("void")
                    .Append(" ")
                    .Append("(")
                    .Append(new string('*', value.IndirectionLevel + 1))
                    .Append(name)
                    .Append(")")
                    .Append("()");
            }
            else
            {
                sb.Append("uint32_t ")
                    .Append(new string('*', value.IndirectionLevel))
                    .Append(name);
            }
        }

        private void Emit(StringBuilder sb, IStatementNode statement)
        {
            var assignment = statement as AssignmentNode;
            if (assignment != null)
            {
                Emit(sb, assignment);
                return;
            }
            var call = statement as FunctionCallNode;
            if (call != null)
            {
                Emit(sb, call);
                return;
            }
            var ifThenElse = statement as IfThenElseNode;
            if (ifThenElse != null)
            {
                Emit(sb, ifThenElse);
                return;
            }
            var ret = statement as ReturnNode;
            if (ret != null)
            {
                Emit(sb, ret);
                return;
            }
            var scope = statement as ScopeNode;
            if (scope != null)
            {
                Emit(sb, scope);
                return;
            }
            var whileLoop = statement as WhileNode;
            if (whileLoop != null)
            {
                Emit(sb, whileLoop);
                return;
            }
            throw new NotSupportedException();
        }

        private void Emit(StringBuilder sb, AssignmentNode assignment)
        {
            sb.Append(_indent);
            if (_declaredVariables.Add(assignment.Var.Name))
            {
                sb.Append("auto")
                    .Append(" ");
            }
            sb.Append(assignment.Var.Name)
                .Append(" = ");
            Emit(sb, assignment.Expression);
            sb.Append(";")
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
                .Append("return")
                .Append(";")
                .Append(Environment.NewLine);
        }

        private void Emit(StringBuilder sb, ScopeNode scope)
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
                .Append("}")
                .Append(Environment.NewLine);
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
            var binary = expression as BinaryOperatorNode;
            if (binary != null)
            {
                Emit(sb, binary);
                return;
            }
            var dereference = expression as DereferenceNode;
            if (dereference != null)
            {
                Emit(sb, dereference);
                return;
            }
            var value = expression as ValueNode;
            if (value != null)
            {
                Emit(sb, value);
                return;
            }
            var var = expression as VarNode;
            if (var != null)
            {
                Emit(sb, var);
                return;
            }
            throw new NotSupportedException();
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
