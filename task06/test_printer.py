import io
import pytest
import sys

from model import *
from printer import *


def pprint(program):
    printer = PrettyPrinter()
    program.accept(printer)
    return str(printer)


def test_conditional(capsys):
    pretty_print(Conditional(Number(42), [], []))
    out = capsys.readouterr().out
    assert out == "if (42) \n;\n"


def test_function_definition(capsys):
    pretty_print(FunctionDefinition("foo", Function([], [])))
    out = capsys.readouterr().out
    assert out == "def foo() {\n}\n"


def test_print(capsys):
    pretty_print(Print(Number(42)))
    out = capsys.readouterr().out
    assert out == "print 42;\n"


def test_read(capsys):
    pretty_print(Read('x'))
    out = capsys.readouterr().out
    assert out == "read x;\n"


def test_number(capsys):
    pretty_print(Number(10))
    out = capsys.readouterr().out
    assert out == "10;\n"


def test_reference(capsys):
    pretty_print(Reference('x'))
    out = capsys.readouterr().out
    assert out == "x;\n"


def test_binary_operation(capsys):
    add = BinaryOperation(Number(2), '+', Number(3))
    mul = BinaryOperation(Number(0), '*', add)
    pretty_print(mul)
    out = capsys.readouterr().out
    assert out == "(0 * (2 + 3));\n"


def test_unary_operation(capsys):
    pretty_print(UnaryOperation('-', Number(42)))
    out = capsys.readouterr().out
    assert out == "(-42);\n"


def test_function_call(capsys):
    pretty_print(FunctionCall(Reference('foo'),
                 [Number(1), Number(2), Number(3)]))
    out = capsys.readouterr().out
    assert out == "foo(1, 2, 3);\n"


def test_end_to_end(capsys):
    program = FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
                ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                    ])
                ],
            ),
        ]))
    pretty_print(program)
    out = capsys.readouterr().out
    assert out == '''\
def main(arg1) {
    read x;
    print x;
    if ((2 == 3))
        if (1)
        ;
     else
        exit((-arg1));
    ;
}
'''

def test_end_to_end():
    program = FunctionDefinition('main', Function(['arg1'], [
        Read('x'),
        Print(Reference('x')),
        Conditional(
            BinaryOperation(Number(2), '==', Number(3)),
            [
                Conditional(Number(1), [], [])
                ],
            [
                FunctionCall(Reference('exit'), [
                    UnaryOperation('-', Reference('arg1'))
                    ])
                ],
            ),
        ]))
    assert pprint(program) == \
        'def main(arg1) {\n' + \
        '    read x;\n' + \
        '    print x;\n' + \
        '    if ((2 == 3)) \n' + \
        '        if (1) \n' + \
        '        ;\n' + \
        '     else \n' + \
        '        exit(-arg1);\n' + \
        '    ;\n' + \
        '};'


if __name__ == "__main__":
    pytest.main()
