#!/usr/bin/env python3
import pytest
import sys
from io import StringIO
from model import *


def test_construction():
    FunctionDefinition('fac', Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ]))
    Read('n')
    Print(
        UnaryOperation('-', FunctionCall(Reference('fac'), [Reference('n')]))
    )


def test_simple_scope():
    scope = Scope()
    scope['name2'] = 2
    scope['name1'] = 1
    assert scope['name2'] == 2
    assert scope['name1'] == 1


def test_parent_scope():
    grandparent = Scope()
    grandparent['name'] = 0
    parent = Scope(grandparent)
    parent['name1'] = 1
    scope = Scope(parent)
    scope['name2'] = 2
    assert scope['name2'] == 2
    assert scope['name1'] == 1
    assert scope['name'] == 0


def test_scope_key_error():
    scope = Scope()
    scope['name1'] = 1
    with pytest.raises(KeyError):
        scope['name'] == 1


def test_function_definition():
    func = Function(['n'], [])
    func_def = FunctionDefinition('Hello', func)
    scope = Scope()
    assert func_def.evaluate(scope) == func


def test_conditional():
    cond1 = Conditional(Number(0), [Number(1)], [Number(2), Number(3)])
    cond2 = Conditional(Number(5), [Number(1)], [Number(2), Number(3)])
    scope = Scope()
    assert cond1.evaluate(scope) == Number(3)
    assert cond2.evaluate(scope) == Number(1)


def test_print(capsys):
    print_obj = Print(Number(227))
    scope = Scope()
    assert print_obj.evaluate(scope) == Number(227)
    assert capsys.readouterr().out == '227\n'


def test_read_number(monkeypatch):
    monkeypatch.setattr(sys, 'stdin', StringIO('12'))
    read_obj = Read('number')
    scope = Scope()
    assert read_obj.evaluate(scope) == Number(12)


def test_function_call():
    func_call = FunctionCall(Function(['a'], [Reference('a')]),
                             [Number(5)])
    s = Scope()
    assert func_call.evaluate(s) == Number(5)


def test_reference():
    a, b, c = Number(1), Number(2), Number(3)
    parent = Scope()
    parent['name1'] = a
    parent['name3'] = b
    scope = Scope(parent)
    scope['name1'] = c
    scope['name2'] = b
    assert Reference('name3').evaluate(scope) == Number(2)
    assert Reference('name1').evaluate(scope) == Number(3)


def test_unary_operation():
    scope = Scope()
    assert UnaryOperation('!', Number(1)).evaluate(scope) == Number(0)
    assert UnaryOperation('-', Number(1)).evaluate(scope) == Number(-1)


def test_binary_operation():
    scope = Scope()
    assert BinaryOperation(Number(5), '+', Number(1)).evaluate(scope) == (
        Number(6))
    assert BinaryOperation(Number(3), '<=', Number(5)).evaluate(scope) == (
        Number(1))
    assert BinaryOperation(Number(5), '/', Number(2)).evaluate(scope) == (
        Number(2))


def test_end_to_end():
    yat_fac = FunctionDefinition('fac', Function(['n'], [
        Conditional(
            BinaryOperation(Reference('n'), '==', Number(0)),
            [Number(1)],
            [
                BinaryOperation(
                    Reference('n'),
                    '*',
                    FunctionCall(Reference('fac'), [
                        BinaryOperation(
                            Reference('n'),
                            '-',
                            Number(1)
                        )
                    ])
                )
            ]
        )
    ]))
    scope = Scope()
    yat_fac.evaluate(scope)
    assert Print(FunctionCall(Reference('fac'),
                 [Number(10)])).evaluate(scope) == Number(3628800)


if __name__ == "__main__":
    pytest.main()
