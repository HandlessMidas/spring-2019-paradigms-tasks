from model import *


class PrettyPrinter(ASTNodeVisitor):
    def __init__(self):
        self.out = ''
        self.indent = 0

    def newline(self):
        self.out += "\n"
        self.out += "    " * self.indent

    def visit_block(self, block):
        if not block:
            self.newline()
            return
        self.indent += 1
        self.newline()
        for i, stmt in enumerate(block):
            if i != 0:
                self.newline()
            stmt.accept(self)
            if not self.out.endswith("}"):
                self.out += ";"
        self.indent -= 1
        self.newline()

    def visit_number(self, number):
        self.out += str(number.value)

    def visit_function(self, function):
        self.out += " {"
        self.visit_block(function.body)
        self.out += "}"

    def visit_function_definition(self, function_definition):
        self.out += "def {}(".format(function_definition.name)
        self.out += ', '.join(function_definition.function.args)
        self.out += ")"
        function_definition.function.accept(self)

    def visit_conditional(self, conditional):
        self.out += 'if ('
        conditional.condition.accept(self)
        self.out += ") {"
        self.visit_block(conditional.if_true)
        self.out += "}"
        if conditional.if_false:
            self.out += " else {"
            self.visit_block(conditional.if_false)
            self.out += '}'

    def visit_print(self, print_):
        self.out += "print "
        print_.expr.accept(self)

    def visit_read(self, read):
        self.out += "read " + read.name

    def visit_function_call(self, function_call):
        function_call.fun_expr.accept(self)
        self.out += "("
        for i, arg in enumerate(function_call.args):
            if i != 0:
                self.out += ", "
            arg.accept(self)
        self.out += ")"

    def visit_reference(self, reference):
        self.out += reference.name

    def visit_binary_operation(self, binary_operation):
        self.out += "("
        binary_operation.lhs.accept(self)
        self.out += " {} ".format(binary_operation.op)
        binary_operation.rhs.accept(self)
        self.out += ")"

    def visit_unary_operation(self, unary_operation):
        self.out += "("
        self.out += "{}".format(unary_operation.op)
        unary_operation.expr.accept(self)
        self.out += ")"


def pretty_print(program):
    printer = PrettyPrinter()
    program.accept(printer)
    if not printer.out.endswith("}"):
        printer.out += ";"
    print(printer.out)
