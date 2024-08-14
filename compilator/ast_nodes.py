class Node:
    def _str(self, level=0):
        raise NotImplementedError("This method should be overridden by subclasses")

class Program(Node):
    def __init__(self, expressions):
        self.expressions = expressions

    def _str(self, level=0):
        result = "  " * level + "Program:\n"
        for expr in self.expressions:
            if isinstance(expr, Node):  # Проверка, является ли элемент экземпляром Node
                result += expr._str(level + 1)
            else:
                result += "  " * (level + 1) + str(expr) + "\n"
        return result


class Symbol(Node):
    def __init__(self, value):
        self.value = value

    def _str(self, level=0):
        return "  " * level + f"Symbol({self.value})\n"

class IntVal(Node):
    def __init__(self, value):
        self.value = value

    def _str(self, level=0):
        return "  " * level + f"IntVal({self.value})\n"

class Bool(Node):
    def __init__(self, value):
        self.value = value

    def _str(self, level=0):
        return "  " * level + f"Bool({self.value})\n"

class String(Node):
    def __init__(self, value):
        self.value = value

    def _str(self, level=0):
        return "  " * level + f"String({self.value})\n"

class Let(Node):
    def __init__(self, bindings, body):
        self.bindings = bindings
        self.body = body

    def _str(self, level=0):
        result = "  " * level + "Let:\n"
        result += "  " * (level + 1) + "Bindings:\n"
        for var, val in self.bindings:
            var_str = var._str(0) if hasattr(var, '_str') else str(var)
            val_str = val._str(0) if hasattr(val, '_str') else str(val)
            result += "  " * (level + 2) + f"{var_str} = {val_str}\n"
        result += "  " * (level + 1) + "Body:\n"
        if isinstance(self.body, list):
            for item in self.body:
                result += item._str(level + 2) if hasattr(item, '_str') else str(item) + "\n"
        else:
            result += self.body._str(level + 2) if hasattr(self.body, '_str') else str(self.body)
        return result

class Function(Node):
    def __init__(self, name, params, body):
        self.name = name
        self.params = params
        self.body = body

    def _str(self, level=0):
        indent = "  " * level
        result = f"{indent}Function({self.name}):\n"
        result += "  " * (level + 1) + "Params:\n"
        if isinstance(self.params, list):
            for param in self.params:
                if isinstance(param, Node):
                    result += param._str(level + 2)
                else:
                    result += "  " * (level + 2) + str(param) + "\n"
        result += "  " * (level + 1) + "Body:\n"
        if isinstance(self.body, list):
            for item in self.body:
                if isinstance(item, Node):
                    result += item._str(level + 2)
                else:
                    result += "  " * (level + 2) + str(item) + "\n"
        else:
            result += self.body._str(level + 2) if isinstance(self.body, Node) else "  " * (level + 2) + str(self.body) + "\n"
        return result


class FunctionCall(Node):
    def __init__(self, function_name, arguments):
        self.function_name = function_name  # Ожидается строка
        self.arguments = arguments  # Список аргументов

    def _str(self, level=0):
        indent = "  " * level
        result = f"{indent}FunctionCall({self.function_name}):\n"
        if isinstance(self.arguments, list):
            for arg in self.arguments:
                result += arg._str(level + 1) if hasattr(arg, '_str') else indent + str(arg) + "\n"
        return result


class If(Node):
    def __init__(self, condition, then_branch, else_branch=None):
        self.condition = condition
        self.then_branch = then_branch
        self.else_branch = else_branch

    def _str(self, level=0):
        result = "  " * level + "If:\n"
        result += "  " * (level + 1) + "Condition:\n"
        result += self.condition._str(level + 2)
        result += "  " * (level + 1) + "Then:\n"
        result += self.then_branch._str(level + 2)
        if self.else_branch:
            result += "  " * (level + 1) + "Else:\n"
            result += self.else_branch._str(level + 2)
        return result

class Print(Node):
    def __init__(self, value):
        self.value = value

    def _str(self, level=0):
        return "  " * level + "Print:\n" + self.value._str(level + 1)

class Input(Node):
    def _str(self, level=0):
        return "  " * level + "Input()\n"

class SetQ(Node):
    def __init__(self, variable, value):
        self.variable = variable  # Ожидается строка
        self.value = value        # Ожидается объект узла

    def _str(self, level=0):
        indent = "  " * level
        if isinstance(self.value, Node):
            value_str = self.value._str(level + 1)
        else:
            value_str = str(self.value)
        return f"{indent}SetQ:\n{indent}  {self.variable} = {value_str}\n"


class BinaryOperator(Node):
    def __init__(self, operator, left, right):
        self.operator = operator
        self.left = left
        self.right = right

    def _str(self, level=0):
        result = "  " * level + f"BinaryOperator({self.operator}):\n"
        result += self.left._str(level + 1)
        result += self.right._str(level + 1)
        return result

class UnaryOperator(Node):
    def __init__(self, operator, operand):
        self.operator = operator
        self.operand = operand

    def _str(self, level=0):
        result = "  " * level + f"UnaryOperator({self.operator}):\n"
        result += self.operand._str(level + 1)
        return result

class For(Node):
    def __init__(self, loop_var, start_expr, end_expr, body):
        self.loop_var = loop_var
        self.start_expr = start_expr
        self.end_expr = end_expr
        self.body = body  # Это список выражений

    def _str(self, level=0):
        indent = "  " * level
        # Преобразуем выражения в строки
        start_str = self.start_expr._str(0) if hasattr(self.start_expr, '_str') else str(self.start_expr)
        end_str = self.end_expr._str(0) if hasattr(self.end_expr, '_str') else str(self.end_expr)
        result = f"{indent}For loop with variable {self.loop_var} from {start_str} to {end_str}\n"

        # Преобразуем каждое выражение в теле цикла в строку
        for expr in self.body:
            result += (expr._str(level + 1) if hasattr(expr, '_str') else f"{indent}  {str(expr)}\n")

        return result
