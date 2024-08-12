from ast_nodes import *

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def parse(self):
        expressions = []
        while self.pos < len(self.tokens):
            expressions.append(self.parse_expression())
        return Program(expressions)

    def parse_expression(self):
        token = self.tokens[self.pos]

        if token.type == 'LPAREN':
            self.pos += 1  # consume '('
            token = self.tokens[self.pos]

            if token.type == 'LET':
                return self.parse_let()
            elif token.type == 'SYMBOL' and token.value == 'function':
                return self.parse_function()
            elif token.type == 'IF':
                return self.parse_if()
            elif token.type == 'PRINT':
                return self.parse_print()
            elif token.type == 'SETQ':
                return self.parse_setq()
            elif token.type == 'INPUT':
                return self.parse_input()
            elif token.type == 'PARSE_INT':
                return self.parse_parse_int()
            elif token.type in ['LT', 'GT', 'EQ']:
                return self.parse_binary_operator()
            elif token.type in ['PLUS', 'MINUS', 'MOD', 'DIVIDE']:
                return self.parse_binary_operator()
            elif token.type == 'FOR':
                return self.parse_for()
            else:
                expressions = []
                while self.tokens[self.pos].type != 'RPAREN':
                    expressions.append(self.parse_expression())

                if self.tokens[self.pos].type == 'RPAREN':
                    self.pos += 1  # consume ')'
                else:
                    raise RuntimeError(f'Expected RPAREN but got {self.tokens[self.pos]}')

                return expressions

        elif token.type == 'SYMBOL':
            self.pos += 1
            return Symbol(token.value)

        elif token.type == 'INTVAL':
            self.pos += 1
            return IntVal(int(token.value))

        elif token.type == 'BOOL':
            self.pos += 1
            return Bool(token.value == 'true')

        elif token.type == 'STRING':
            self.pos += 1
            return String(token.value)

        else:
            raise RuntimeError(f'Unexpected token: {token}')


    def parse_function_call(self):
        function_name = self.tokens[self.pos].value
        self.pos += 1  # consume the function name

        arguments = []
        while self.tokens[self.pos].type != 'RPAREN':
            arguments.append(self.parse_expression())  # Parse each argument

        self.expect_token('RPAREN')  # consume ')'

        return FunctionCall(function_name, arguments)
    
    def parse_for(self):
        self.pos += 1  # consume 'for'

        # Ожидаем переменную цикла
        loop_var = self.expect_token('SYMBOL').value

        # Ожидаем 'from'
        from_token = self.expect_token('SYMBOL')
        if from_token.value != 'from':
            raise RuntimeError(f"Expected 'from' but got {from_token.value}")

        # Ожидаем начальное значение, которое может быть переменной или числом
        start_expr = self.parse_expression()

        # Ожидаем 'to'
        to_token = self.expect_token('SYMBOL')
        if to_token.value != 'to':
            raise RuntimeError(f"Expected 'to' but got {to_token.value}")

        # Ожидаем конечное значение, которое может быть переменной или числом
        end_expr = self.parse_expression()

        # Ожидаем 'do'
        do_token = self.expect_token('SYMBOL')
        if do_token.value != 'do':
            raise RuntimeError(f"Expected 'do' but got {do_token.value}")

        # Парсим тело цикла
        body = []
        while self.tokens[self.pos].type != 'RPAREN':
            body.append(self.parse_expression())

        self.expect_token('RPAREN')  # Закрываем цикл

        return For(loop_var, start_expr, end_expr, body)



    def parse_parse_int(self):
        self.pos += 1  # consume 'parse-int'
        argument = self.parse_expression()  # Парсим аргумент для parse-int
        self.expect_token('RPAREN')  # consume ')'
        return UnaryOperator('parse-int', argument)

    def parse_let(self):
        self.pos += 1  # consume 'let'
        self.expect_token('LPAREN')  # consume '('

        bindings = []
        while self.tokens[self.pos].type == 'LPAREN':
            self.pos += 1  # consume '('
            var_name = self.expect_token('SYMBOL').value
            var_value = self.parse_expression()  # Используйте parse_expression для значений
            bindings.append((var_name, var_value))
            self.expect_token('RPAREN')  # consume ')'

        self.expect_token('RPAREN')  # consume ')'

        body = []
        while self.pos < len(self.tokens) and self.tokens[self.pos].type != 'RPAREN':
            body.append(self.parse_expression())

        if self.pos < len(self.tokens) and self.tokens[self.pos].type == 'RPAREN':
            self.pos += 1  # consume ')'

        return Let(bindings, body)

    def parse_setq(self):
        self.pos += 1  # consume 'setq'

        variable = self.tokens[self.pos].value
        self.pos += 1  # consume variable

        value = self.parse_expression()

        self.expect_token('RPAREN')  # consume ')'
        return SetQ(variable, value)

    def parse_function(self):
        self.pos += 1  # consume 'function'

        name = self.expect_token('SYMBOL').value
        self.expect_token('LPAREN')

        params = []
        while self.tokens[self.pos].type != 'RPAREN':
            params.append(self.expect_token('SYMBOL').value)

        self.expect_token('RPAREN')  # Закрываем параметры

        # Парсим тело функции
        body = []
        while self.tokens[self.pos].type != 'RPAREN':
            body.append(self.parse_expression())

        self.expect_token('RPAREN')  # Закрываем тело функции

        return Function(name, params, body)

    def parse_if(self):
        self.pos += 1  # consume 'if'

        condition = self.parse_expression()  # Parse condition
        then_branch = self.parse_expression()  # Parse then branch
        else_branch = self.parse_expression()  # Parse else branch

        self.expect_token('RPAREN')  # Expect closing parenthesis
        return If(condition, then_branch, else_branch)

    def parse_print(self):
        self.pos += 1  # consume 'print'
        value = self.parse_expression()
        self.expect_token('RPAREN')
        return Print(value)

    def parse_input(self):
        self.pos += 1  # consume 'input'
        self.expect_token('RPAREN')  # consume ')'
        return Input()

    def expect_token(self, type_):
        if self.tokens[self.pos].type != type_:
            raise RuntimeError(f'Expected token {type_} but got {self.tokens[self.pos]}')
        token = self.tokens[self.pos]
        self.pos += 1
        return token

    def parse_binary_operator(self):
        operator = self.tokens[self.pos].value
        self.pos += 1  # consume operator

        left_operand = self.parse_expression()  # Parse the left operand
        right_operand = self.parse_expression()  # Parse the right operand

        self.expect_token('RPAREN')  # Expect the closing parenthesis for the binary expression

        return BinaryOperator(operator, left_operand, right_operand)

    def parse_unary_operator(self):
        operator = self.tokens[self.pos].value
        self.pos += 1  # consume the operator (e.g., '-')

        # Parse the single operand following the unary operator
        operand = self.parse_expression()

        self.expect_token('RPAREN')  # Expect a closing parenthesis

        return UnaryOperator(operator, operand)
