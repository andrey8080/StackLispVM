from compilator.ast_nodes import *

class CodeGenerator:
    def __init__(self):
        self.assembler_code = []
        self.symbol_table = {}  # Таблица символов для хранения адресов переменных
        self.label_counter = 0  # Счётчик для создания уникальных меток
        self.data_memory_address = 0x000  # Начальный адрес для данных
        self.stack_pointer = 0xFFF  # Начальный адрес стека (на вершине памяти)

    def generate_label(self):
        """Генерация уникальной метки."""
        label = f"L{self.label_counter}"
        self.label_counter += 1
        return label

    def allocate_memory(self, var_name):
        """Выделение памяти для переменной и возврат её адреса."""
        if var_name not in self.symbol_table:
            self.symbol_table[var_name] = self.data_memory_address
            self.data_memory_address += 1  # Инкремент адреса для следующей переменной
        return self.symbol_table[var_name]

    def generate_code(self, node):
        """Главная функция для генерации кода."""
        if isinstance(node, list):
            # Если узел - это список, рекурсивно обрабатываем каждый элемент списка
            for item in node:
                self.generate_code(item)
        elif isinstance(node, Program):
            for expr in node.expressions:
                self.generate_code(expr)
        elif isinstance(node, Let):
            self.generate_let_code(node)
        elif isinstance(node, SetQ):
            self.generate_setq_code(node)
        elif isinstance(node, If):
            self.generate_if_code(node)
        elif isinstance(node, For):
            self.generate_for_code(node)
        elif isinstance(node, Print):
            self.generate_print_code(node)
        elif isinstance(node, Function):
            self.generate_function_code(node)
        elif isinstance(node, BinaryOperator):
            self.generate_binary_operation_code(node)
        elif isinstance(node, UnaryOperator):
            self.generate_unary_operation_code(node)
        elif isinstance(node, IntVal):
            self.generate_intval_code(node)
        elif isinstance(node, Bool):
            self.generate_bool_code(node)
        elif isinstance(node, String):
            self.generate_string_code(node)
        elif isinstance(node, Symbol):
            self.generate_symbol_code(node)
        elif isinstance(node, Input):
            self.generate_input_code(node)
        else:
            raise ValueError(f"Неизвестный тип узла: {type(node)}")


    def generate_let_code(self, node):
        """Генерация кода для let (инициализация переменных)."""
        for var, val in node.bindings:
            # Генерация кода для значения переменной
            self.generate_code(val)
            # Выделение памяти и сохранение значения переменной
            address = self.allocate_memory(var)
            self.assembler_code.append(f"STORE {address:03X}")
        
        # Генерация кода для тела let-блока
        if isinstance(node.body, list):
            for expr in node.body:
                self.generate_code(expr)
        else:
            self.generate_code(node.body)


    def generate_setq_code(self, node):
        """Генерация кода для setq (присваивание значения переменной)."""
        var_name = node.variable
        value = node.value
        self.generate_code(value)
        address = self.allocate_memory(var_name)
        self.assembler_code.append(f"STORE {address:03X}")

    def generate_if_code(self, node):
        """Генерация кода для условной конструкции if."""
        else_label = self.generate_label()
        end_label = self.generate_label()

        self.generate_code(node.condition)
        self.assembler_code.append(f"JZ {else_label}")

        self.generate_code(node.then_branch)
        self.assembler_code.append(f"JMP {end_label}")

        self.assembler_code.append(f"{else_label}:")
        if node.else_branch:
            self.generate_code(node.else_branch)
        self.assembler_code.append(f"{end_label}:")

    def generate_for_code(self, node):
        """Генерация кода для цикла for."""
        start_label = self.generate_label()
        end_label = self.generate_label()

        # Инициализация переменной цикла
        self.generate_code(node.start_expr)
        var_address = self.allocate_memory(node.loop_var)
        self.assembler_code.append(f"STORE {var_address:03X}")

        self.assembler_code.append(f"{start_label}:")
        # Условие завершения цикла
        self.generate_code(node.end_expr)
        self.assembler_code.append(f"LOAD {var_address:03X}")
        self.assembler_code.append("SUB")
        self.assembler_code.append(f"JZ {end_label}")

        # Тело цикла
        for expr in node.body:
            self.generate_code(expr)

        # Инкремент переменной цикла
        self.assembler_code.append(f"LOAD {var_address:03X}")
        self.assembler_code.append("PUSH 1")
        self.assembler_code.append("ADD")
        self.assembler_code.append(f"STORE {var_address:03X}")

        self.assembler_code.append(f"JMP {start_label}")
        self.assembler_code.append(f"{end_label}:")

    def generate_print_code(self, node):
        """Генерация кода для print."""
        self.generate_code(node.value)
        self.assembler_code.append("OUT")

    def generate_function_code(self, node):
        """Генерация кода для функции."""
        function_label = self.generate_label()
        self.assembler_code.append(f"{function_label}:")
        for expr in node.body:
            self.generate_code(expr)
        self.assembler_code.append("RET")

    def generate_binary_operation_code(self, node):
        """Генерация кода для бинарной операции."""
        self.generate_code(node.left)
        self.generate_code(node.right)
        
        if node.operator == "+":
            self.assembler_code.append("ADD")
        elif node.operator == "-":
            self.assembler_code.append("SUB")
        elif node.operator == "*":
            self.assembler_code.append("MUL")
        elif node.operator == "/":
            self.assembler_code.append("DIV")
        elif node.operator == "AND":
            self.assembler_code.append("AND")
        elif node.operator == "OR":
            self.assembler_code.append("OR")
        elif node.operator == "<":
            self.assembler_code.append("CMP")
            self.assembler_code.append("JLT")
        elif node.operator == "<=":
            self.assembler_code.append("CMP")
            self.assembler_code.append("JLE")
        elif node.operator == ">":
            self.assembler_code.append("CMP")
            self.assembler_code.append("JGT")
        elif node.operator == ">=":
            self.assembler_code.append("CMP")
            self.assembler_code.append("JGE")
        elif node.operator == "==":
            self.assembler_code.append("CMP")
            self.assembler_code.append("JEQ")
        elif node.operator == "!=":
            self.assembler_code.append("CMP")
            self.assembler_code.append("JNE")
        else:
            raise ValueError(f"Неизвестный оператор: {node.operator}")


    def generate_unary_operation_code(self, node):
        """Генерация кода для унарной операции."""
        if node.operator == "parse-int":
            # Генерация кода для унарной операции parse-int
            # Вызываем функцию или процедуру для парсинга строки в число
            self.generate_code(node.operand)
            self.assembler_code.append("CALL PARSE_INT_ROUTINE")
        else:
            raise ValueError(f"Неизвестный унарный оператор: {node.operator}")


    def generate_intval_code(self, node):
        """Генерация кода для целочисленного значения."""
        self.assembler_code.append(f"PUSH {node.value:03X}")

    def generate_bool_code(self, node):
        """Генерация кода для булевского значения."""
        value = 1 if node.value else 0
        self.assembler_code.append(f"PUSH {value:03X}")

    def generate_string_code(self, node):
        """Генерация кода для строкового значения."""
        for char in node.value:
            self.assembler_code.append(f"PUSH {ord(char):03X}")
            self.assembler_code.append("OUT")

    def generate_symbol_code(self, node):
        """Генерация кода для символа (переменной)."""
        address = self.allocate_memory(node.value)
        self.assembler_code.append(f"LOAD {address:03X}")

    def generate_input_code(self, node):
        """Генерация кода для ввода данных."""
        self.assembler_code.append("IN")

    def get_assembler_code(self):
        """Возвращает сгенерированный ассемблерный код."""
        return "\n".join(self.assembler_code)
