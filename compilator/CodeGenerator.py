from compilator.ast_nodes import *

class CodeGenerator:
    def __init__(self):
        self.assembler_code = []
        self.symbol_table = {}  # Таблица символов для хранения адресов переменных
        self.label_table = {}  # Таблица меток и их адресов
        self.label_counter = 0  # Счётчик для создания уникальных меток
        self.data_memory_address = 0x000  # Начальный адрес для данных
        self.current_address = 0  # Текущий адрес инструкции

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
        elif isinstance(node, FunctionCall):
            self.generate_call_code(node)
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
        for var, val in node.bindings:
            self.generate_code(val)
            address = self.allocate_memory(var)
            self.assembler_code.append(f"STORE {address:03X}")
            self.current_address += 1
        
        if isinstance(node.body, list):
            for expr in node.body:
                self.generate_code(expr)
        else:
            self.generate_code(node.body)

    def generate_setq_code(self, node):
        var_name = node.variable
        value = node.value
        self.generate_code(value)
        address = self.allocate_memory(var_name)
        self.assembler_code.append(f"STORE {address:03X}")
        self.current_address += 1

    def generate_if_code(self, node):
        else_label = self.generate_label()
        end_label = self.generate_label()

        self.generate_code(node.condition)
        self.assembler_code.append(f"JZ {else_label}")
        self.current_address += 1

        self.generate_code(node.then_branch)
        self.assembler_code.append(f"JMP {end_label}")
        self.current_address += 1

        self.assembler_code.append(f"{else_label}:")
        self.label_table[else_label] = self.current_address

        if node.else_branch:
            self.generate_code(node.else_branch)

        self.assembler_code.append(f"{end_label}:")
        self.label_table[end_label] = self.current_address

    def generate_for_code(self, node):
        start_label = self.generate_label()
        end_label = self.generate_label()

        self.generate_code(node.start_expr)
        var_address = self.allocate_memory(node.loop_var)
        self.assembler_code.append(f"STORE {var_address:03X}")
        self.current_address += 1

        self.assembler_code.append(f"{start_label}:")
        self.label_table[start_label] = self.current_address

        self.generate_code(node.end_expr)
        self.assembler_code.append(f"LOAD {var_address:03X}")
        self.current_address += 1

        self.assembler_code.append("SUB")
        self.current_address += 1

        self.assembler_code.append(f"JZ {end_label}")
        self.current_address += 1

        for expr in node.body:
            self.generate_code(expr)

        self.assembler_code.append(f"LOAD {var_address:03X}")
        self.current_address += 1

        self.assembler_code.append("PUSH 1")
        self.current_address += 1

        self.assembler_code.append("ADD")
        self.current_address += 1

        self.assembler_code.append(f"STORE {var_address:03X}")
        self.current_address += 1

        self.assembler_code.append(f"JMP {start_label}")
        self.current_address += 1

        self.assembler_code.append(f"{end_label}:")
        self.label_table[end_label] = self.current_address

    def generate_print_code(self, node):
        self.generate_code(node.value)
        self.assembler_code.append("OUT")
        self.current_address += 1

    def generate_binary_operation_code(self, node):
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
        self.current_address += 1

    def generate_unary_operation_code(self, node):
        if node.operator == "parse-int":
            pass
            # self.generate_code(node.operand)
            # self.assembler_code.append("CALL PARSE_INT_ROUTINE")
        else:
            raise ValueError(f"Неизвестный унарный оператор: {node.operator}")
        self.current_address += 1

    def generate_intval_code(self, node):
        """Обрабатываем значение целого числа, включая отрицательные."""
        value = node.value
        
        if value < 0:
            # Преобразуем отрицательное число в 2-комплементарную форму
            value = (1 << 11) + value  # Добавляем 2^11 к значению
        
        self.assembler_code.append(f"PUSH {value:03X}")
        self.current_address += 1

    def generate_bool_code(self, node):
        value = 1 if node.value else 0
        self.assembler_code.append(f"PUSH {value:03X}")
        self.current_address += 1

    def generate_string_code(self, node):
        for char in node.value:
            self.assembler_code.append(f"PUSH {ord(char):03X}")
            self.assembler_code.append("OUT")
            self.current_address += 2

    def generate_symbol_code(self, node):
        address = self.allocate_memory(node.value)
        self.assembler_code.append(f"LOAD {address:03X}")
        self.current_address += 1

    def generate_input_code(self, node):
        self.assembler_code.append("IN")
        self.current_address += 1

    def generate_call_code(self, node):
        if isinstance(node, FunctionCall):
            function_label = node.function_name
            if function_label not in self.label_table:
                raise ValueError(f"Функция {function_label} не определена")

            for arg in node.arguments:
                self.generate_code(arg)
            self.assembler_code.append(f"CALL {function_label}")
        else:
            raise ValueError(f"Неверный тип узла для CALL: {type(node)}")
        self.current_address += 1

    def generate_function_code(self, node):
        function_name = node.name
        self.label_table[function_name] = self.current_address
        self.assembler_code.append(f"{function_name}:")
        for expr in node.body:
            self.generate_code(expr)
        self.assembler_code.append("RET")
        self.current_address += 1


    def get_assembler_code(self):
        """Возвращает сгенерированный ассемблерный код."""
        return "\n".join(self.assembler_code)

    def get_binary_code(self):
        binary_code = ""
        for instruction in self.assembler_code:
            parts = instruction.split()
            opcode = parts[0]
            binary_instruction = ""

            if opcode in self.isa_opcode:
                binary_instruction += f"{self.isa_opcode[opcode]:05b}"
                
                if len(parts) > 1:  # Если есть аргументы
                    operand = parts[1]
                    
                    if opcode in ["PUSH", "LOAD", "STORE"]:
                        # Преобразуем операнд в 12-битное число (адрес или значение)
                        operand_bin = f"{int(operand, 16):012b}"
                        binary_instruction += operand_bin

                    elif opcode == "CALL":
                        # Для CALL используем адрес функции из таблицы меток
                        label_bin = f"{self.label_table[operand]:012b}"
                        binary_instruction += label_bin

                    elif opcode.startswith("J"):
                        # Для команд перехода добавляем 12-битную метку
                        label_bin = f"{self.label_table[operand]:012b}"
                        binary_instruction += label_bin
                else:
                    # Добавляем 12 нулевых бит для выравнивания длины команды без операнда
                    binary_instruction += "0" * 12
            if binary_instruction:
                binary_code += binary_instruction + "\n"
        return binary_code.strip()

    isa_opcode = {
        "PUSH":  0b00000,
        "POP":   0b00001,
        "ADD":   0b00010,
        "SUB":   0b00011,
        "MUL":   0b00100,
        "DIV":   0b00101,
        "AND":   0b00110,
        "OR":    0b00111,
        "NOT":   0b01000,
        "LOAD":  0b01001,
        "STORE": 0b01010,
        "IN":    0b01011,
        "OUT":   0b01100,
        "JMP":   0b01101,
        "CMP":   0b01110,
        "JZ":    0b01111,
        "JNZ":   0b10000,
        "JLT":   0b10001,
        "JLE":   0b10010,
        "JGT":   0b10011,
        "JGE":   0b10100,
        "JEQ":   0b10101,
        "JNE":   0b10110,
        "CALL":  0b10111,
        "RET":   0b11000,
    }
