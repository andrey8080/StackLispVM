# class CodeGenerator:
#     def __init__(self):
#         self.instructions = []
#         self.variables = {}
#         self.label_count = 0

#     def generate(self, ast):
#         for node in ast:
#             self.eval(node)

#     def eval(self, node):
#         if node[0] == 'let':
#             _, bindings, body = node
#             for ident, expr in bindings:
#                 self.eval(expr)
#                 self.variables[ident] = len(self.variables)
#                 self.instructions.append(('STORE', self.variables[ident]))
#             self.eval(body)
#         elif node[0] == 'setq':
#             _, ident, expr = node
#             self.eval(expr)
#             self.instructions.append(('STORE', self.variables[ident]))
#         elif node[0] == 'print':
#             _, expr = node
#             self.eval(expr)
#             self.instructions.append(('PRINT',))
#         elif node[0] == 'if':
#             _, cond, true_expr, false_expr = node
#             self.eval(cond)
#             false_label = self.new_label()
#             end_label = self.new_label()
#             self.instructions.append(('JZ', false_label))
#             self.eval(true_expr)
#             self.instructions.append(('JMP', end_label))
#             self.instructions.append(('LABEL', false_label))
#             self.eval(false_expr)
#             self.instructions.append(('LABEL', end_label))
#         elif node[0] == 'function':
#             _, name, params, body = node
#             self.instructions.append(('LABEL', name))
#             for param in params:
#                 self.variables[param] = len(self.variables)
#             self.eval(body)
#             self.instructions.append(('RET',))
#         elif node[0] == 'run-maths':
#             self.instructions.append(('CALL', 'run-maths'))
#         elif node[0] == 'number':
#             _, value = node
#             self.instructions.append(('PUSH', value))
#         elif node[0] == 'string':
#             _, value = node
#             self.instructions.append(('PUSH', value))
#         elif node[0] == 'input':
#             self.instructions.append(('INPUT',))
#         elif node[0] == 'op':
#             op = node[1]
#             if op == '<':
#                 self.instructions.append(('LT',))
#             elif op == '>':
#                 self.instructions.append(('GT',))

#     def new_label(self):
#         label = f'label_{self.label_count}'
#         self.label_count += 1
#         return label

#     def to_binary(self):
#         binary_instructions = []
#         for instr in self.instructions:
#             if instr[0] == 'PUSH':
#                 binary_instructions.append(0b0001)
#                 binary_instructions.append(instr[1])
#             elif instr[0] == 'STORE':
#                 binary_instructions.append(0b1110)
#                 binary_instructions.append(instr[1])
#             elif instr[0] == 'PRINT':
#                 binary_instructions.append(0b1011)
#             elif instr[0] == 'JZ':
#                 binary_instructions.append(0b1001)
#                 binary_instructions.append(instr[1])
#             elif instr[0] == 'JMP':
#                 binary_instructions.append(0b1010)
#                 binary_instructions.append(instr[1])
#             elif instr[0] == 'LABEL':
#                 binary_instructions.append(0b0110)
#                 binary_instructions.append(instr[1])
#             elif instr[0] == 'CALL':
#                 binary_instructions.append(0b0111)
#                 binary_instructions.append(instr[1])
#             elif instr[0] == 'RET':
#                 binary_instructions.append(0b1000)
#             elif instr[0] == 'INPUT':
#                 binary_instructions.append(0b1100)
#             elif instr[0] == 'LT':
#                 binary_instructions.append(0b0100)
#             elif instr[0] == 'GT':
#                 binary_instructions.append(0b0101)
#         return binary_instructions
