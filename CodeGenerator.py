class CodeGenerator:
    def __init__(self):
        self.instructions = []
        self.variables = {}
        self.label_count = 0

    def generate(self, ast):
        for node in ast:
            # self.eval(node)
            continue
# -------------------------------------
    
    def to_binary(self):
        binary_instructions = []
        for instr in self.instructions:
            if instr[0] == 'PUSH':
                binary_instructions.append(0b0001)
                binary_instructions.append(instr[1])
            elif instr[0] == 'STORE':
                binary_instructions.append(0b1110)
                binary_instructions.append(instr[1])
            elif instr[0] == 'PRINT':
                binary_instructions.append(0b1011)
            elif instr[0] == 'JZ':
                binary_instructions.append(0b1001)
                binary_instructions.append(instr[1])
            elif instr[0] == 'JMP':
                binary_instructions.append(0b1010)
                binary_instructions.append(instr[1])
            elif instr[0] == 'LABEL':
                binary_instructions.append(0b0110)
                binary_instructions.append(instr[1])
            elif instr[0] == 'CALL':
                binary_instructions.append(0b0111)
                binary_instructions.append(instr[1])
            elif instr[0] == 'RET':
                binary_instructions.append(0b1000)
            elif instr[0] == 'INPUT':
                binary_instructions.append(0b1100)
            elif instr[0] == 'LT':
                binary_instructions.append(0b0100)
            elif instr[0] == 'GT':
                binary_instructions.append(0b0101)
        return binary_instructions
