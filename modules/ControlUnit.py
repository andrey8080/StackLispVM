from isa import Opcode

class ControlUnit:
    def __init__(self, program, data_path):
        self.program = program
        self.program_counter = 0
        self.data_path = data_path

    def fetch_instruction(self):
        instruction = self.program[self.program_counter]
        return instruction

    def decode_and_execute_instruction(self):
        instr = self.fetch_instruction()
        opcode = Opcode(instr["opcode"])  # Преобразуем число в объект Enum

        if opcode in {Opcode.PUSH, Opcode.LOAD, Opcode.STORE}:
            self.data_path.signal_latch_data_addr(opcode)
        elif opcode in {Opcode.ADD, Opcode.SUB, Opcode.MUL, Opcode.DIV, Opcode.AND, Opcode.OR, Opcode.NOT}:
            self.data_path.signal_wr(opcode)
        elif opcode == Opcode.OUT:
            self.data_path.signal_output()
        elif opcode == Opcode.IN:
            self.data_path.signal_latch_data_addr(Opcode.LOAD)

        self.program_counter += 1

    def run(self):
        while self.program_counter < len(self.program):
            self.decode_and_execute_instruction()
