from isa import Opcode
from modules.ALU import ALU

class DataPath:
    def __init__(self, data_memory_size, input_buffer):
        self.data_memory_size = data_memory_size
        self.data_memory = [0] * data_memory_size
        self.data_address = 0
        self.acc = 0
        self.input_buffer = input_buffer
        self.output_buffer = []
        self.alu = ALU()

    def signal_latch_data_addr(self, sel):
        if sel == Opcode.PUSH.value:
            self.data_address = self.acc
        elif sel == Opcode.LOAD.value:
            self.data_address = int(self.input_buffer.pop(0), 16)
        elif sel == Opcode.STORE.value:
            self.data_memory[self.data_address] = self.acc

    def signal_latch_acc(self):
        self.acc = self.alu.compute(self.acc, self.data_memory[self.data_address])

    def signal_wr(self, sel):
        if sel == Opcode.ADD.value:
            self.alu.set_operation(ALU.Operation.ADD)
        elif sel == Opcode.SUB.value:
            self.alu.set_operation(ALU.Operation.SUB)
        elif sel == Opcode.MUL.value:
            self.alu.set_operation(ALU.Operation.MUL)
        elif sel == Opcode.DIV.value:
            self.alu.set_operation(ALU.Operation.DIV)
        elif sel == Opcode.AND.value:
            self.alu.set_operation(ALU.Operation.AND)
        elif sel == Opcode.OR.value:
            self.alu.set_operation(ALU.Operation.OR)
        elif sel == Opcode.NOT.value:
            self.alu.set_operation(ALU.Operation.NOT)

        self.signal_latch_acc()

    def signal_output(self):
        self.output_buffer.append(self.acc)

    def get_output(self):
        return self.output_buffer
