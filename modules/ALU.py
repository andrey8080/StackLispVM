from enum import Enum

class ALU:
    class Operation(Enum):
        ADD = 1
        SUB = 2
        AND = 3
        OR = 4
        NOT = 5
        MUL = 6
        DIV = 7

    def __init__(self):
        self.operation = None

    def set_operation(self, operation):
        self.operation = operation

    def compute(self, operand1, operand2=0):
        if self.operation == ALU.Operation.ADD:
            return operand1 + operand2
        elif self.operation == ALU.Operation.SUB:
            return operand1 - operand2
        elif self.operation == ALU.Operation.AND:
            return operand1 & operand2
        elif self.operation == ALU.Operation.OR:
            return operand1 | operand2
        elif self.operation == ALU.Operation.NOT:
            return ~operand1
        elif self.operation == ALU.Operation.MUL:
            return operand1 * operand2
        elif self.operation == ALU.Operation.DIV:
            return operand1 // operand2