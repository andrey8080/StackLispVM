from enum import Enum

class Opcode(int, Enum):
    PUSH =    0b00000000
    POP =     0b00000001
    ADD =     0b00000010
    SUB =     0b00000011
    MUL =     0b00000100
    DIV =     0b00000101
    AND =     0b00000110
    OR =      0b00000111
    NOT =     0b00001000
    LOAD =    0b00001001
    STORE =   0b00001010
    IN =      0b00001011
    OUT =     0b00001100
    JMP =     0b00001101
    CMP =     0b00001110
    JZ =      0b00001111
    JNZ =     0b00010000
    JLT =     0b00010001
    JLE =     0b00010010
    JGT =     0b00010011
    JGE =     0b00010100
    JEQ =     0b00010101
    JNE =     0b00010110
    CALL =    0b00010111
    RET =     0b00011000
