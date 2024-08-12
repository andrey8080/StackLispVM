import re
from typing import List, Tuple, Union

class Token:
    def __init__(self, type_: str, value: str):
        self.type = type_
        self.value = value

    def __repr__(self):
        return f'Token({self.type}, {self.value})'

class Lexer:
    def __init__(self):
        self.token_specification = [
            ('COMMENT',  r';[^\n]*'),
            ('INTVAL',   r'-?\d+'),
            ('BOOL',     r'false|true'),
            ('STRING',   r'"[^"]*"'),
            ('LET',      r'let'),
            ('FOR',      r'loop for'),
            ('IF',       r'if'),
            ('INPUT',    r'input'),
            ('PRINT',    r'print'),
            ('PARSE_INT', r'parse-int'),
            ('SETQ',     r'setq'),
            ('LT',       r'<'),
            ('LE',       r'<='),
            ('GT',       r'>'),
            ('GE',       r'>='),
            ('PLUS',     r'\+'),
            ('MINUS',    r'-'),
            ('MOD',      r'\*'),
            ('DIVIDE',   r'/'),
            ('LPAREN',   r'\('),
            ('RPAREN',   r'\)'),
            ('SYMBOL',   r'[a-zA-Z_][a-zA-Z_0-9\-]*'),
            ('SKIP',     r'[ \t\n]+'),
            ('MISMATCH', r'.'),
        ]
        self.tokens_regex = '|'.join(f'(?P<{pair[0]}>{pair[1]})' for pair in self.token_specification)

    def tokenize(self, code: str) -> List[Token]:
        tokens = []
        for match in re.finditer(self.tokens_regex, code):
            type_ = match.lastgroup
            value = match.group(type_)
            if type_ == 'SKIP' or type_ == 'COMMENT':
                continue
            if type_ == 'MISMATCH':
                raise RuntimeError(f'Unexpected character {value}')
            tokens.append(Token(type_, value))
        return tokens
