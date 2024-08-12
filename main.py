from Lexer import Lexer
from Parser import Parser

if __name__ == "__main__":
    file = open("file.lsp")
    code = file.read()

    lexer = Lexer()
    tokens = lexer.tokenize(code)
    parser = Parser(tokens)
    ast = parser.parse()
    # Печать AST в виде дерева
    print(ast._str())
