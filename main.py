from compilator.Lexer import Lexer
from compilator.Parser import Parser
from compilator.CodeGenerator import CodeGenerator

if __name__ == "__main__":
    file = open("progs/file.lsp")
    code = file.read()

    lexer = Lexer()
    tokens = lexer.tokenize(code)
    parser = Parser(tokens)
    ast = parser.parse()
    # Печать AST в виде дерева
    print("Абстрактное синтаксическое дерево")
    print(ast._str())

    # Создание генератора кода
    codegen = CodeGenerator()

    # Генерация ассемблерного кода из AST
    codegen.generate_code(ast)

    # Получение сгенерированного ассемблерного кода
    assembly_code = codegen.get_assembler_code()

    # Печать кода
    print("assembly code")
    print(assembly_code)