import sys
from compilator.Lexer import Lexer
from compilator.Parser import Parser
from compilator.CodeGenerator import CodeGenerator

if __name__ == "__main__":
    flag = sys.argv[1]

    if flag == "-asm" :
        path_file_lsp = sys.argv[2]
        path_file_asm = sys.argv[3]
        file_lsp = open(path_file_lsp)
        file_asm = open(path_file_asm, 'w')

        code = file_lsp.read()
        lexer = Lexer()
        tokens = lexer.tokenize(code)
        parser = Parser(tokens)
        ast = parser.parse()    
        codegen = CodeGenerator()
        codegen.generate_code(ast)
        assembly_code = codegen.get_assembler_code()

        for stroke in assembly_code:
            file_asm.write(stroke)

    if flag == "-bin" :
        path_file_lsp = sys.argv[2]
        path_file_bin = sys.argv[3]
        file_lsp = open(path_file_lsp)
        file_bin = open(path_file_bin, 'w')

        code = file_lsp.read()
        lexer = Lexer()
        tokens = lexer.tokenize(code)
        parser = Parser(tokens)
        ast = parser.parse()
        codegen = CodeGenerator()
        codegen.generate_code(ast)
        assembly_code = codegen.get_assembler_code()
        binary_code = codegen.get_binary_code()

        for stroke in binary_code:
            file_bin.write(stroke)