from modules.ALU import ALU
from modules.ControlUnit import ControlUnit
from modules.DataPath import DataPath


# Пример использования
if __name__ == "__main__":
    program = ???
    input_buffer = ???

    data_path = DataPath(256, input_buffer)
    control_unit = ControlUnit(program, data_path)

    control_unit.run()

    print("Output:", data_path.get_output())
