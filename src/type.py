from enum import Enum, auto

class DTYPES(Enum):
    INT = auto()
    BOOL = auto()
    VOID = auto()
    SUB = auto()

class DataType:
    def __init__(self, name: str, type: DTYPES):
        self.name = name
        self.type = type

    def getSize(self) -> int:
        raise NotImplementedError

    # should be overwritten for usertypes
    def compare(self, type):
        return self.type == type.type

class Subroutine(DataType):
    def __init__(self, retType: DataType):
        super().__init__("_func", DTYPES.SUB)
        self.instrs = ""
        self.args: list[DataType] = []
        self.retType = retType

    def addArg(self, dtype: DataType):
        self.args.append(dtype)

    def addUnxtal(self, uxntal: str):
        self.instrs = self.instrs + uxntal

    def getSize(self) -> int:
        return 2 # the actual data passed to the stack is an absolute address (short)

class IntDataType(DataType):
    def __init__(self):
        super().__init__("int", DTYPES.INT)

    def getSize(self):
        return 2

class BoolDataType(DataType):
    def __init__(self):
        super().__init__("bool", DTYPES.BOOL)

    def getSize(self):
        return 1

class VoidDataType(DataType):
    def __init__(self):
        super().__init__("void", DTYPES.VOID)

    def getSize(self) -> int:
        return 0

class VoidDataType(DataType):
    def __init__(self):
        super().__init__("void", DTYPES.VOID)

    def getSize(self):
        return 0