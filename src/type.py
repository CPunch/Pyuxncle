from enum import Enum, auto

class DTYPES(Enum):
    INT = auto()
    CHAR = auto()
    BOOL = auto()
    VOID = auto()
    SUB = auto()
    DEV = auto()
    POINTER  = auto()

class DataType:
    def __init__(self, name: str, type: DTYPES):
        self.name = name
        self.type = type

    def getSize(self) -> int:
        raise NotImplementedError

    # should be overwritten for usertypes
    def compare(self, type):
        return self.type == type.type

class Variable:
    def __init__(self, name: str, dtype: DataType):
        self.name = name
        self.dtype = dtype

# variable info, including the variable (name & datatype) and the index in the stack
class VarInfo:
    def __init__(self, var: Variable, indx: int): # indx of -1 means a global, -2 means subroutine, -3 means device
        self.var = var
        self.indx = indx

class Pointer(DataType):
    def __init__(self, pointerToType: DataType):
        super().__init__("*%s" % pointerToType.name, DTYPES.POINTER)
        self.pType = pointerToType

    def getSize(self) -> int:
        return 2 # we push the absolute address to the stack

    def compare(self, other):
        return other.type == DTYPES.POINTER and other.pType.compare(self.pType)

class Subroutine(DataType):
    def __init__(self, retType: DataType, name: str):
        super().__init__("_func", DTYPES.SUB)
        self.instrs = ""
        self.args: list[DataType] = []
        self.retType = retType
        self.subname = name

    def addArg(self, dtype: DataType):
        self.args.append(dtype)

    def addUnxtal(self, uxntal: str):
        self.instrs = self.instrs + uxntal

    def getSize(self) -> int:
        return 2 # the absolute address is pushed onto the stack :D

    def compare(self, type):
        return False # TODO

class IndxInfo:
    def __init__(self, indx: int, dtype: DataType):
        self.indx = indx
        self.dtype = dtype

class Device(DataType):
    def __init__(self, name: str, addr: int):
        super().__init__("DEV: %s" % name, DTYPES.DEV)
        self.devname = name
        self.members: list[DataType] = []
        self.addr = addr # zeropage address

    def addMember(self, mem: Variable):
        self.members.append(mem)

    # returns VarInfo of member, or None if not found
    def searchMembers(self, ident: str) -> IndxInfo:
        indx = 0
        for mem in self.members:
            if mem.name == ident:
                return IndxInfo(indx, mem.dtype)

            indx += mem.dtype.getSize()

        return None

    def getSize(self):
        sz = 0
        for mem in self.members:
            sz += mem.dtype.getSize()
        return sz

    def compare(self, type):
        return False

class IntDataType(DataType):
    def __init__(self):
        super().__init__("int", DTYPES.INT)

    def getSize(self):
        return 2

class CharDataType(DataType):
    def __init__(self):
        super().__init__("char", DTYPES.CHAR)

    def getSize(self):
        return 1

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