from enum import IntEnum, Enum, auto

class DTYPES(Enum):
    INT = auto()
    CHAR = auto()
    BOOL = auto()
    VOID = auto()
    SUB = auto()
    DEV = auto()
    POINTER = auto()
    ARRAY = auto()

class DataType:
    def __init__(self, name: str, type: DTYPES):
        self.name = name
        self.type = type

    def getSize(self) -> int:
        raise NotImplementedError

    # should be overwritten for usertypes
    def compare(self, type):
        return self.type == type.type

    # for pointer arithmetic
    def getPSize(self) -> int:
        return self.getSize()

    # for stack pushes/pops
    def getStackSize(self) -> int:
        return self.getSize()

class Variable:
    def __init__(self, name: str, dtype: DataType):
        self.name = name
        self.dtype = dtype

class VARINFOINDXS(IntEnum):
    GLOBAL = -1,
    SUBROUTINE = -2,
    DEVICE = -3

# variable info, including the variable (name & datatype) and the index in the stack
class VarInfo:
    def __init__(self, var: Variable, indx: int): # indx >= 0 means on the heap, < 0 means a VARINFOINDXS
        self.var = var
        self.indx = indx

class Pointer(DataType):
    def __init__(self, pointerToType: DataType):
        super().__init__("*%s" % pointerToType.name, DTYPES.POINTER)
        self.pType = pointerToType

    def getSize(self) -> int:
        return 2 # we push the absolute address to the stack

    def getPSize(self) -> int:
        return self.pType.getSize()

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

    # pointer arithmetic for void* acts like it's a size of 1
    def getPSize(self) -> int:
        return 1

class DataArray(DataType):
    def __init__(self, pType: DataType, size: int):
        super().__init__("%s[%d]" % (pType.name, size), DTYPES.ARRAY)
        self.pType = pType
        self.size = size

    def getSize(self) -> int:
        # size of the array is the size of the datatype * the number of elements
        return self.pType.getSize() * self.size

    def getPSize(self) -> int:
        return self.pType.getSize()

    def getStackSize(self) -> int:
        return 2 # an absolute address is pushed :P