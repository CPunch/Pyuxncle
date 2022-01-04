from enum import IntEnum, auto
from collections.abc import Callable
from tokenizer import Lexer, TOKENTYPE, Token
from type import *
from compError import doCompError
import thinlib

class PRECTYPE(IntEnum):
    NONE = 0,
    ASSIGNMENT = 1, # =
    INDEX = 2,      # []
    COMPAR = 3,     # == !=
    TERM = 4,       # + - 
    FACTOR = 5,     # * /
    CALL = 6,       # ( )
    PRIMARY = 7

class _PrecRule:
    def __init__(self, prec: PRECTYPE, prefix: Callable[[DataType, bool, bool, PRECTYPE], DataType],
                                        infix: Callable[[DataType, bool, bool, PRECTYPE], DataType]):
        self.prec = prec
        self.prefix = prefix
        self.infix = infix

class _Constant:
    def __init__(self, id: int, data: str):
        self.id = id
        self.data = data

class _Scope:
    def __init__(self):
        # array of declared variables (and their respective locations in the stack)
        self.vars: list[Variable] = []
        self.allocated = 0
        self.instrs = ''

    def addVar(self, var: Variable):
        self.vars.append(var)

    def getSize(self):
        totalSz = 0
        for var in self.vars:
            totalSz += var.dtype.getSize()

        return totalSz

class Parser:
    def __init__(self, source: str, outFile: str):
        self.lexer = Lexer(source)
        self.current: Token = None
        self.previous: Token = None
        self.out = open(outFile, "w")
        self.scopeStack: list[_Scope] = []
        self.pushed = 0
        self.jmpIDS = 0
        self.parseTable = {
            # TOKEN :               _PrecRule(Precedence,       prefix,         infix)
            TOKENTYPE.SEMICOLON:    _PrecRule(PRECTYPE.NONE,    None,           None),
            TOKENTYPE.LBRACKET:     _PrecRule(PRECTYPE.INDEX,   None,           self.__index),
            TOKENTYPE.RBRACKET:     _PrecRule(PRECTYPE.NONE,    None,           None),
            TOKENTYPE.PLUS:         _PrecRule(PRECTYPE.TERM,    None,           self.__binOp),
            TOKENTYPE.MINUS:        _PrecRule(PRECTYPE.TERM,    None,           self.__binOp),
            TOKENTYPE.STAR:         _PrecRule(PRECTYPE.FACTOR,  self.__pointer, self.__binOp),
            TOKENTYPE.SLASH:        _PrecRule(PRECTYPE.FACTOR,  None,           self.__binOp),
            TOKENTYPE.MOD:          _PrecRule(PRECTYPE.FACTOR,  None,           self.__binOp),
            TOKENTYPE.AMPER:        _PrecRule(PRECTYPE.NONE,    self.__ampersand, None),
            TOKENTYPE.EQUAL:        _PrecRule(PRECTYPE.NONE,    None,           None),
            TOKENTYPE.EQUALEQUAL:   _PrecRule(PRECTYPE.COMPAR,  None,           self.__binOp),
            TOKENTYPE.GRTR:         _PrecRule(PRECTYPE.COMPAR,  None,           self.__binOp),
            TOKENTYPE.LESS:         _PrecRule(PRECTYPE.COMPAR,  None,           self.__binOp),
            TOKENTYPE.GRTREQL:      _PrecRule(PRECTYPE.COMPAR,  None,           self.__binOp),
            TOKENTYPE.LESSEQL:      _PrecRule(PRECTYPE.COMPAR,  None,           self.__binOp),

            TOKENTYPE.LPAREN:       _PrecRule(PRECTYPE.CALL,    self.__group,   self.__callSub),
            TOKENTYPE.RPAREN:       _PrecRule(PRECTYPE.NONE,    None,           None),

            TOKENTYPE.IDENT:        _PrecRule(PRECTYPE.NONE,    self.__ident,   None),
            TOKENTYPE.NUM:          _PrecRule(PRECTYPE.NONE,    self.__number,  None),
            TOKENTYPE.CHARLIT:      _PrecRule(PRECTYPE.NONE,    self.__charlit, None),
            TOKENTYPE.EOF:          _PrecRule(PRECTYPE.NONE,    None,           None)
        }

        self.typeTable = {
            TOKENTYPE.INT : IntDataType(),
            TOKENTYPE.BOOL : BoolDataType(),
            TOKENTYPE.CHAR : CharDataType(),
            TOKENTYPE.VOID : VoidDataType()
        }

        # compiler-related stuff (in the rewrite, this should be given to another module that takes care of building the output uxntal)
        self.lefthand: list[str] = []
        self.entryInstr = ""
        self.currentSub: int = -1
        self.subs: list[Variable] = []
        self.globals: list[Variable] = []
        self.devices: list[Device] = []
        self.constants: list[_Constant] = [] # holds data like strings, constant arrays, etc. [TODO]

    def __errorAt(self, tkn: Token, err: str):
        doCompError("At '%s' on line %d:\n\t%s" % (tkn.word, tkn.line, err))

    def __error(self, err: str):
        self.__errorAt(self.previous, err)

    # advances to the next token received by the lexer
    def __advance(self):
        self.previous = self.current
        self.current = self.lexer.scanNext()
        self.current.print()
        return self.current

    def __check(self, tknType: TOKENTYPE):
        return self.current.type == tknType

    def __match(self, tknType: TOKENTYPE):
        if not self.__check(tknType):
            return False

        self.__advance()
        return True

    # creates a compiler error if the expected token isn't there, else it just consumes the token
    def __consume(self, tknType: TOKENTYPE, errMsg: str):
        if not self.__match(tknType):
            self.__error(errMsg)

    # returns true if the parser reached the end of the token list
    def __isEnd(self):
        return self.__check(TOKENTYPE.EOF)

    def __pushLeftHand(self):
        self.lefthand.append("")

    def __popLeftHand(self) -> str:
        return self.lefthand.pop()

    # expects LBRACKET to already be consumed
    def __consumeArrayType(self, dtype: DataType) -> DataType:
        # NOTE: VLAs are NOT supported, i cba to write support for that lol.
        # (it's technically an extension anyways, besides this is a 'c-like' language)

        # ALSO NOTE: pyuxncle has no support for compiler time constexpr combining. (aka. 2 * 4 would be optimized at compile-time to 8.)
        # so anything in between the '[]' should just be 1 token long, and MUST BE an int.

        self.__consume(TOKENTYPE.NUM, "Expected size of array!")
        sz = self.__grabNumber(self.previous)
        self.__consume(TOKENTYPE.RBRACKET, "Expected end of array definition! (missing ']')")
        return DataArray(dtype, sz)

    def __checkDataType(self) -> DataType:
        # current token isn't a datatype
        if not self.current.type in self.typeTable:
            return None

        dtype = self.typeTable[self.current.type]

        self.__advance()
        if self.__match(TOKENTYPE.STAR): # is it a pointer?
            dtype = Pointer(dtype)
        elif self.__match(TOKENTYPE.LBRACKET):
            dtype = self.__consumeArrayType(dtype)

        return dtype

    # forces a datatype, if not it errors
    def __matchDataType(self) -> DataType:
        dtype = self.__checkDataType()
        if dtype == None:
            self.__errorAt(self.current, "Expected a datatype!")
        return dtype

    # =============== formatting for uxntal output ===============

    def __writeOut(self, buf: str):
        if len(self.lefthand) > 0: # we're parsing an expression that needs to be emitted after the righthand is emitted
            self.lefthand[len(self.lefthand)-1] += buf
        elif self.currentSub == -1: # we're not currently parsing a function, write it to the entry instructions
            self.entryInstr += buf
        else:
            self.subs[self.currentSub].dtype.addUnxtal(buf)

    def __writeIntLiteral(self, num: int):
        self.__writeOut("#%.4x " % num)

    def __writeByteLiteral(self, num: int):
        self.__writeOut("#%.2x " % num)

    # dtype: the datatype of the 2 values on the stack, rtype: the datatype pushed by the instruction
    def __writeBinaryOp(self, op: str, dtype: DataType, rtype: DataType):
        if dtype.type == DTYPES.INT:
            self.__writeOut("%s2\n" % op)
        elif dtype.type == DTYPES.CHAR:
            self.__writeOut("%s\n" % op)
        else:
            self.__error("Cannot perform binary operation on type '%s'" % dtype.name)

        # we popped both values
        self.pushed -= dtype.getSize() * 2
        # we pushed the rtype
        self.pushed += rtype.getSize()

    # creates new jump ID, does NOT write the label
    def __newJmpLbl(self) -> int:
        self.jmpIDS += 1
        return self.jmpIDS

    def __declareLbl(self, id: int):
        self.__writeOut("&lbl%d\n" % id)

    # expects BOOL already on the stack
    def __jmpCondLbl(self, id: int):
        self.__writeOut(",&lbl%d JCN\n" % id)
        self.pushed -= 1

    # jumps to label
    def __jmpLbl(self, id: int):
        self.__writeOut(",&lbl%d JMP\n" % id)

    # ===================== scope management =====================

    # TODO since this a single-pass compiler, it's kind of hard to get the whole scope size before writting the allocation.
    # maybe hold the index of the intliteral to patch int __popScope? the returned VarInfo is valid until __addScopeVar
    # or __popScope is called again
    def __addScopeVar(self, var: Variable) -> VarInfo:
        # if we're not parsing a function, define the variable as a global
        if self.currentSub == -1:
            self.globals.append(var)
            return VarInfo(var, -1)

        self.scopeStack[-1].addVar(var) # add the variable to the current scope
        self.__writeIntLiteral(var.dtype.getSize())
        self.__writeOut(";alloc-uxncle JSR2\n")
        return VarInfo(var, var.dtype.getSize())

    def __newScope(self):
        self.scopeStack.append(_Scope())

    # pop scopes without actually removing anything from the scope stack
    def __popRawScopes(self, scopes: int):
        sz = 0
        for i in range(scopes):
            scope = self.scopeStack[(len(self.scopeStack)-1) - i]
            sz += scope.getSize()

        if sz != 0: # minor optimization
            self.__writeIntLiteral(sz)
            self.__writeOut(";dealloc-uxncle JSR2\n")

    # pop the stack and deallocate all the variables in scope from the heap
    def __popScope(self):
        self.__popRawScopes(1)
        self.scopeStack.pop()

    # pops size from the uxn stack
    def __pop(self, size: int):
        self.pushed -= size

        # while we can, pop 2 bytes off the stack at a time
        while size > 1:
            size -= 2
            self.__writeOut("POP2\n")

        # if we still have a leftover byte, pop that too
        if size == 1:
            self.__writeOut("POP\n")

    # duplicates the value currently on the stack
    def __dupVal(self, dtype: DataType):
        if dtype.getSize() == 2:
            self.__writeOut("DUP2\n")
        elif dtype.getSize() == 1:
            self.__writeOut("DUP\n")
        else:
            self.__error("Can't dup values greater than 2!")

        self.pushed += dtype.getSize()

    def __setDevice(self, dev: Device, indx: int, type: DataType):
        self.__writeOut(".%s " % dev.devname)

        if indx > 0:
            self.__writeByteLiteral(indx)
            self.__writeOut("ADD ")

        if type.getSize() == 1:
            self.__writeOut("DEO\n")
        elif type.getSize() == 2:
            self.__writeOut("DEO2\n")
        else:
            self.__error("Can't set value of device > 2 bytes!")

        self.pushed -= type.getSize()

    def __getDevice(self, dev: Device, indx: int, type: DataType):
        self.__writeOut(".%s " % dev.devname)

        if indx > 0:
            self.__writeByteLiteral(indx)
            self.__writeOut("ADD ")

        if type.getSize() == 1:
            self.__writeOut("DEI\n")
        elif type.getSize() == 2:
            self.__writeOut("DEI2\n")
        else:
            self.__error("Can't get value of device > 2 bytes!")

        self.pushed += type.getSize()

    # searches for the variable in the scope stack, returns the index needed to be passed to ;poke-uxncle-* if var isn't found None is returned
    def __findVar(self, name: str) -> VarInfo:
        indx = 0

        # walk each scope (from top)
        for i in range(len(self.scopeStack)):
            scope = self.scopeStack[(len(self.scopeStack)-1)-i]
            # walk each variable in the scope (from top)
            for var in reversed(scope.vars):
                indx += var.dtype.getSize()

                # if the variable is the one we're looking for, return the index of the variable in our heap
                if var.name == name:
                    return VarInfo(var, indx)

        # variable wasn't found, check for it in our global array?
        for var in self.globals:
            if var.name == name:
                return VarInfo(var, -1) # found it! return as a global

        # global wasn't found, maybe its a function
        for sub in self.subs:
            if sub.name == name:
                return VarInfo(sub, -2)

        for dev in self.devices:
            if dev.devname == name:
                return VarInfo(dev, -3)

        return None

    # reads variable from heap & pushes to stack
    def __getVar(self, varInfo: VarInfo):
        dtype = varInfo.var.dtype

        # if array, return absolute
        if dtype.type == DTYPES.ARRAY:
            self.__getVarAddr(varInfo)
            return

        # is it a global?
        if varInfo.indx == -1:
            # read global
            if dtype.getSize() == 2:
                self.__writeOut(";globals/%s LDA2\n" % varInfo.var.name)
            elif dtype.getSize() == 1:
                self.__writeOut(";globals/%s LDA\n" % varInfo.var.name)
            else:
                self.__error("Can't set '%s': size greater than 2!" % varInfo.var.name)
        elif varInfo.indx == -2: # it's a sub, out the absolute address
            self.__writeOut(";SUB_%s " % varInfo.var.name)
        else: # it's a normal var stored in the heap
            # read variable from the heap
            if dtype.getSize() == 2:
                self.__writeIntLiteral(varInfo.indx)
                self.__writeOut(";peek-uxncle-short JSR2\n")
            elif dtype.getSize() == 1:
                self.__writeIntLiteral(varInfo.indx)
                self.__writeOut(";peek-uxncle JSR2\n")
            else:
                self.__error("Can't set '%s': size greater than 2!" % varInfo.var.name)

        # peek-uxncle pushes the value from the heap to the stack
        self.pushed += dtype.getSize()

    # pops value from stack and writes to heap
    def __setVar(self, varInfo: VarInfo):
        dtype = varInfo.var.dtype

        if dtype.type == DTYPES.ARRAY:
            self.__error("Can't set '%s': cannot set array type!")

        # is it a global?
        if varInfo.indx == -1:
            # set global
            if dtype.getSize() == 2:
                self.__writeOut(";globals/%s STA2\n" % varInfo.var.name)
            elif dtype.getSize() == 1:
                self.__writeOut(";globals/%s STA\n" % varInfo.var.name)
            else:
                self.__error("Can't set '%s': size greater than 2!" % varInfo.var.name)
        elif varInfo.indx == -2: # it's a sub, can't set that!
            self.__error("Can't set '%s': constant function!" % varInfo.var.name)
        else: # it's a normal var stored in the heap
            # set variable
            if dtype.getSize() == 2:
                self.__writeIntLiteral(varInfo.indx)
                self.__writeOut(";poke-uxncle-short JSR2\n")
            elif dtype.getSize() == 1:
                self.__writeIntLiteral(varInfo.indx)
                self.__writeOut(";poke-uxncle JSR2\n")
            else:
                self.__error("Can't set '%s': size greater than 2!" % varInfo.var.name)

        # poke-uxncle pops the value from the stack
        self.pushed -= dtype.getSize()

    def __getVarAddr(self, varInfo: VarInfo):
        dtype = varInfo.var.dtype

        if varInfo.indx == -1: # it's a global! our job is easy, just push the absolute address
            self.__writeOut(";globals/%s " % varInfo.var.name)
        elif varInfo.indx == -2: # it's a subroutine! our job is easy again, just push the absolute address (again)
            self.__writeOut(";%s " % dtype.subname)
        elif varInfo.indx == -3: # it's a device! we only alow DEO/DEI to interact with the devices address directly
            self.__error("Can't get address of device!")
        else: # it's a normal variable, we'll need to push it's heap address.
            self.__writeOut(".uxncle/heap LDZ2 ")
            self.__writeIntLiteral(varInfo.indx)
            self.__writeOut("SUB2\n") # subtract our index from the top heap address. now the absolute address of the local var is on the stack :D

        self.pushed += 2 # we pushed an absolute address onto the stack

    # expects fromType to already be on the stack
    def __tryTypeCast(self, fromType: DataType, toType: DataType) -> bool:
        # if the types are the same, do nothing
        if fromType == None or toType == None or fromType.compare(toType):
            return True

        self.pushed += toType.getSize() - fromType.getSize()

        # convert (BOOL or CHAR) to (BOOL or CHAR) 
        if (fromType.type == DTYPES.BOOL and toType.type == DTYPES.CHAR) or (fromType.type == DTYPES.CHAR and toType.type == DTYPES.BOOL):
            return True # they're basically the same datatype
        # convert INT to (BOOL or CHAR)
        elif fromType.type == DTYPES.INT and (toType.type == DTYPES.BOOL or toType.type == DTYPES.CHAR):
            self.__writeOut("SWP POP ") # pops the most significant byte
            return True
        # convert (BOOL or CHAR) to INT
        elif (fromType.type == DTYPES.BOOL or fromType.type == DTYPES.CHAR) and toType.type == DTYPES.INT:
            self.__writeOut("#00 SWP ")
            return True
        # convert pointer to INT, (it's already the proper size)
        elif fromType.type == DTYPES.POINTER and toType.type == DTYPES.INT:
            return True

        return False

    def __isWalkable(self, dtype: DataType):
        return dtype.type == DTYPES.DEV

    # ======================== parse rules =======================

    def __getRule(self, tkn: TOKENTYPE) -> _PrecRule:
        if tkn.type not in self.parseTable:
            self.__errorAt(tkn, "Unknown Token, maybe forgot a ';'?")

        return self.parseTable[tkn.type]

    def __grabNumber(self, tkn: Token):
        num = int(tkn.word, 0) # python can guess the base using the 0* prefix if you pass base 0 (https://docs.python.org/3.4/library/functions.html?highlight=int#int)

        if num > 65536:
            self.__error("Number literal '%d' is too big! (> 65536)" % num)

        return num

    # parses number literal
    def __number(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        # the expression expects nothing, return nothing (void)
        if not expectValue:
            return VoidDataType()

        num = self.__grabNumber(self.previous)

        if num > 256:
            self.__writeIntLiteral(num)
            self.pushed += 2
            return IntDataType()
        else:
            self.__writeByteLiteral(num)
            self.pushed += 1
            return CharDataType()

    # parses character literal
    def __charlit(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        self.__writeByteLiteral(ord(self.previous.word[1]))
        self.pushed += 1
        return CharDataType()

    def __index(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        if leftType.type != DTYPES.POINTER and leftType.type != DTYPES.ARRAY:
            self.__error("ltype error! Expected lefthand expression of pointer or array type! Got '%s'!" % leftType.name)

        exprType = self.__expression()

        if not self.__tryTypeCast(exprType, IntDataType()):
            self.__error("Expected type of 'int' for index, got '%s'" % exprType.name)

        self.__consume(TOKENTYPE.RBRACKET, "Expected ']' to end index!")

        # get offset data from pointer (multiply pointer size with the index)
        self.__writeIntLiteral(leftType.pType.getPSize())
        self.__writeOut("MUL2\n")

        # add pointer and offset together to get offset
        self.__writeOut("ADD2\n")

        # popped offset
        self.pushed -= 2

        if canAssign and self.__match(TOKENTYPE.EQUAL):
            # assigning to index

            addrInstr = self.__popLeftHand() # encapsulate the pointer expression
            self.__pushLeftHand()

            # write the value that needs to be set first
            valType = self.__expression()

            if not self.__tryTypeCast(valType, leftType.pType):
                self.__error("Expected expression of type '%s', got '%s'!" % (leftType.pType.name, valType.name))

            if expectValue:
                self.__dupVal(leftType.pType)

            # push address onto stack
            self.__writeOut(addrInstr)

            # set value
            if leftType.pType.getSize() == 2:
                self.__writeOut("STA2\n")
            elif leftType.pType.getSize() == 1:
                self.__writeOut("STA\n")
            else:
                self.__error("Can't set value > 2 bytes!")

            self.pushed -= leftType.pType.getSize() # popped data on stack
            self.pushed -= 2 # popped address
        else:
            # load data from that address (it's an absolute address on the stack)
            if leftType.pType.getSize() == 2:
                self.__writeOut("LDA2\n")
            elif leftType.pType.getSize() == 1:
                self.__writeOut("LDA\n")
            else:
                self.__error("Can't get value > 2 bytes!")

            self.pushed += leftType.pType.getSize() # index is now on stack
            self.pushed -= 2 # popped address

        return leftType.pType

    # parses binary operators
    def __binOp(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        tkn = self.previous

        # parse the righthand side of the expression (if needed)
        rtype = self.__parsePrecedence(PRECTYPE(int(precLevel.value) + 1), expectValue)

        # the expression expects nothing, return nothing (void)
        if not expectValue:
            return VoidDataType()

        # try to convert the expression to our expected type
        if not self.__tryTypeCast(rtype, leftType):
            self.__errorAt(tkn, "Cannot convert '%s' to '%s'!" % (rtype.name, leftType.name))

        if tkn.type == TOKENTYPE.PLUS:
            self.__writeBinaryOp("ADD", leftType, leftType)
        elif tkn.type == TOKENTYPE.MINUS:
            self.__writeBinaryOp("SUB", leftType, leftType)
        elif tkn.type == TOKENTYPE.STAR:
            self.__writeBinaryOp("MUL", leftType, leftType)
        elif tkn.type == TOKENTYPE.SLASH:
            self.__writeBinaryOp("DIV", leftType, leftType)
        elif tkn.type == TOKENTYPE.MOD:
            self.__writeBinaryOp("DIVk", leftType, leftType)
            self.__writeBinaryOp("MUL", leftType, leftType)
            self.__writeBinaryOp("SUB", leftType, leftType)
        elif tkn.type == TOKENTYPE.EQUALEQUAL:
            self.__writeBinaryOp("EQU", leftType, BoolDataType())
            leftType = BoolDataType()
        elif tkn.type == TOKENTYPE.GRTR:
            self.__writeBinaryOp("GTH", leftType, BoolDataType())
            leftType = BoolDataType()
        elif tkn.type == TOKENTYPE.LESS:
            self.__writeBinaryOp("LTH", leftType, BoolDataType())
            leftType = BoolDataType()
        elif tkn.type == TOKENTYPE.GRTREQL:
            # >= is just *not* <. so check if it's less than, and NOT the result
            self.__writeBinaryOp("LTH", leftType, BoolDataType())
            self.__writeOut("#00 EQU\n")
            leftType = BoolDataType()
        elif tkn.type == TOKENTYPE.LESSEQL:
            # <= is just *not* >. so check if it's greater than, and NOT the result
            self.__writeBinaryOp("GTH", leftType, BoolDataType())
            self.__writeOut("#00 EQU\n")
            leftType = BoolDataType()
        else: # should never happen
            self.__errorAt(tkn, "Invalid binary operator token!")

        return leftType

    def __ampersand(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        self.__consume(TOKENTYPE.IDENT, "Expected identifier after '&'!")
        ident = self.previous

        # check if the identifier exists
        varInfo = self.__findVar(ident.word)
        if varInfo == None:
            self.__error("Unknown identifier '%s'!" % ident.word)

        # TODO: call __walkIdent

        self.__getVarAddr(varInfo)
        return Pointer(varInfo.var.dtype)

    def __pointer(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        self.__pushLeftHand() # we don't want to emit the instructions immediately, we'll need to emit them after the value (if we are setting) is on the stack
        ltype = self.__parsePrecedence(PRECTYPE(int(PRECTYPE.ASSIGNMENT.value)+1), True)
        leftExpr = self.__popLeftHand() # grab the not-yet-emitted lefthand expression instructions

        if not ltype.type == DTYPES.POINTER:
            self.__error("Expected expression to evaluate to a pointer, got '%s'!", ltype.name)

        if canAssign and self.__match(TOKENTYPE.EQUAL): # set the address to the next expression
            rtype = self.__expression()

            # try to convert data to the expected type
            if not self.__tryTypeCast(rtype, ltype.pType):
                self.__error("Couldn't convert expression of type '%s' to '%s'!" % (rtype.name, ltype.pType.name))

            if expectValue:
                self.__dupVal(ltype.pType)

            # emit lefthand expression (the address)
            self.__writeOut(leftExpr)

            # store the data at the addr
            if ltype.pType.getSize() == 2:
                self.__writeOut("STA2\n")
            elif ltype.pType.getSize() == 1:
                self.__writeOut("STA\n")

            # we popped the value AND the pointer
            self.pushed -= ltype.pType.getSize() + ltype.getSize()
        else: # grab the value at the address
            # emit the size of the data on the stack
            self.__writeByteLiteral(ltype.pType.getSize())

            # emit lefthand expression (the address)
            self.__writeOut(leftExpr)

            # load the data from the addr
            if ltype.pType.getSize() == 2:
                self.__writeOut("LDA2\n")
            elif ltype.pType.getSize() == 1:
                self.__writeOut("LDA\n")

            self.pushed += ltype.pType.getSize()

    def __callSub(self, sub: Subroutine, canAssign: bool, expectValue: bool, precLevel: PRECTYPE):
        if not sub.type == DTYPES.SUB:
            self.__error("Expression of type '%s' is not callable!" % sub.name)

        subInstr = self.__popLeftHand() # grab instructions that get the sub address
        for i in range(len(sub.args)):
            arg = sub.args[i]
            exprType = self.__expression()

            if not self.__tryTypeCast(exprType, arg):
                self.__error("Expected expression of type '%s' for parameter #%d, got '%s'!" % (arg.name, i+1, exprType.name))

            self.pushed -= arg.getSize()

            if i < len(sub.args)-1:
                self.__consume(TOKENTYPE.COMMA, "Expected ',' to start argument #d!" % i+2)

        self.__consume(TOKENTYPE.RPAREN, "Expected ')' to end function call!")

        # call subroutine
        self.__pushLeftHand() # push another LeftHand expression
        self.__writeOut("%sJSR2\n" % subInstr)
        self.pushed -= sub.getSize() # absolute address is popped

        # track pushed value on stack
        self.pushed += sub.retType.getSize()

        return sub.retType

    def __ident(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        ident = self.previous

        # check if the identifier exists
        varInfo = self.__findVar(ident.word)
        if not varInfo == None:
            ltype: DataType = None
            lastType = None
            offset = 0

            if varInfo.indx == -3 or varInfo == -2:
                ltype = varInfo.var
            else:
                ltype = varInfo.var.dtype

            while self.__isWalkable(ltype):
                if self.__match(TOKENTYPE.DOT):
                    lastType = ltype
                    self.__consume(TOKENTYPE.IDENT, "Expected member identifier!")
                    memInfo = ltype.searchMembers(self.previous.word)

                    # check member actually exists
                    if memInfo == None:
                        self.__errorAt(self.previous, "Member '%s' not found!" % self.previous.word)

                    offset += memInfo.indx
                    ltype = memInfo.dtype

            # it's a variable, if we *can* assign & there's an EQUAL token, handle assignment
            if canAssign and self.__match(TOKENTYPE.EQUAL):
                rtype = self.__expression()

                # try to typecast by default
                if not self.__tryTypeCast(rtype, ltype):
                    self.__error("Cannot convert '%s' to '%s'!" % (rtype.name, leftType.name))

                # duplicate the value on the stack if it expects a value
                if expectValue:
                    self.__dupVal(ltype)

                # finally, set the variable
                if not lastType == None: # we walked through '.' or '->'
                    if lastType.type == DTYPES.DEV: # it's setting a device
                        self.__setDevice(lastType, memInfo.indx, ltype)
                    else:
                        self.__error("UNIMPL!")
                else:
                    self.__setVar(varInfo)
            else:
                # we ignore expectValue since the user should know better than to write a single expression-statement that computes useless values.

                # finally, get the variable
                if not lastType == None: # we walked through '.' or '->'
                    if lastType.type == DTYPES.DEV: # it's setting a device
                        self.__getDevice(lastType, memInfo.indx, ltype)
                    else:
                        self.__error("UNIMPL!")
                else:
                    self.__getVar(varInfo)

            return ltype
        self.__errorAt(ident, "Unknown identifier!")

    def __group(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        # first, check for a typecast
        dtype = self.__checkDataType()
        if dtype != None:
            # it's a typecast, consume next expression
            self.__consume(TOKENTYPE.RPAREN, "Expected ')' to end open '('!")
            exprType = self.__parsePrecedence(PRECTYPE.ASSIGNMENT, expectValue)

            if not expectValue: # skip typecasting
                return exprType

            # else, try typecasting
            if not self.__tryTypeCast(exprType, dtype):
                self.__error("Couldn't typecast '%s' to '%s'!" % (exprType.name, dtype.name))

            return dtype

        # parse expression
        exprType = self.__parsePrecedence(PRECTYPE.ASSIGNMENT, expectValue)
        self.__consume(TOKENTYPE.RPAREN, "Expected ')' to end open '('!")
        return exprType

    def __parsePrecedence(self, precLevel: PRECTYPE, expectValue: bool) -> DataType:
        self.__advance()

        func = self.__getRule(self.previous).prefix
        if func == None:
            self.__errorAt(self.previous, "Illegal syntax! [prefix]")

        canAssign: bool = precLevel.value <= PRECTYPE.ASSIGNMENT

        self.__pushLeftHand() # encapsulate the prefix & postfix together
        dtype = func(VoidDataType(), canAssign, expectValue, precLevel)
        while precLevel.value <= self.__getRule(self.current).prec.value:
            func = self.__getRule(self.current).infix
            if func == None:
                self.__errorAt(self.current, "Illegal syntax! [infix]")
            self.__advance()
            dtype = func(dtype, canAssign, expectValue, self.__getRule(self.previous).prec)

        self.__writeOut(self.__popLeftHand())
        return dtype

    # ========================= statements =======================

    def __readArgument(self, sub: Subroutine) -> Subroutine:
        # grab datatype
        dtype = self.__matchDataType()

        # grab identifier of argument
        self.__consume(TOKENTYPE.IDENT, "Expected identifier for argument!")
        ident = self.previous

        # add to scope and subroutine
        sub.addArg(dtype)
        self.scopeStack[-1].addVar(Variable(ident.word, dtype))

        return sub

    def __defSub(self, retType: DataType, ident: Token):
        if not self.currentSub == -1:
            self.__error("Cannot define a function here!")

        ident = ident.word
        sub = Subroutine(retType, "SUB_%s" % ident)

        self.__newScope()
        if not self.__check(TOKENTYPE.RPAREN): # arguments are being defined
            sub = self.__readArgument(sub)
            while self.__match(TOKENTYPE.COMMA): # would be nice if python had A FUCKING DO-WHILE UGH
                sub = self.__readArgument(sub)

        self.__consume(TOKENTYPE.RPAREN, "Expected ')' to end argument list!")

        # define our function in our subroutine list
        self.subs.append(Variable(ident, sub))
        self.currentSub = len(self.subs) - 1

        # allocate enough space on the "stack" heap for our passed parameters
        self.__writeIntLiteral(self.scopeStack[-1].getSize())
        self.__writeOut(";alloc-uxncle JSR2\n")

        # in the scope, pop the parameters from the stack and set the arguments
        """
            definition: (int a, int b, int c)
            call: (0, 0x1000, 0xFFFF)

            stack:
            #0000
            #1000
            #FFFF < top

            ^ so, we set the arguments in reverse, eg. pop & set c, pop & set b, pop & set a
        """
        indx = self.scopeStack[-1].getSize()
        for i in range(len(self.scopeStack[-1].vars)):
            var = self.scopeStack[-1].vars[i]
            self.__setVar(VarInfo(var, indx))
            indx -= var.dtype.getSize()

        # parse the subroutine scope
        self.__consume(TOKENTYPE.LBRACE, "Expected '{' to start function body!")
        self.__parseScope(True)
        self.__popScope()

        # reset our currentSub index :)
        self.currentSub = -1
    
    def __defArray(self, dType: DataType, ident: Token):
        # arrays will *always* be defined in our heap. This keeps room in
        # our globals, as well as makes things much simpler lol
        # NOTE: read __consumeArrayType() for some restrictions

        array = self.__consumeArrayType(dType)
        self.__addScopeVar(Variable(ident.word, array))

        # TODO: array initialization = {}

    # returns true if it parsed a function
    def __varTypeState(self, dtype: DataType):
        self.__consume(TOKENTYPE.IDENT, "Expected identifier!")
        ident = self.previous

        if self.__match(TOKENTYPE.LPAREN): # they're declaring a subroutine!
            self.__defSub(dtype, ident)
            return True

        if self.__match(TOKENTYPE.LBRACKET): # they're declaring an array!
            self.__defArray(dtype, ident)
            return False

        varInfo = self.__addScopeVar(Variable(ident.word, dtype))

        if self.__match(TOKENTYPE.EQUAL):
            rtype = self.__expression()

            if not self.__tryTypeCast(rtype, dtype):
                self.__error("Expected expession of type '%s', got '%s'!" % (dtype.name, rtype.name))
            self.__setVar(varInfo)

        return False

    def __parseScope(self, expectBrace: bool):
        while not self.__isEnd() and (not expectBrace or not self.__check(TOKENTYPE.RBRACE)):
            self.__statement()

        if expectBrace:
            self.__consume(TOKENTYPE.RBRACE, "Expected '}' to end scope!")

    def __ifState(self):
        self.__consume(TOKENTYPE.LPAREN, "Expected '('!")
        jmp = self.__newJmpLbl()
        
        # parse the expession
        dtype = self.__expression()

        self.__consume(TOKENTYPE.RPAREN, "Expected ')'!")

        if not self.__tryTypeCast(dtype, BoolDataType()):
            self.__error("Couldn't convert '%s' to 'bool'!" % dtype.name)

        # write comparison jump, if the flag is false, skip the true block
        self.__writeOut("#00 EQU ")
        self.__jmpCondLbl(jmp)

        # now parse the true branch
        self.__statement()

        # there's an else branch!
        if self.__match(TOKENTYPE.ELSE):
            elseJmp = jmp
            jmp = self.__newJmpLbl()
            self.__jmpLbl(jmp) # skip the else block
            self.__declareLbl(elseJmp) # if the condition is false, they jump here (the beginning of the else block!)
            self.__statement()

        # define the label to jump to after the statements
        self.__declareLbl(jmp)

    def __whileState(self):
        self.__consume(TOKENTYPE.LPAREN, "Expected '(' to start conditional expression!")
        jmp = self.__newJmpLbl()
        exitJmp = self.__newJmpLbl()

        # declare the label that will be the start of the loop
        self.__declareLbl(jmp)
        dtype = self.__expression()

        self.__consume(TOKENTYPE.RPAREN, "Expected ')' to end conditional expression!")

        if not self.__tryTypeCast(dtype, BoolDataType()):
            self.__error("Couldn't convert '%s' to 'bool'!" % dtype.name)

        # write comparison jump, if the flag is false, jump out of the loop
        self.__writeOut("#00 EQU ")
        self.__jmpCondLbl(exitJmp)

        # now parse the loop body
        self.__statement()

        # jump back to the start of the loop
        self.__jmpLbl(jmp)
        self.__declareLbl(exitJmp)

    def __deviceState(self):
        """
            device Console[0x18] {
                char character;
                char byte;
                int short;
                char *str;
            };
        """
        self.__consume(TOKENTYPE.IDENT, "Expected identifier for device declaration!")
        ident = self.previous

        self.__consume(TOKENTYPE.LBRACKET, "Expected '[' for start of zeropage address of device!")
        self.__consume(TOKENTYPE.NUM, "Expected zeropage address for start of device!")
        addr = self.__grabNumber(self.previous)
        self.__consume(TOKENTYPE.RBRACKET, "Expected ']' for end of zeropage address of device!")
        self.__consume(TOKENTYPE.LBRACE, "Expected '{' to start member list!")

        dev = Device(ident.word, addr)

        # consume members
        while not self.__match(TOKENTYPE.RBRACE):
            dtype = self.__matchDataType() # grab datatype
            self.__consume(TOKENTYPE.IDENT, "Expected identifier!")
            dev.addMember(Variable(self.previous.word, dtype))
            self.__consume(TOKENTYPE.SEMICOLON, "Expected ';' to end member declaration!")

        self.devices.append(dev)

    def __returnState(self):
        if self.currentSub == -1: # we're not currently parsing a function!
            self.error("'return' not allowed in this context!")

        retType: DataType = self.subs[self.currentSub].dtype.retType

        if self.__check(TOKENTYPE.SEMICOLON): # there's no expression
            if retType.compare(VoidDataType): # make sure we can actually return nothing
                self.__popRawScopes(len(self.scopeStack))
                self.__writeOut("JMP2r\n")
                return
            else:
                self.__error("Expected expression of type '%s', got 'void'!" % retType.name)

        dtype = self.__expression()

        if not self.__tryTypeCast(dtype, retType):
            self.__error("Cannot convert expression of type '%s' to return type of '%s'" % (dtype.name, retType.name))

        self.__popRawScopes(len(self.scopeStack))
        self.__writeOut("JMP2r\n")

        # we don't need to keep track of that value anymore
        self.pushed -= retType.getSize()

    def __expression(self) -> DataType:
        return self.__parsePrecedence(PRECTYPE.ASSIGNMENT, True)

    def __voidExpression(self):
        self.__parsePrecedence(PRECTYPE.ASSIGNMENT, False)

    def __statement(self):
        ttype = self.current.type
        currentPushed = self.pushed

        dtype = self.__checkDataType()
        if not dtype == None: # is it a variable definition?
            if self.__varTypeState(dtype): # if we parsed a function, we don't expect a ';'
                return
        elif self.__match(TOKENTYPE.IF): # we don't expect a ';', and the stack *should* already be balanced (???) so these statements return immediately
            self.__ifState()
            return
        elif self.__match(TOKENTYPE.WHILE): # we don't expect a ';'
            self.__whileState()
            return
        elif self.__match(TOKENTYPE.RETURN):
            self.__returnState()
        elif self.__match(TOKENTYPE.DEVICE):
            self.__deviceState()
        elif self.__match(TOKENTYPE.LBRACE):
            self.__newScope()
            self.__parseScope(True)
            self.__popScope()
            return
        else: # it's not a statement, parse it as an expression (with no value expected!)
            self.__voidExpression()

        self.__pop(self.pushed - currentPushed)
        self.__consume(TOKENTYPE.SEMICOLON, "Expected ';' to mark end of statement!")

    def parse(self):
        self.__advance()

        self.__newScope() # no 'true' variables will be allocated here. just allows heap allocation for arrays and such
        # parse until the end of the file
        self.__parseScope(False)
        self.__popScope()

        # write the license header
        self.out.write(thinlib._LICENSE)

        # declare devices
        for dev in self.devices:
            self.out.write("|%.2x @%s [ &padd $%d ]\n" % (dev.addr, dev.devname, dev.getSize()))

        # write the globals into the zero area
        self.out.write("|0000\n")
        self.out.write(thinlib._MEMDEFS)

        # write entrypoint
        self.out.write("|0100\n")
        self.out.write(thinlib._MEMENTRY)
        self.out.write(self.entryInstr)
        self.out.write("BRK\n\n")

        # now write all globals
        self.out.write("@globals [ ")
        for var in self.globals:
            self.out.write("&%s $%d " % (var.name, var.dtype.getSize()))
        self.out.write("]\n\n")

        # TODO: write subroutines
        for sub in self.subs:
            self.out.write("@%s\n" % sub.dtype.subname)
            self.out.write(sub.dtype.instrs)
            self.out.write("JMP2r\n\n")

        # write memlib
        self.out.write(thinlib._MEMLIB)
