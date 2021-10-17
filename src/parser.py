from enum import IntEnum, auto
from collections.abc import Callable
from tokenizer import Lexer, TOKENTYPE, Token
from type import *
import thinlib

class PRECTYPE(IntEnum):
    NONE = 0,
    ASSIGNMENT = 1, # =
    COMPAR = 2,     # == !=
    TERM = 3,       # + - 
    FACTOR = 4,     # * /
    CALL = 5,       # ( )
    PRIMARY = 6

class _PrecRule:
    def __init__(self, prec: PRECTYPE, prefix: Callable[[DataType, bool, bool, PRECTYPE], DataType],
                                        infix: Callable[[DataType, bool, bool, PRECTYPE], DataType]):
        self.prec = prec
        self.prefix = prefix
        self.infix = infix

class _Variable:
    def __init__(self, name: str, dtype: DataType):
        self.name = name
        self.dtype = dtype

class _Constant:
    def __init__(self, id: int, data: str):
        self.id = id
        self.data = data

# variable info, including the variable (name & datatype) and the index in the stack
class _VarInfo:
    def __init__(self, var: _Variable, indx: int): # indx of -1 means a global, -2 means subroutine
        self.var = var
        self.indx = indx

class _Scope:
    def __init__(self):
        # array of declared variables (and their respective locations in the stack)
        self.vars: list[_Variable] = []
        self.allocated = 0
        self.instrs = ''

    def addVar(self, var: _Variable):
        self.vars.append(var)

    def getSize(self):
        totalSz = 0
        for var in self.vars:
            totalSz += var.dtype.getSize()

        return totalSz

class Parser:
    def __init__(self, source: str):
        self.lexer = Lexer(source)
        self.current: Token = None
        self.previous: Token = None
        self.out = open("out.tal", "w")
        self.scopeStack: list[_Scope] = []
        self.pushed = 0
        self.jmpIDS = 0
        self.parseTable = {
            # TOKEN :               _PrecRule(Precedence,       prefix,         infix)
            TOKENTYPE.SEMICOLON:    _PrecRule(PRECTYPE.NONE,    None,           None),
            TOKENTYPE.PLUS:         _PrecRule(PRECTYPE.TERM,    None,           self.__binOp),
            TOKENTYPE.MINUS:        _PrecRule(PRECTYPE.TERM,    None,           self.__binOp),
            TOKENTYPE.STAR:         _PrecRule(PRECTYPE.FACTOR,  None,           self.__binOp),
            TOKENTYPE.SLASH:        _PrecRule(PRECTYPE.FACTOR,  None,           self.__binOp),
            TOKENTYPE.EQUALEQUAL:   _PrecRule(PRECTYPE.COMPAR,  None,           self.__binOp),

            TOKENTYPE.LPAREN:       _PrecRule(PRECTYPE.CALL,    self.__group,   self.__callSub),
            TOKENTYPE.RPAREN:       _PrecRule(PRECTYPE.NONE,    None,           None),

            TOKENTYPE.IDENT:        _PrecRule(PRECTYPE.NONE,    self.__ident,   None),
            TOKENTYPE.NUM:          _PrecRule(PRECTYPE.NONE,    self.__number,  None),
            TOKENTYPE.EOF:          _PrecRule(PRECTYPE.NONE,    None,           None)
        }

        self.typeTable = {
            TOKENTYPE.INT : IntDataType(),
            TOKENTYPE.BOOL : BoolDataType(),
            TOKENTYPE.VOID : VoidDataType()
        }

        # compiler-related stuff (in the rewrite, this should be given to another module that takes care of building the output uxntal)
        self.entryInstr = ""
        self.currentSub: int = -1
        self.subs: list[_Variable] = []
        self.globals: list[_Variable] = []
        self.constants: list[_Constant] = [] # holds data like strings, constant arrays, etc. [TODO]

    def __errorAt(self, tkn: Token, err: str):
        print("At '%s' on line %d:\n\t%s" % (tkn.word, tkn.line, err))
        exit(1)

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

    # returns true if the parser reached the end of the token list or reached a tokenizer error
    def __isEnd(self):
        return self.__check(TOKENTYPE.EOF) or self.__check(TOKENTYPE.ERR)

    # =============== formatting for uxntal output ===============

    def __writeOut(self, buf: str):
        if self.currentSub == -1: # we're not currently parsing a function, write it to the entry instructions
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
    # maybe hold the index of the intliteral to patch int __popScope? the returned _VarInfo is valid until __addScopeVar
    # or __popScope is called again
    def __addScopeVar(self, var: _Variable) -> _VarInfo:
        self.scopeStack[-1].addVar(var) # add the variable to the current scope

        # if we're not parsing a function, define the variable as a global
        if self.currentSub == -1:
            self.globals.append(var)
            return _VarInfo(var, -1)

        self.__writeIntLiteral(var.dtype.getSize())
        self.__writeOut(";alloc-uxncle JSR2\n")
        return _VarInfo(var, var.dtype.getSize())

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
        if dtype.type == DTYPES.INT:
            self.__writeOut("DUP2\n")

        self.pushed += dtype.getSize()

    # searches for the variable in the scope stack, returns the index needed to be passed to ;poke-uxncle-* if var isn't found None is returned
    def __findVar(self, name: str) -> _VarInfo:
        indx = 0

        # walk each scope (from top)
        for i in range(len(self.scopeStack)):
            scope = self.scopeStack[(len(self.scopeStack)-1)-i]
            # walk each variable in the scope (from top)
            for var in reversed(scope.vars):
                indx += var.dtype.getSize()

                # if the variable is the one we're looking for, return the index of the variable in our heap
                if var.name == name:
                    return _VarInfo(var, indx)

        # variable wasn't found, check for it in our global array?
        for var in self.globals:
            if var.name == name:
                return _VarInfo(var, -1) # found it! return as a global

        # global wasn't found, maybe its a function
        for sub in self.subs:
            if sub.name == name:
                return _VarInfo(sub, -2)

        return None

    # reads variable from heap & pushes to stack
    def __getVar(self, varInfo: _VarInfo):
        dtype = varInfo.var.dtype
        indx = 0

        # is it a global?
        if varInfo.indx == -1:
            # read global 2 bytes at a time
            while indx < dtype.getSize() - 1:
                if indx == 0:
                    self.__writeOut(".globals/%s LDZ2\n" % varInfo.var.name)
                else:
                    self.__writeOut(".globals/%s " % varInfo.var.name)
                    self.__writeByteLiteral(indx) # add offset to read from
                    self.__writeOut("ADD LDZ2")
                indx += 2

            # read final byte (if needed)
            if indx == dtype.getSize() - 1:
                if indx == 0:
                    self.__writeOut(".globals/%s LDZ\n" % varInfo.var.name)
                else:
                    self.__writeOut(".globals/%s " % varInfo.var.name)
                    self.__writeByteLiteral(indx) # add offset to read from
                    self.__writeOut("ADD LDZ")
        else: # it's a normal var stored in the heap
            # read variable from the heap 2 bytes at a time
            while indx < dtype.getSize() - 1:
                self.__writeIntLiteral(varInfo.indx + indx)
                self.__writeOut(";peek-uxncle-short JSR2\n")
                indx += 2

            # read final byte (if needed)
            if indx == dtype.getSize() - 1:
                self.__writeIntLiteral(varInfo.indx + indx)
                self.__writeOut(";peek-uxncle JSR2\n")

        # peek-uxncle pushes the value from the heap to the stack
        self.pushed += dtype.getSize()

    # pops value from stack and writes to heap
    def __setVar(self, varInfo: _VarInfo):
        dtype = varInfo.var.dtype
        indx = 0

        # is it a global?
        if varInfo.indx == -1:
            # read global 2 bytes at a time
            while indx < dtype.getSize() - 1:
                if indx == 0:
                    self.__writeOut(".globals/%s STZ2\n" % varInfo.var.name)
                else:
                    self.__writeOut(".globals/%s " % varInfo.var.name)
                    self.__writeByteLiteral(indx) # add offset to write to
                    self.__writeOut("ADD STZ2")
                indx += 2

            # read final byte (if needed)
            if indx == dtype.getSize() - 1:
                if indx == 0:
                    self.__writeOut(".globals/%s STZ\n" % varInfo.var.name)
                else:
                    self.__writeOut(".globals/%s " % varInfo.var.name)
                    self.__writeByteLiteral(indx) # add offset to write to
                    self.__writeOut("ADD STZ")
        else: # it's a normal var stored in the heap
            # read variable from the heap 2 bytes at a time
            while indx < dtype.getSize() - 1:
                self.__writeIntLiteral(varInfo.indx + indx)
                self.__writeOut(";poke-uxncle-short JSR2\n")
                indx += 2

            # read final byte (if needed)
            if indx == dtype.getSize() - 1:
                self.__writeIntLiteral(varInfo.indx + indx)
                self.__writeOut(";poke-uxncle JSR2\n")

        # poke-uxncle pops the value from the stack
        self.pushed -= dtype.getSize()

    # expects fromType to already be on the stack
    def __tryTypeCast(self, fromType: DataType, toType: DataType) -> bool:
        # if the types are the same, do nothing
        if fromType == None or toType == None or fromType.compare(toType):
            return True

        self.pushed += toType.getSize() - fromType.getSize()

        # convert INT to BOOL
        if fromType.type == DTYPES.INT and toType.type == DTYPES.BOOL:
            self.__writeOut("SWP POP ") # pops the most significant byte
            return True
        # convert BOOL to INT
        elif fromType.type == DTYPES.BOOL and toType.type == DTYPES.INT:
            self.__writeOut("#00 SWP ")
            return True
        # convert subroutine to INT, (push the absolute address !)
        elif fromType.type == DTYPES.SUB and toType.type == DTYPES.INT:
            self.__writeOut(";%s " % fromType.subname)
            return True

        return False

    # ======================== parse rules =======================

    def __getRule(self, tkn: TOKENTYPE) -> _PrecRule:
        if tkn.type not in self.parseTable:
            self.__errorAt(tkn, "Unknown Token")

        return self.parseTable[tkn.type]

    # parses number literal
    def __number(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        # the expression expects nothing, return nothing (void)
        if not expectValue:
            return VoidDataType()

        self.__writeIntLiteral(int(self.previous.word))
        self.pushed += 2
        return IntDataType()

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
            self.__writeBinaryOp("ADD", rtype, rtype)
        elif tkn.type == TOKENTYPE.MINUS:
            self.__writeBinaryOp("SUB", rtype, rtype)
        elif tkn.type == TOKENTYPE.STAR:
            self.__writeBinaryOp("MUL", rtype, rtype)
        elif tkn.type == TOKENTYPE.SLASH:
            self.__writeBinaryOp("DIV", rtype, rtype)
        elif tkn.type == TOKENTYPE.EQUALEQUAL:
            self.__writeBinaryOp("EQU", rtype, BoolDataType())
            rtype = BoolDataType()
        else: # should never happen
            self.__errorAt(tkn, "Invalid binary operator token!")

        return rtype

    def __callSub(self, sub: Subroutine, canAssign: bool, expectValue: bool, precLevel: PRECTYPE):
        if not sub.type == DTYPES.SUB:
            self.__error("Expression of type '%s' is not callable!" % sub.name)

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
        self.__writeOut(";%s JSR2\n" % sub.subname)

        # track pushed value on stack
        self.pushed += sub.retType.getSize()

        return sub.retType

    def __ident(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        ident = self.previous

        # check if the identifier is a variable
        varInfo = self.__findVar(ident.word)
        if not varInfo == None:
            ltype = varInfo.var.dtype
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
                self.__setVar(varInfo)
            else:
                if not expectValue and varInfo.var.dtype.type != DTYPES.SUB: # sanity check
                    return VoidDataType()

                self.__getVar(varInfo)

            return ltype
        self.__errorAt(ident, "Unknown identifier!")

    def __group(self, leftType: DataType, canAssign: bool, expectValue: bool, precLevel: PRECTYPE) -> DataType:
        # parse expression
        dtype = self.__parsePrecedence(PRECTYPE.ASSIGNMENT, expectValue)
        self.__consume(TOKENTYPE.RPAREN, "Expected ')' to end open '('!")
        return dtype

    def __parsePrecedence(self, precLevel: PRECTYPE, expectValue: bool) -> DataType:
        self.__advance()

        func = self.__getRule(self.previous).prefix
        if func == None:
            self.__errorAt(self.previous, "Illegal syntax! [prefix]")

        canAssign: bool = precLevel.value <= PRECTYPE.ASSIGNMENT

        dtype = func(VoidDataType(), canAssign, expectValue, precLevel)
        while precLevel.value <= self.__getRule(self.current).prec.value:
            func = self.__getRule(self.current).infix
            if func == None:
                self.__errorAt(self.current, "Illegal syntax! [infix]")
            self.__advance()
            dtype = func(dtype, canAssign, expectValue, self.__getRule(self.previous).prec)

        return dtype

    # ========================= statements =======================

    def __readArgument(self, sub: Subroutine) -> Subroutine:
        if self.current.type in self.typeTable:
            dtype = self.typeTable[self.current.type]
            self.__advance()

            # grab identifier of argument
            self.__consume(TOKENTYPE.IDENT, "Expected identifier for argument!")
            ident = self.previous

            # add to scope and subroutine
            sub.addArg(dtype)
            self.scopeStack[-1].addVar(_Variable(ident.word, dtype))
        else:
            self.__errorAt(self.current, "Expected a datatype!")

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
        self.subs.append(_Variable(ident, sub))
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
            self.__setVar(_VarInfo(var, indx))
            indx -= var.dtype.getSize()

        # parse the subroutine scope
        self.__consume(TOKENTYPE.LBRACE, "Expected '{' to start function body!")
        self.__parseScope(True)
        self.__popScope()

        # reset our currentSub index :)
        self.currentSub = -1

    # returns true if it parsed a function
    def __varTypeState(self, dtype: DataType):
        self.__consume(TOKENTYPE.IDENT, "Expected identifier!")

        ident = self.previous

        if self.__match(TOKENTYPE.LPAREN): # they're declaring a subroutine!
            self.__defSub(dtype, ident)
            return True

        varInfo = self.__addScopeVar(_Variable(ident.word, dtype))

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

        # write comparison jump, if the flag is not equal to true, skip the true block
        self.__writeOut("#01 NEQ ")
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

        if ttype in self.typeTable: # is it a variable definition?
            self.__advance()
            if self.__varTypeState(self.typeTable[ttype]): # if we parsed a function, we don't expect a ';'
                return
        elif self.__match(TOKENTYPE.IF): # we don't expect a ';', and the stack *should* already be balanced (???) so these statements return immediately
            self.__ifState()
            return
        elif self.__match(TOKENTYPE.RETURN):
            self.__returnState()
        elif self.__match(TOKENTYPE.PRINT):
            rtype = self.__expression()
            if not self.__tryTypeCast(rtype, IntDataType()):
                self.__error("'print' only accepts integer expressions!")
            self.__writeOut(";print-decimal JSR2 #20 .Console/char DEO\n")
            self.pushed -= 2
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

        # parse until the end of the file
        self.__parseScope(False)

        # write the license header
        self.out.write(thinlib._LICENSE)

        # write the globals into the zero area
        self.out.write("|0000\n")
        self.out.write(thinlib._MEMDEFS)

        # now write all globals
        self.out.write("@globals [ ")
        for var in self.globals:
            self.out.write("&%s $%d " % (var.name, var.dtype.getSize()))
        self.out.write("]\n\n")

        # write entrypoint
        self.out.write("|0100\n")
        self.out.write(thinlib._MEMENTRY)
        self.out.write(self.entryInstr)
        self.out.write("BRK\n\n")

        # TODO: write subroutines
        for sub in self.subs:
            self.out.write("@%s\n" % sub.dtype.subname)
            self.out.write(sub.dtype.instrs)
            self.out.write("JMP2r\n\n")

        # write memlib
        self.out.write(thinlib._MEMLIB)
