from enum import Enum, auto
import string

class TOKENTYPE(Enum):
    SEMICOLON = auto() # ;
    COMMA = auto() # ,
    LPAREN = auto() # (
    RPAREN = auto() # )
    LBRACE = auto() # {
    RBRACE = auto() # }
    LBRACKET = auto() # [
    RBRACKET = auto() # ]
    PLUS = auto() # +
    MINUS = auto() # -
    STAR = auto() # *
    SLASH = auto() # /
    EQUAL = auto() # =
    EQUALEQUAL = auto() # ==
    GRTREQL = auto() # >=
    GRTR = auto() # >
    LESSEQL = auto() # <=
    LESS = auto() # <
    IF = auto() # if
    ELSE = auto() # else
    WHILE = auto() # while
    RETURN = auto() # return
    IDENT = auto() # Identifier_123
    NUM = auto() # 1234567890
    INT = auto() # int
    BOOL = auto() # bool
    VOID = auto() # void
    EOF = auto() # end of file
    ERR = auto() # tokenizer error
    PRINT = auto() # TEMPORARY TOKEN TO TEST EXPRESSIONS

class Token:
    def __init__(self, type: TOKENTYPE, word: str, line: int):
        self.type = type
        self.word = word
        self.line = line
    
    def print(self):
        print("\'" + self.word + "\' : [TOKEN_" + self.type.name + "]")

_ReservedWordTable = {
    "if"    : TOKENTYPE.IF,
    "else"  : TOKENTYPE.ELSE,
    "while" : TOKENTYPE.WHILE,
    "return": TOKENTYPE.RETURN,
    "int"   : TOKENTYPE.INT,
    "bool"  : TOKENTYPE.BOOL,
    "void"  : TOKENTYPE.VOID,

    "print" : TOKENTYPE.PRINT
}

def _isWhitespace(c):
    return c in string.whitespace

def _isNum(c):
    return c in string.digits

def _isAlpha(c):
    return c in (string.ascii_letters + "_")

class Lexer:
    def __init__(self, source: str):
        self.src = source
        self.size = len(source)
        self.cursor = 0
        self.start = 0
        self.line = 1

    # peeks the current character
    def __peek(self):
        if self.cursor < self.size:
            return self.src[self.cursor]
        return '\0'

    # grabs the current character and increments the cursor
    def __next(self):
        if self.cursor < self.size:
            self.cursor += 1
            return self.src[self.cursor-1]
        return '\0'

    # returns true & consumes character if next
    def __checkNext(self, char):
        if self.__peek() == char:
            self.__next()
            return True
        return False

    def __getWord(self):
        return self.src[self.start:self.cursor]

    def __makeToken(self, type: TOKENTYPE):
        return Token(type, self.__getWord(), self.line)

    def __skipWhitespace(self):
        while (_isWhitespace(self.__peek())):
            if self.__peek() == '\n': # on newlines, count the line!
                self.line += 1
            self.__next()

    def __readIdentifier(self):
        while (_isAlpha(self.__peek()) or _isNum(self.__peek())):
            self.__next()

        # check if identifier is a reserved word
        if self.__getWord() in _ReservedWordTable:
            return self.__makeToken(_ReservedWordTable[self.__getWord()])

        return self.__makeToken(TOKENTYPE.IDENT)
    
    def __readNumber(self):
        while (_isNum(self.__peek())):
            self.__next()

        return self.__makeToken(TOKENTYPE.NUM)

    # grabs the next token (skipping whitespace)
    def scanNext(self) -> Token:
        # skip all uninteresting characters
        self.__skipWhitespace();

        self.start = self.cursor
        c = self.__next()

        # python you ugly bastard, give us syntaxical sugar and add switch statements already
        charLookup = {
            ';' : (lambda : self.__makeToken(TOKENTYPE.SEMICOLON)),
            ',' : (lambda : self.__makeToken(TOKENTYPE.COMMA)),
            '(' : (lambda : self.__makeToken(TOKENTYPE.LPAREN)),
            ')' : (lambda : self.__makeToken(TOKENTYPE.RPAREN)),
            '{' : (lambda : self.__makeToken(TOKENTYPE.LBRACE)),
            '}' : (lambda : self.__makeToken(TOKENTYPE.RBRACE)),
            '[' : (lambda : self.__makeToken(TOKENTYPE.LBRACKET)),
            ']' : (lambda : self.__makeToken(TOKENTYPE.RBRACKET)),
            '+' : (lambda : self.__makeToken(TOKENTYPE.PLUS)),
            '-' : (lambda : self.__makeToken(TOKENTYPE.MINUS)),
            '*' : (lambda : self.__makeToken(TOKENTYPE.STAR)),
            '/' : (lambda : self.__makeToken(TOKENTYPE.SLASH)),
            '=' : (lambda : self.__makeToken(TOKENTYPE.EQUALEQUAL) if self.__checkNext('=') else self.__makeToken(TOKENTYPE.EQUAL)),
            '>' : (lambda : self.__makeToken(TOKENTYPE.GRTEQL) if self.__checkNext('=') else self.__makeToken(TOKENTYPE.GRTR)),
            '<' : (lambda : self.__makeToken(TOKENTYPE.LESSEQL) if self.__checkNext('=') else self.__makeToken(TOKENTYPE.LESS)),
            '\0': (lambda : self.__makeToken(TOKENTYPE.EOF)),
        }

        # if it's a simple token, return it.
        if c in charLookup:
            return charLookup[c]()

        # otherwise it's an identifier or number
        if _isAlpha(c):
            return self.__readIdentifier()
        elif _isNum(c):
            return self.__readNumber()
        else:
            return self.__makeToken(TOKENTYPE.ERR)

