device Console[0x10] {
    char pad[8];
    char write;
};

void printInt(int x) {
    char rem; /* rem is a byte since the remainder shouldn't ever be above 10 */
    char len = 0;
    char str[5];

    if (x == 0) {
        str[0] = '0';
        len = 1;
    }

    while (x > 0) {
        rem = x % 10;
        str[len] = '0' + rem;
        x = x / 10;
        len = len + 1;
    }

    while (len > 0) {
        len = len - 1;
        Console.write = str[len];
    }

    /* write newline character */
    Console.write = 0x0A;
}

/* basic factorial example using recursion :D */
int fact(int i) {
    if (i > 1)
        return i * fact(i-1);

    return 1;
}

int total = 0;
int x = 10;

while (x >= 1) {
    total = total + x;
    x = x - 1;
}

printInt(total);
printInt(fact(8));