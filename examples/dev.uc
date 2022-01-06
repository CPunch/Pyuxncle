device Console[0x10] {
    char pad[9];
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
        Console.pad[8] = str[len];
    }

    /* write newline character */
    Console.pad[8] = 0x0A;
}

printInt(2 * 4 + 3); // should print 11?
