device Console[0x10] {
    char pad[8]; /* these first few bytes contain a vector and other reserved bytes that we aren't using :P */
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

int arr[10];
int i = 0;

while (i < 10) {
    arr[i] = i;
    i = i + 1;
}

while (i > 0) {
    i = i - 1;
    printInt(arr[i]);
}