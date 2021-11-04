device Console[0x18] {
    char character;
    char byte;
    int short;
    char *str;
};

int total = 0;
int x = 10;

while (x >= 1) {
    total = total + x;
    x = x - 1;
}

int fact(int i) {
    if (i > 1)
        return i * fact(i-1);

    return 1;
}

void print(int x) {
    Console.short = x;
    Console.character = 0x20;
}

print(total);
print(fact(5));