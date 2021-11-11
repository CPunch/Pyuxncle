device Console[0x18] {
    char character;
    char byte;
    int short;
    char *str;
};

int i;
int a = 2;

void print(int x) {
    Console.short = x;
    Console.character = 0x20;
}

print(((int*)(&i))[1]);