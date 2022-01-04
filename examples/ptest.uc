device Console[0x18] {
    char write;
};

int i;
int a = 2;

void print(int x) {
    Console.write = '0' + x;
    Console.write = 0x0A;
}

print(((int*)(&i))[1]);