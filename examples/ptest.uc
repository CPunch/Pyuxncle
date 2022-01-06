device Console[0x10] {
    char pad[8]; /* these first few bytes contain a vector and other reserved bytes that we aren't using :P */
    char write;
};

int i;
int a = 2;

void print(int x) {
    Console.write = '0' + x;
    Console.write = 0x0A;
}

print(((int*)(&i))[1]);