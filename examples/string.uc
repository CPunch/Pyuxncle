device Console[0x10] {
    char pad[8]; /* these first few bytes contain a vector and other reserved bytes that we aren't using :P */
    char write;
};

char *str = "Hello world!";
char i = 0;

while (i < 12) {
    Console.write = str[i];
    i = i + 1;
}