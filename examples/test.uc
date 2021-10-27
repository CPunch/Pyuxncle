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

print total;
print fact(5);