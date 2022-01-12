device System[0x08] {
    int r;
    int g;
    int b;
};

device Screen[0x20] {
    void *vec;
    int width;
    int height;
    int padd; /* unused */
    int x;
    int y;
    char *addr; /* address to pull sprite data from */
    char pixel;
    char sprite;
};

int x = 0x008;
int y = 0x008;

System.r = 0x2ce9;
System.g = 0x01c0;
System.b = 0x2ce5;
Screen.y = y;

/* draw a line across the top of the screen */
while (x < Screen.width - 8) {
    Screen.x = x;
    Screen.pixel = 0x41;
    x = x + 1;
}

/* draw a line down the right side of the screen */
while (y < Screen.height - 8) {
    Screen.y = y;
    Screen.pixel = 0x41;
    y = y + 1;
}

/* draw a line across the bottom of the screen */
while (x > 8) {
    Screen.x = x;
    Screen.pixel = 0x41;
    x = x - 1;
}

/* draw a line across the left side of the screen */
while (y > 8) {
    Screen.y = y;
    Screen.pixel = 0x41;
    y = y - 1;
}