# Pyuxncle

Pyuxncle is a single-pass compiler for a small subset of C (albeit without the std library). This compiler targets Uxntal, the assembly language of the Uxn virtual computer. The output Uxntal is not meant to be human readable, only to be directly passed to uxnasm.

## Usage

To compile a source file, pass it like so:

```sh
./src/pyuxncle [SRC_FILE] [OUT_UXNTAL_FILE]
```

## Examples


```c
device Console[0x18] {
    char character;
    char byte;
    int short;
    char *str;
};

void print(int x) {
    Console.short = x;
    Console.character = 0x20;
}

print(2 * 8);
```
> 16