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
    char write;
};

Console.write = 'H';
Console.write = 'i';
Console.write = '!';
Console.write = 0x0A;

```
> Hi!