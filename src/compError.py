import traceback

def doCompError(err: str):
    #for line in traceback.format_stack():
    #    print(line.strip())
    print(err)
    exit(0)