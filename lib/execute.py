
from subprocess import check_call, check_output, STDOUT, run, PIPE
from collections import namedtuple

Result = namedtuple("Result", ["out", "err", "failed", "ret"])

call = check_call
capture = check_output

def swallow(*args):
    r = run(*args, stdout=PIPE, stderr=PIPE)
    return Result(out=r.stdout, err=r.stderr, failed=r.returncode != 0, ret=r.returncode)

def run_all(cmds):
    for c in cmds:
        c()

def do_when(cmd, condition):
    def f():
        if condition == True or condition():
            cmd()
    return f
