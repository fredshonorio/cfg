
from subprocess import check_output, STDOUT, run, PIPE
from collections import namedtuple
from sys import exit

Result = namedtuple("Result", ["out", "err", "failed", "ret"])

def swallow(cmd, sudo=False):
    _cmd = ["sudo"] + cmd if sudo else cmd
    r = run(_cmd, stdout=PIPE, stderr=PIPE)
    return Result(out=r.stdout, err=r.stderr, failed=r.returncode != 0, ret=r.returncode)

def call(cmd, sudo=False):
    r = swallow(cmd, sudo)
    if r.failed:
        print("Command %s failed" % " ".join(cmd))
        print((r.err or r.out).decode("utf8"))
        exit(-1)

def run_all(cmds):
    for c in cmds:
        c()

def die(msg):
    _msg = msg() if hasattr(msg, "__call__") else msg
    print(_msg)
    exit(-1)

def execute(run, when, fail=None, msg=None):
    def f():
        if when == True or when():
            if fail and fail():
                die(msg)
            run()
    return f
