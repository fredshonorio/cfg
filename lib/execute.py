
from subprocess import check_call, check_output, STDOUT, run, PIPE
from collections import namedtuple
from sys import exit

Result = namedtuple("Result", ["out", "err", "failed", "ret"])

call = check_call

def call(cmd, sudo=False):
    r = swallow(cmd, sudo)
    if r.failed:
        print("Command %s failed" % " ".join(cmd))
        print((r.err or r.out).decode("utf8"))
        exit(-1)

def swallow(cmd, sudo=False):
    _cmd = ["sudo"] + cmd if sudo else cmd

    r = run(_cmd, stdout=PIPE, stderr=PIPE)
    return Result(out=r.stdout, err=r.stderr, failed=r.returncode != 0, ret=r.returncode)

def run_all(cmds):
    for c in cmds:
        c()

def do_when(cmd, condition):
    def f():
        if condition == True or condition():
            cmd()
    return f

def die(msg):
    print(msg)
    exit(-1)

def do_when_except(exc, condition, fail_condition, fail_msg):
    def f():
        if condition == True or condition():
            if fail_condition == True or fail_condition():
                die(fail_msg)
            exc()
    return f
