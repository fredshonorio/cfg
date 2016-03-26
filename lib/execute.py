
from subprocess import check_call, run, PIPE
from collections import namedtuple
from sys import exit

Result = namedtuple("Result", ["out", "err", "failed", "ret"])

# TODO: clean this up
# - There are 3 ways to run commands, do we need them?
# - A lot of the commands are deferred in the call site, provide deferred versions?

def sudoize(cmd, sudo=False):
    return ["sudo"] + cmd if sudo else cmd

def swallow(cmd, sudo=False):
    r = run(sudoize(cmd, sudo), stdout=PIPE, stderr=PIPE)
    return Result(out=r.stdout, err=r.stderr, failed=r.returncode != 0, ret=r.returncode)

def call(cmd, sudo=False):
    r = swallow(cmd, sudo)
    if r.failed:
        print("Command %s failed" % " ".join(cmd))
        print((r.err or r.out).decode("utf8"))
        exit(-1)

def call_show(cmd, sudo=False):
    try:
        check_call(sudoize(cmd, sudo))
    except:
        print("Command %s failed" % " ".join(cmd))
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
