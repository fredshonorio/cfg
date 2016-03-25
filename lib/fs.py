from lib.execute import call, do_when, swallow
from os.path import expanduser, dirname, isfile

MERGE_TOOL = "meld"

def files_are_not_equal(src, dest):
    return swallow(["diff", src, dest]).failed

def merge(src, dest, mkdir=False):
    src, dest = map(expanduser, [src, dest])
    def cmd():
        if mkdir:
            call(["mkdir", "-p", dirname(dest)])
        if not isfile(dest):
            call(["touch", dest])
        call([MERGE_TOOL, src, dest])
    do_when(cmd, lambda: files_are_not_equal(src, dest))()
