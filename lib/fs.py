from lib.execute import call, do_when, swallow
from os.path import expanduser

MERGE_TOOL = "meld"

def files_are_not_equal(src, dest):
    return swallow(["diff", src, dest]).failed

def merge(src, dest):
    src, dest = map(expanduser, [src, dest])
    do_when(lambda: call([MERGE_TOOL, src, dest]),
            lambda: files_are_not_equal(src, dest))()
