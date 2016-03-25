
from lib.execute import run, do_when, swallow
from os.path import expanduser

MERGE_TOOL = "meld"

def files_are_not_equal(src, dest):
    return swallow(["diff", src, dest]).failed

def merge(_src, _dest):
    src, dest = map(expanduser, [_src, _dest])
    do_when(
        lambda: run([MERGE_TOOL, src, dest]),
        lambda: files_are_not_equal(src, dest)
    )()
