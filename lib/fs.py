from lib.execute import call, do_when, swallow
from os.path import expanduser, dirname, isfile, abspath

MERGE_TOOL = "meld"

def readlink(lnk):
    if not isfile(lnk):
        return None
    r = swallow(["readlink", "-f", lnk])
    return None if r.failed else r.out.decode("utf8").strip()

def is_symlink_to(lnk, real):
    l = readlink(lnk)
    return False if l is None else l == real

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

def symlink(file, link, sudo=False):
    file, link = map(lambda p: abspath(expanduser(p)), [file, link])
    def cmd():
        l = readlink(file)
        if l:
            raise Exception("It's a symlink to somewhere else")
        call(["ln", "-s", file, link], sudo=sudo)
    do_when(cmd, lambda: not is_symlink_to(link, file))()
