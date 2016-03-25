from lib.execute import call, execute, swallow
from os.path import expanduser, dirname, isfile, abspath

MERGE_TOOL = "meld"

def readlink(lnk, sudo=False):
    if not isfile(lnk):
        return None
    r = swallow(["readlink", "-f", lnk], sudo=sudo)
    return None if r.failed else r.out.decode("utf8").strip()

def is_symlink_to(lnk, real):
    l = readlink(lnk)
    return False if l is None else l == real

def files_are_not_equal(src, dest):
    return swallow(["diff", src, dest]).failed

def merge(src, dest, mkdir=False):
    """Compares two files, unless they are already equal"""

    src, dest = map(expanduser, [src, dest])
    def cmd():
        if mkdir:
            call(["mkdir", "-p", dirname(dest)])
        if not isfile(dest):
            call(["touch", dest])
        call([MERGE_TOOL, src, dest])

    execute(cmd,
            when=lambda: files_are_not_equal(src, dest))()

def symlink(file, link, sudo=False):
    file, link = map(lambda p: abspath(expanduser(p)), [file, link])
    execute(lambda: call(["ln", "-s", file, link], sudo=sudo),
            when=lambda: not is_symlink_to(link, file),
            fail=lambda: readlink(link) is not None,
            msg="File %s points to somewhere else")()
