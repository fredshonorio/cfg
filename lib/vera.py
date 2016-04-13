
from contextlib import contextmanager
from lib.execute import call, execute
from subprocess import check_output
from lib.fs import flatten_dir
from os.path import expanduser, getmtime
from time import ctime

MOUNT_DIR_PREFIX = "Mount Directory: "

@contextmanager
def mount(volume):
    call(["veracrypt", "--mount", volume])
    out_lines = check_output(["veracrypt", "-t", "--volume-properties", volume]) \
        .decode("utf8") \
        .split("\n")

    m_dir = next(
        map(lambda l: l.strip().replace(MOUNT_DIR_PREFIX, ""),
        filter(lambda l: l.startswith(MOUNT_DIR_PREFIX),
        out_lines)))

    try:
        yield m_dir
    finally:
        call(["veracrypt", "-d", volume])

def merge_to_dir(volume, d, force=False):
    volume, d = map(expanduser, [volume, d])
    def merge():
        with mount(volume) as mount_dir:
            call(["meld", mount_dir, d])

    execute(merge,
            when=lambda: force or volume_was_modified_after_files(volume, d))()

def volume_was_modified_after_files(volume, d):
    dir = list(flatten_dir(d))

    if not dir:
        return True

    return all(modified(f) <= modified(volume) for f in dir)

modified = lambda f: ctime(getmtime(f))
