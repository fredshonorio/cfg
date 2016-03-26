from lib.execute import run_all, execute, swallow, call_show
from functools import partial

def _install(cmd, *args):
    mk_cmd = lambda pkg: execute(lambda: call_show(cmd + ["-S", pkg, "--needed"]),
                                 when=lambda: package_is_not_installed(pkg))
    run_all(map(mk_cmd, args))

pacman = partial(_install, ["sudo", "pacman"])
yaourt = partial(_install, ["yaourt"])

def package_is_not_installed(pkg):
    return swallow(["pacman", "-Q", pkg]).failed
