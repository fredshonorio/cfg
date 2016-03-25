from lib.execute import run_all, do_when, call, swallow

def pacman(*args):
    def make_cmd(pkg):
        return do_when(
            lambda: call(["sudo", "pacman", "-S", pkg, "--needed"]),
            lambda: package_is_not_installed(pkg))
    run_all(map(make_cmd, args))

def yaourt(*args):
    def make_cmd(pkg):
        return do_when(
            lambda: call(["yaourt", "-S", pkg, "--needed"]),
            lambda: package_is_not_installed(pkg))
    run_all(map(make_cmd, args))

def package_is_not_installed(pkg):
    return swallow(["pacman", "-Q", pkg]).failed
