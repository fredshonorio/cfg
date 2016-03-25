from lib.execute import run_all, execute, swallow, call

def pacman(*args):
    def make_cmd(pkg):
        return execute(lambda: call(["sudo", "pacman", "-S", pkg, "--needed"]),
                       when=lambda: package_is_not_installed(pkg))
    run_all(map(make_cmd, args))

def yaourt(*args):
    def make_cmd(pkg):
        return execute(lambda: call(["yaourt", "-S", pkg, "--needed"]),
                       when=lambda: package_is_not_installed(pkg))
    run_all(map(make_cmd, args))

def package_is_not_installed(pkg):
    return swallow(["pacman", "-Q", pkg]).failed
