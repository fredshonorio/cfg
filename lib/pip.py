
from lib.execute import execute, call_show, swallow, run_all
from functools import partial

def package_is_not_intalled(pip, pkg):
    return swallow([pip, "show", pkg]).failed

def _pip(pip, *pkgs, sudo=False):
    mk_call = lambda pkg: execute(lambda: call_show([pip, "install", pkg], sudo),
                                  when=lambda: package_is_not_intalled(pip, pkg))
    run_all(map(mk_call, pkgs))

pip2 = partial(_pip, "pip2")
pip3 = partial(_pip, "pip3")
