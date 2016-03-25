from lib.execute import swallow, do_when, call
from functools import partial

def get_prop(chan, prop):
    r = swallow(["xfconf-query", "-c", chan, "-p", prop, "-v"])
    return None if r.failed else r.out.decode("utf8").strip()

def prop_is_set(chan, prop, val):
    p = get_prop(chan, prop)
    return False if not p else p == val

def set_prop(chan, prop, val):
    def cmd():
        print("Setting property %s in channel %s to %s (was %s)" % (chan, prop, val, get_prop(chan, prop)))
        call(["xfconf-query", "-c", chan, "-p", prop, "-s", val])
    do_when(cmd, lambda: not prop_is_set(chan, prop, val))()

def unset_prop(chan, prop, old_val):
    p = get_prop(chan, prop)
    if not p:
        return
    if p == old_val:
        print("Unsetting property %s in channel %s (was %s)" % (chan, prop, old_val))
        call(["xfconf-query", "-c", chan, "-p", prop, "-r"])
    raise Exception("Not expecting this value")

kb_set = partial(set_prop, "xfce4-keyboard-shortcuts")
kb_unset = partial(unset_prop, "xfce4-keyboard-shortcuts")
wm_set = partial(set_prop, "xfwm4")
