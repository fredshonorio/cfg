from lib.execute import swallow, execute, call
from functools import partial

def get_prop(chan, prop):
    r = swallow(["xfconf-query", "-c", chan, "-p", prop, "-v"])
    return None if r.failed else r.out.decode("utf8").strip()

def prop_is_set(chan, prop, val):
    p = get_prop(chan, prop)
    return False if not p else p == val

def set_prop(chan, prop, val, create=False):
    """Sets a property value, unless it's already set"""
    def cmd():
        old_prop = get_prop(chan, prop)
        frag = ["-n", "-t", "string", "-s", val] if create and not old_prop \
               else ["-s", val]

        print("Setting property %s in channel %s to %s (was %s)" % (chan, prop, val, old_prop))
        call(["xfconf-query", "-c", chan, "-p", prop] + frag)
    execute(cmd,
            when=lambda: not prop_is_set(chan, prop, val))()

def unset_prop(chan, prop, expected_old_val):
    """Unsets a property value, checks if the old value is the one expected"""
    def cmd():
        print("Unsetting property %s in channel %s (was %s)" % (chan, prop, old_val))
        call(["xfconf-query", "-c", chan, "-p", prop, "-r"])
    old_val = get_prop(chan, prop)
    execute(cmd,
            when=lambda: old_val is not None,
            fail=lambda: old_val != expected_old_val,
            msg=lambda: "Cannot set property %s: got value '%s', expecting '%s'" % (prop, old_val, expected_old_val))()

kb_set = partial(set_prop, "xfce4-keyboard-shortcuts")
kb_unset = partial(unset_prop, "xfce4-keyboard-shortcuts")
wm_set = partial(set_prop, "xfwm4")
