import re
from xkeysnail.transform import *

# Keybindings for Firefox/Chrome
# define_keymap(re.compile("Firefox|Google-chrome"), {
#     # Ctrl+j/k to switch next/previous tab
#     K("C-k"): K("C-TAB"),
#     K("C-j"): K("C-Shift-TAB"),
# }, "Firefox and Chrome")

# Keybindings for Zeal https://github.com/zealdocs/zeal/
# define_keymap(re.compile("Zeal"), {
#     # Ctrl+s to focus search area
#     K("C-s"): K("C-k"),
# }, "Zeal")

# define_modmap({
#     Key.CAPSLOCK: Key.LEFT_CTRL,
#     Key.ENTER: Key.RIGHT_CTRL
# })
# define_modmap({
#     Key.LEFT_CTRL: Key.CAPSLOCK
#     })
define_multipurpose_modmap({
    Key.ENTER: [Key.ENTER, Key.RIGHT_CTRL],
    Key.CAPSLOCK: [Key.ESC, Key.LEFT_CTRL],
    # Key.LEFT_CTRL: [Key.CAPSLOCK, Key.LEFT_CTRL],
    # Key.SPACE: [Key.SPACE, Key.LEFT_META],
    })
# Emacs-like keybindings in non-Emacs applications, my version
# define_keymap(lambda wm_class: wm_class not in ("Emacs24", "URxvt"),{
#     # Ctrl+j/k to switch next/previous tab
#     K("C-k"): K("C-TAB"),
#     K("C-j"): K("C-Shift-TAB"),
#     K("C-LEFT_BRACE"): [K("esc"), set_mark(False)],
#     K("C-m"): K("enter")
# }, "None")
define_keymap(lambda wm_class: wm_class in ("konsole"),{
    K("C-k"): K("C-TAB"),
    K("C-j"): K("C-Shift-TAB"),
    }, "keybindings in konsole")
# Emacs-like keybindings in non-Emacs applications
define_keymap(lambda wm_class: wm_class not in ("Emacs", "URxvt", "konsole"), {
# define_keymap(re.compile("emacs"),{
    # Add by Yanghao xie
    # Ctrl+j/k to switch next/previous tab
    K("C-k"): K("C-TAB"),
    K("C-l"): K("C-TAB"),
    K("C-j"): K("C-Shift-TAB"),
    K("C-LEFT_BRACE"): [K("esc"), set_mark(False)],
    K("C-r"): K("F5"),
    # The end
    # Cursor
    K("C-b"): with_mark(K("left")),
    K("C-f"): with_mark(K("right")),
    K("C-p"): with_mark(K("up")),
    K("C-n"): with_mark(K("down")),
    K("C-h"): with_mark(K("backspace")),
    # Forward/Backward word
    K("M-b"): with_mark(K("C-left")),
    K("M-f"): with_mark(K("C-right")),
    # Beginning/End of line
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    # Page up/down
    # K("M-v"): with_mark(K("page_up")),
    # K("C-v"): with_mark(K("page_down")),
    # Beginning/End of file
    K("M-Shift-comma"): with_mark(K("C-home")),
    K("M-Shift-dot"): with_mark(K("C-end")),
    # Newline
    K("C-m"): K("enter"),
    # K("C-j"): K("enter"),
    K("C-o"): [K("enter"), K("left")],
    # Copy
    # K("C-w"): [K("C-x"), set_mark(False)],
    # K("M-w"): [K("C-c"), set_mark(False)],
    # K("C-y"): [K("C-v"), set_mark(False)],
    # Delete
    K("C-d"): [K("backspace"), set_mark(False)],
    # K("C-W"): [K("C-backspace"), set_mark(False)],
    # Kill line
    # K("C-k"): [K("Shift-end"), K("C-x"), set_mark(False)],
    # Undo
    K("C-slash"): [K("C-z"), set_mark(False)],
    K("C-Shift-ro"): K("C-z"),
    # Mark
    # K("C-space"): set_mark(True),
    # Search
    # K("C-s"): K("F3"),
    # K("C-r"): K("Shift-F3"),
    # K("M-Shift-key_5"): K("C-h"),
    # Cancel
    K("C-LEFT_BRACE"): [K("esc"), set_mark(False)],
    # Escape
    # K("C-q"): escape_next_key,
    # C-x YYY
    # K("C-x"): {
    #     # C-x h (select all)
    #     K("h"): [K("C-home"), K("C-a"), set_mark(True)],
    #     # C-x C-f (open)
    #     K("C-f"): K("C-o"),
    #     # C-x C-s (save)
    #     K("C-s"): K("C-s"),
    #     # C-x k (kill tab)
    #     K("k"): K("C-f4"),
    #     # C-x C-c (exit)
    #     K("C-c"): K("C-q"),
    #     # cancel
    #     K("C-g"): pass_through_key,
    #     # C-x u (undo)
    #     K("u"): [K("C-z"), set_mark(False)],
    # }
}, "Emacs-like keys")
