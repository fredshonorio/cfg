cfg
---
This repo hosts my configuration and tools to apply it. Requires python 3.

# TODO
- chmod? or merge with permissions? - ~/.bin/ scripts are not executable
- add a first install script:
  - choose mirrors (pacman-mirrors)
  - get keys
    - manjaro-keyring
    - archlinux-keyring
  - pacman -Syu
- do i need desk? https://github.com/jamesob/desk
- import intellij settings, pycharm settings
- if not `groups fredh | grep docker`
      usermod -a -G docker fredh
      systemctl enable docker.service
      print restart boy

# Manual
## Update emacs (and prelude)

__Update packages__
`M-x package-list-packages RET U x`

__Update prelude__
`M-x prelude-update`
