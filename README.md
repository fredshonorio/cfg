
cfg
---
This repo hosts my configuration and tools to apply it. Requires python 3.

# TODO
- chmod? or merge with permissions? - ~/.bin/ scripts are not executable
- also unbind alt-f7, alt-insert
- add a first install script:
  - choose mirrors (pacman-mirrors)
  - get keys
    - manjaro-keyring
    - archlinux-keyring
  - pacman -Syu
- do i need desk? https://github.com/jamesob/desk
- import intellij settings, pycharm settings
- ssh, aws? - merge/encrypt to spideroak vault?
- use tomb for secrets:
  - add a method that opens a tomp and runs a file inside
  - add a method that syncronizes a tomb with a dir
      - tomb_diff("ssh-config", "~/.ssh")
- if not `groups fredh | grep docker`
      usermod -a -G docker fredh
      systemctl enable docker.service
      print restart boy
