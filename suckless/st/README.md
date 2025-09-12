1. apply the version specific `st-scrollback` patch
2. copy `config.def.h` to `config.h`
3. compile with `sudo make clean install`
3. apply custom patch with `patch -p0 < ~/dotfiles/suckless/st/mypatch.diff`
