1. apply the version specific `st-scrollback` patch
2. apply the version specific `st-externalpipe` patch
3. copy `config.def.h` to `config.h`
4. compile with `sudo make clean install`
5. apply custom patch with `patch -p0 < ~/dotfiles/suckless/st/mypatch.diff`
