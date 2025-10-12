1. apply the version specific `st-scrollback` patch
2. apply the version specific `st-scrollback-reflow` patch
3. apply the version specific `st-externalpipe` patch
4. apply the version specific `st-externalpipe-eternal` patch
5. copy `config.def.h` to `config.h`
6. compile with `sudo make clean install`
7. apply custom patch with `patch -p0 < ~/dotfiles/suckless/st/mypatch.diff`
