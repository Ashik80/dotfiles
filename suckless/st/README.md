1. apply the version specific `st-scrollback` patch
2. apply the version specific `st-externalpipe` patch
3. apply the version specific `st-externalpipe-eternal` patch
4. copy `config.def.h` to `config.h`
5. compile with `sudo make clean install`
6. apply custom patch with `patch -p0 < ~/dotfiles/suckless/st/mypatch.diff`
