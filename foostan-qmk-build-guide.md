# Increase layers for vial

follow the repo instructions to clone the repo with submodules https://github.com/foostan/kbd_firmware.git

add this to the file `keyboards/crkbd/vial-kb/vial-qmk/keymaps/vial/config.h`

```c
#undef DYNAMIC_KEYMAP_LAYER_COUNT
#define DYNAMIC_KEYMAP_LAYER_COUNT 8
```

compile with the command

```bash
kb=crkbd kr=rev4_1/standard km=vial make vial-qmk-compile
```

the artifact location would be in the directory `keyboards/crkbd/vial-kb/vial-qmk/.build`
