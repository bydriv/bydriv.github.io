struct asset_defn {
  const char *path;
  const char *name;
  unsigned int x;
  unsigned int y;
  unsigned int width;
  unsigned int height;
};

unsigned int SCALE = 2;
unsigned int WIDTH = 320;
unsigned int HEIGHT = 240;
unsigned int ASSET_DEFNS_LEN = 49;

struct asset_defn ASSET_DEFNS[49] = {
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/left/0.png", 0, 0, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/left/1.png", 16, 0, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/left/2.png", 32, 0, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/left/3.png", 48, 0, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/back/0.png", 0, 16, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/back/1.png", 16, 16, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/back/2.png", 32, 16, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/back/3.png", 48, 16, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/right/0.png", 0, 32, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/right/1.png", 16, 32, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/right/2.png", 32, 32, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/right/3.png", 48, 32, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/front/0.png", 0, 48, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/front/1.png", 16, 48, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/front/2.png", 32, 48, 16, 16},
    {"pixelart/teiri/walk.png", "pixelart/teiri/walk/front/3.png", 48, 48, 16, 16},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/left/0.png", 0, 0, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/left/1.png", 32, 0, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/left/2.png", 64, 0, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/left/3.png", 96, 0, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/back/0.png", 0, 32, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/back/1.png", 32, 32, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/back/2.png", 64, 32, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/back/3.png", 96, 32, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/right/0.png", 0, 64, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/right/1.png", 32, 64, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/right/2.png", 64, 64, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/right/3.png", 96, 64, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/front/0.png", 0, 96, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/front/1.png", 32, 96, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/front/2.png", 64, 96, 32, 32},
    {"pixelart/teiri/hijack.png", "pixelart/teiri/hijack/front/3.png", 96, 96, 32, 32},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/left/0.png", 0, 0, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/left/1.png", 16, 0, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/left/2.png", 32, 0, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/left/3.png", 48, 0, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/back/0.png", 0, 16, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/back/1.png", 16, 16, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/back/2.png", 32, 16, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/back/3.png", 48, 16, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/right/0.png", 0, 32, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/right/1.png", 16, 32, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/right/2.png", 32, 32, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/right/3.png", 48, 32, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/front/0.png", 0, 48, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/front/1.png", 16, 48, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/front/2.png", 32, 48, 16, 16},
    {"pixelart/verity/walk.png", "pixelart/verity/walk/front/3.png", 48, 48, 16, 16},
    {"pixelart/maptip/archimedes.png", "pixelart/maptip/archimedes.png", 0, 0, 16, 16},
};
