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
unsigned int ASSET_DEFNS_LEN = 312;

struct asset_defn ASSET_DEFNS[312] = {
    {"pixelart/system/window.png", "pixelart/system/window/top-left.png", 0, 0, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/top.png", 8, 0, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/top-arrow.png", 16, 0, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/top-right.png", 24, 0, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/left-arrow.png", 0, 8, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/center.png", 8, 8, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/right.png", 24, 8, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/left.png", 0, 16, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/right-arrow.png", 24, 16, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/bottom-left.png", 0, 24, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/bottom-arrow.png", 8, 24, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/bottom.png", 16, 24, 8, 8},
    {"pixelart/system/window.png", "pixelart/system/window/bottom-right.png", 24, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/a.png", 0, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/b.png", 8, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/c.png", 16, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/d.png", 24, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/e.png", 32, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/f.png", 40, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/g.png", 48, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/h.png", 56, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/i.png", 64, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/j.png", 72, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/k.png", 80, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/l.png", 88, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/m.png", 96, 16, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/n.png", 0, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/o.png", 8, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/p.png", 16, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/q.png", 24, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/r.png", 32, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/s.png", 40, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/t.png", 48, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/u.png", 56, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/v.png", 64, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/w.png", 72, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/x.png", 80, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/y.png", 88, 24, 8, 8},
    {"pixelart/font/alpha.png", "pixelart/font/alpha/z.png", 96, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/a.png", 0, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/i.png", 0, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/u.png", 0, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/e.png", 0, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/o.png", 0, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ka.png", 8, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ki.png", 8, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ku.png", 8, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ke.png", 8, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ko.png", 8, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/sa.png", 16, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/si.png", 16, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/su.png", 16, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/se.png", 16, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/so.png", 16, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ta.png", 24, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ti.png", 24, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/tu.png", 24, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/te.png", 24, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/to.png", 24, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xtu.png", 24, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/na.png", 32, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ni.png", 32, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/nu.png", 32, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ne.png", 32, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/no.png", 32, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ha.png", 40, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/hi.png", 40, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/hu.png", 40, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/he.png", 40, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ho.png", 40, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ma.png", 48, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/mi.png", 48, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/mu.png", 48, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/me.png", 48, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/mo.png", 48, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ya.png", 56, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/yu.png", 56, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/yo.png", 56, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ra.png", 64, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ri.png", 64, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ru.png", 64, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/re.png", 64, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/ro.png", 64, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/wa.png", 72, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/wo.png", 72, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/n.png", 72, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xa.png", 80, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xi.png", 80, 8, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xu.png", 80, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xe.png", 80, 24, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xo.png", 80, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xya.png", 88, 0, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xyu.png", 88, 16, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/xyo.png", 88, 32, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/touten.png", 0, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/kuten.png", 8, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/choonpu.png", 16, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/left-corner-bracket.png", 32, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/right-corner-bracket.png", 40, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/dakuten.png", 80, 40, 8, 8},
    {"pixelart/font/hiragana.png", "pixelart/font/hiragana/handakuten.png", 88, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/a.png", 0, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/i.png", 0, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/u.png", 0, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/e.png", 0, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/o.png", 0, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ka.png", 8, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ki.png", 8, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ku.png", 8, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ke.png", 8, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ko.png", 8, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/sa.png", 16, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/si.png", 16, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/su.png", 16, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/se.png", 16, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/so.png", 16, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ta.png", 24, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ti.png", 24, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/tu.png", 24, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/te.png", 24, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/to.png", 24, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xtu.png", 24, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/na.png", 32, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ni.png", 32, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/nu.png", 32, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ne.png", 32, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/no.png", 32, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ha.png", 40, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/hi.png", 40, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/hu.png", 40, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/he.png", 40, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ho.png", 40, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ma.png", 48, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/mi.png", 48, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/mu.png", 48, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/me.png", 48, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/mo.png", 48, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ya.png", 56, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/yu.png", 56, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/yo.png", 56, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ra.png", 64, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ri.png", 64, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ru.png", 64, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/re.png", 64, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/ro.png", 64, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/wa.png", 72, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/wo.png", 72, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/n.png", 72, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xa.png", 80, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xi.png", 80, 8, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xu.png", 80, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xe.png", 80, 24, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xo.png", 80, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xya.png", 88, 0, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xyu.png", 88, 16, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/xyo.png", 88, 32, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/touten.png", 0, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/kuten.png", 8, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/choonpu.png", 16, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/left-corner-bracket.png", 32, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/right-corner-bracket.png", 40, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/dakuten.png", 80, 40, 8, 8},
    {"pixelart/font/katakana.png", "pixelart/font/katakana/handakuten.png", 88, 40, 8, 8},
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
    {"pixelart/emily/walk.png", "pixelart/emily/walk/left/0.png", 0, 0, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/left/1.png", 16, 0, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/left/2.png", 32, 0, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/left/3.png", 48, 0, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/back/0.png", 0, 16, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/back/1.png", 16, 16, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/back/2.png", 32, 16, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/back/3.png", 48, 16, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/right/0.png", 0, 32, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/right/1.png", 16, 32, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/right/2.png", 32, 32, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/right/3.png", 48, 32, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/front/0.png", 0, 48, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/front/1.png", 16, 48, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/front/2.png", 32, 48, 16, 16},
    {"pixelart/emily/walk.png", "pixelart/emily/walk/front/3.png", 48, 48, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/left/0.png", 0, 0, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/left/1.png", 16, 0, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/left/2.png", 32, 0, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/left/3.png", 48, 0, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/back/0.png", 0, 16, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/back/1.png", 16, 16, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/back/2.png", 32, 16, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/back/3.png", 48, 16, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/right/0.png", 0, 32, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/right/1.png", 16, 32, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/right/2.png", 32, 32, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/right/3.png", 48, 32, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/front/0.png", 0, 48, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/front/1.png", 16, 48, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/front/2.png", 32, 48, 16, 16},
    {"pixelart/mathprobe/fly.png", "pixelart/mathprobe/fly/front/3.png", 48, 48, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/left/0.png", 0, 0, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/left/1.png", 16, 0, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/left/2.png", 32, 0, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/left/3.png", 48, 0, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/back/0.png", 0, 16, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/back/1.png", 16, 16, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/back/2.png", 32, 16, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/back/3.png", 48, 16, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/right/0.png", 0, 32, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/right/1.png", 16, 32, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/right/2.png", 32, 32, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/right/3.png", 48, 32, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/front/0.png", 0, 48, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/front/1.png", 16, 48, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/front/2.png", 32, 48, 16, 16},
    {"pixelart/lonelygem/fly.png", "pixelart/lonelygem/fly/front/3.png", 48, 48, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/left/0.png", 0, 0, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/left/1.png", 16, 0, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/left/2.png", 32, 0, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/left/3.png", 48, 0, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/back/0.png", 0, 16, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/back/1.png", 16, 16, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/back/2.png", 32, 16, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/back/3.png", 48, 16, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/right/0.png", 0, 32, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/right/1.png", 16, 32, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/right/2.png", 32, 32, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/right/3.png", 48, 32, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/front/0.png", 0, 48, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/front/1.png", 16, 48, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/front/2.png", 32, 48, 16, 16},
    {"pixelart/security-drone/fly.png", "pixelart/security-drone/fly/front/3.png", 48, 48, 16, 16},
    {"pixelart/maptip/archimedes.png", "pixelart/maptip/archimedes.png", 0, 0, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/top-left.png", 0, 0, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/top.png", 16, 0, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/top-right.png", 32, 0, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/left.png", 0, 16, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/center.png", 16, 16, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/right.png", 32, 16, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/bottom-left.png", 0, 32, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/bottom.png", 16, 32, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/bottom-right.png", 32, 32, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/top-left_inv.png", 48, 0, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/top-right_inv.png", 80, 0, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/bottom-left_inv.png", 48, 32, 16, 16},
    {"pixelart/maptip/ground.png", "pixelart/maptip/ground/bottom-right_inv.png", 80, 32, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/top-left.png", 0, 0, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/top.png", 16, 0, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/top-right.png", 32, 0, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/left.png", 0, 16, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/center.png", 16, 16, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/right.png", 32, 16, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/bottom-left.png", 0, 32, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/bottom.png", 16, 32, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/bottom-right.png", 32, 32, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/top-left_inv.png", 48, 0, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/top-right_inv.png", 80, 0, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/bottom-left_inv.png", 48, 32, 16, 16},
    {"pixelart/maptip/asphalt.png", "pixelart/maptip/asphalt/bottom-right_inv.png", 80, 32, 16, 16},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/top-left.png", 0, 0, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/top.png", 32, 0, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/top-right.png", 64, 0, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/left.png", 0, 32, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/center.png", 32, 32, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/right.png", 64, 32, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/bottom-left.png", 0, 64, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/bottom.png", 32, 64, 32, 32},
    {"pixelart/maptip/tree.png", "pixelart/maptip/tree/bottom-right.png", 64, 64, 32, 32},
    {"pixelart/effect/dark.png", "pixelart/effect/dark.png", 0, 0, 320, 240},
};
