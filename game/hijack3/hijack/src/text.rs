use super::*;

pub fn text(x: i32, y: i32, msg: String) -> Vec<View> {
    msg.chars()
        .enumerate()
        .filter_map(|(i, c)| {
            let i = i as i32;
            match c {
                'a' => Some(View::Image(
                    "pixelart/font/alpha/a.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'b' => Some(View::Image(
                    "pixelart/font/alpha/b.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'c' => Some(View::Image(
                    "pixelart/font/alpha/c.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'd' => Some(View::Image(
                    "pixelart/font/alpha/d.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'e' => Some(View::Image(
                    "pixelart/font/alpha/e.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'f' => Some(View::Image(
                    "pixelart/font/alpha/f.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'g' => Some(View::Image(
                    "pixelart/font/alpha/g.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'h' => Some(View::Image(
                    "pixelart/font/alpha/h.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'i' => Some(View::Image(
                    "pixelart/font/alpha/i.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'j' => Some(View::Image(
                    "pixelart/font/alpha/j.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'k' => Some(View::Image(
                    "pixelart/font/alpha/k.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'l' => Some(View::Image(
                    "pixelart/font/alpha/l.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'm' => Some(View::Image(
                    "pixelart/font/alpha/m.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'n' => Some(View::Image(
                    "pixelart/font/alpha/n.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'o' => Some(View::Image(
                    "pixelart/font/alpha/o.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'p' => Some(View::Image(
                    "pixelart/font/alpha/p.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'q' => Some(View::Image(
                    "pixelart/font/alpha/q.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'r' => Some(View::Image(
                    "pixelart/font/alpha/r.png".to_string(),
                    x + i * 8,
                    y,
                )),
                's' => Some(View::Image(
                    "pixelart/font/alpha/s.png".to_string(),
                    x + i * 8,
                    y,
                )),
                't' => Some(View::Image(
                    "pixelart/font/alpha/t.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'u' => Some(View::Image(
                    "pixelart/font/alpha/u.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'v' => Some(View::Image(
                    "pixelart/font/alpha/v.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'w' => Some(View::Image(
                    "pixelart/font/alpha/w.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'x' => Some(View::Image(
                    "pixelart/font/alpha/x.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'y' => Some(View::Image(
                    "pixelart/font/alpha/y.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'z' => Some(View::Image(
                    "pixelart/font/alpha/z.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'あ' => Some(View::Image(
                    "pixelart/font/hiragana/a.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'い' => Some(View::Image(
                    "pixelart/font/hiragana/i.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'う' => Some(View::Image(
                    "pixelart/font/hiragana/u.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'え' => Some(View::Image(
                    "pixelart/font/hiragana/e.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'お' => Some(View::Image(
                    "pixelart/font/hiragana/o.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'か' => Some(View::Image(
                    "pixelart/font/hiragana/ka.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'き' => Some(View::Image(
                    "pixelart/font/hiragana/ki.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'く' => Some(View::Image(
                    "pixelart/font/hiragana/ku.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'け' => Some(View::Image(
                    "pixelart/font/hiragana/ke.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'こ' => Some(View::Image(
                    "pixelart/font/hiragana/ko.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'さ' => Some(View::Image(
                    "pixelart/font/hiragana/sa.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'し' => Some(View::Image(
                    "pixelart/font/hiragana/si.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'す' => Some(View::Image(
                    "pixelart/font/hiragana/su.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'せ' => Some(View::Image(
                    "pixelart/font/hiragana/se.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'そ' => Some(View::Image(
                    "pixelart/font/hiragana/so.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'た' => Some(View::Image(
                    "pixelart/font/hiragana/ta.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ち' => Some(View::Image(
                    "pixelart/font/hiragana/ti.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'つ' => Some(View::Image(
                    "pixelart/font/hiragana/tu.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'て' => Some(View::Image(
                    "pixelart/font/hiragana/te.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'と' => Some(View::Image(
                    "pixelart/font/hiragana/to.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'な' => Some(View::Image(
                    "pixelart/font/hiragana/na.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'に' => Some(View::Image(
                    "pixelart/font/hiragana/ni.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ぬ' => Some(View::Image(
                    "pixelart/font/hiragana/nu.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ね' => Some(View::Image(
                    "pixelart/font/hiragana/ne.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'の' => Some(View::Image(
                    "pixelart/font/hiragana/no.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'は' => Some(View::Image(
                    "pixelart/font/hiragana/ha.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ひ' => Some(View::Image(
                    "pixelart/font/hiragana/hi.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ふ' => Some(View::Image(
                    "pixelart/font/hiragana/hu.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'へ' => Some(View::Image(
                    "pixelart/font/hiragana/he.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ほ' => Some(View::Image(
                    "pixelart/font/hiragana/ho.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ま' => Some(View::Image(
                    "pixelart/font/hiragana/ma.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'み' => Some(View::Image(
                    "pixelart/font/hiragana/mi.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'む' => Some(View::Image(
                    "pixelart/font/hiragana/mu.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'め' => Some(View::Image(
                    "pixelart/font/hiragana/me.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'も' => Some(View::Image(
                    "pixelart/font/hiragana/mo.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'や' => Some(View::Image(
                    "pixelart/font/hiragana/ya.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ゆ' => Some(View::Image(
                    "pixelart/font/hiragana/yu.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'よ' => Some(View::Image(
                    "pixelart/font/hiragana/yo.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ら' => Some(View::Image(
                    "pixelart/font/hiragana/ra.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'り' => Some(View::Image(
                    "pixelart/font/hiragana/ri.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'る' => Some(View::Image(
                    "pixelart/font/hiragana/ru.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'れ' => Some(View::Image(
                    "pixelart/font/hiragana/re.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ろ' => Some(View::Image(
                    "pixelart/font/hiragana/ro.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'わ' => Some(View::Image(
                    "pixelart/font/hiragana/wa.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'を' => Some(View::Image(
                    "pixelart/font/hiragana/wo.png".to_string(),
                    x + i * 8,
                    y,
                )),
                'ん' => Some(View::Image(
                    "pixelart/font/hiragana/n.png".to_string(),
                    x + i * 8,
                    y,
                )),
                _ => None,
            }
        })
        .collect()
}
