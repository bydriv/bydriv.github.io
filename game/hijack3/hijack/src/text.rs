use super::*;

pub fn text(x: i32, y: i32, msg: String) -> Vec<View> {
    msg.chars()
        .enumerate()
        .flat_map(|(i, c)| {
            let i = i as i32;
            match c {
                'a' => vec![View::Image(
                    "pixelart/font/alpha/a.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'b' => vec![View::Image(
                    "pixelart/font/alpha/b.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'c' => vec![View::Image(
                    "pixelart/font/alpha/c.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'd' => vec![View::Image(
                    "pixelart/font/alpha/d.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'e' => vec![View::Image(
                    "pixelart/font/alpha/e.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'f' => vec![View::Image(
                    "pixelart/font/alpha/f.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'g' => vec![View::Image(
                    "pixelart/font/alpha/g.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'h' => vec![View::Image(
                    "pixelart/font/alpha/h.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'i' => vec![View::Image(
                    "pixelart/font/alpha/i.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'j' => vec![View::Image(
                    "pixelart/font/alpha/j.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'k' => vec![View::Image(
                    "pixelart/font/alpha/k.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'l' => vec![View::Image(
                    "pixelart/font/alpha/l.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'm' => vec![View::Image(
                    "pixelart/font/alpha/m.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'n' => vec![View::Image(
                    "pixelart/font/alpha/n.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'o' => vec![View::Image(
                    "pixelart/font/alpha/o.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'p' => vec![View::Image(
                    "pixelart/font/alpha/p.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'q' => vec![View::Image(
                    "pixelart/font/alpha/q.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'r' => vec![View::Image(
                    "pixelart/font/alpha/r.png".to_string(),
                    x + i * 8,
                    y,
                )],
                's' => vec![View::Image(
                    "pixelart/font/alpha/s.png".to_string(),
                    x + i * 8,
                    y,
                )],
                't' => vec![View::Image(
                    "pixelart/font/alpha/t.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'u' => vec![View::Image(
                    "pixelart/font/alpha/u.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'v' => vec![View::Image(
                    "pixelart/font/alpha/v.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'w' => vec![View::Image(
                    "pixelart/font/alpha/w.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'x' => vec![View::Image(
                    "pixelart/font/alpha/x.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'y' => vec![View::Image(
                    "pixelart/font/alpha/y.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'z' => vec![View::Image(
                    "pixelart/font/alpha/z.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'あ' => vec![View::Image(
                    "pixelart/font/hiragana/a.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'い' => vec![View::Image(
                    "pixelart/font/hiragana/i.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'う' => vec![View::Image(
                    "pixelart/font/hiragana/u.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'え' => vec![View::Image(
                    "pixelart/font/hiragana/e.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'お' => vec![View::Image(
                    "pixelart/font/hiragana/o.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'か' => vec![View::Image(
                    "pixelart/font/hiragana/ka.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'き' => vec![View::Image(
                    "pixelart/font/hiragana/ki.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'く' => vec![View::Image(
                    "pixelart/font/hiragana/ku.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'け' => vec![View::Image(
                    "pixelart/font/hiragana/ke.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'こ' => vec![View::Image(
                    "pixelart/font/hiragana/ko.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'が' => vec![
                    View::Image("pixelart/font/hiragana/ka.png".to_string(), x + i * 8, y),
                    View::Image(
                        "pixelart/font/hiragana/dakuten.png".to_string(),
                        x + i * 8,
                        y - 6,
                    ),
                ],
                'ぎ' => vec![
                    View::Image("pixelart/font/hiragana/ki.png".to_string(), x + i * 8, y),
                    View::Image(
                        "pixelart/font/hiragana/dakuten.png".to_string(),
                        x + i * 8,
                        y - 6,
                    ),
                ],
                'ぐ' => vec![
                    View::Image("pixelart/font/hiragana/ku.png".to_string(), x + i * 8, y),
                    View::Image(
                        "pixelart/font/hiragana/dakuten.png".to_string(),
                        x + i * 8,
                        y - 6,
                    ),
                ],
                'げ' => vec![
                    View::Image("pixelart/font/hiragana/ke.png".to_string(), x + i * 8, y),
                    View::Image(
                        "pixelart/font/hiragana/dakuten.png".to_string(),
                        x + i * 8,
                        y - 6,
                    ),
                ],
                'ご' => vec![
                    View::Image("pixelart/font/hiragana/ko.png".to_string(), x + i * 8, y),
                    View::Image(
                        "pixelart/font/hiragana/dakuten.png".to_string(),
                        x + i * 8,
                        y - 6,
                    ),
                ],
                'さ' => vec![View::Image(
                    "pixelart/font/hiragana/sa.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'し' => vec![View::Image(
                    "pixelart/font/hiragana/si.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'す' => vec![View::Image(
                    "pixelart/font/hiragana/su.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'せ' => vec![View::Image(
                    "pixelart/font/hiragana/se.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'そ' => vec![View::Image(
                    "pixelart/font/hiragana/so.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'た' => vec![View::Image(
                    "pixelart/font/hiragana/ta.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ち' => vec![View::Image(
                    "pixelart/font/hiragana/ti.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'つ' => vec![View::Image(
                    "pixelart/font/hiragana/tu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'て' => vec![View::Image(
                    "pixelart/font/hiragana/te.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'と' => vec![View::Image(
                    "pixelart/font/hiragana/to.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'っ' => vec![View::Image(
                    "pixelart/font/hiragana/xtu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'な' => vec![View::Image(
                    "pixelart/font/hiragana/na.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'に' => vec![View::Image(
                    "pixelart/font/hiragana/ni.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ぬ' => vec![View::Image(
                    "pixelart/font/hiragana/nu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ね' => vec![View::Image(
                    "pixelart/font/hiragana/ne.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'の' => vec![View::Image(
                    "pixelart/font/hiragana/no.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'は' => vec![View::Image(
                    "pixelart/font/hiragana/ha.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ひ' => vec![View::Image(
                    "pixelart/font/hiragana/hi.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ふ' => vec![View::Image(
                    "pixelart/font/hiragana/hu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'へ' => vec![View::Image(
                    "pixelart/font/hiragana/he.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ほ' => vec![View::Image(
                    "pixelart/font/hiragana/ho.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ま' => vec![View::Image(
                    "pixelart/font/hiragana/ma.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'み' => vec![View::Image(
                    "pixelart/font/hiragana/mi.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'む' => vec![View::Image(
                    "pixelart/font/hiragana/mu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'め' => vec![View::Image(
                    "pixelart/font/hiragana/me.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'も' => vec![View::Image(
                    "pixelart/font/hiragana/mo.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'や' => vec![View::Image(
                    "pixelart/font/hiragana/ya.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ゆ' => vec![View::Image(
                    "pixelart/font/hiragana/yu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'よ' => vec![View::Image(
                    "pixelart/font/hiragana/yo.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ら' => vec![View::Image(
                    "pixelart/font/hiragana/ra.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'り' => vec![View::Image(
                    "pixelart/font/hiragana/ri.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'る' => vec![View::Image(
                    "pixelart/font/hiragana/ru.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'れ' => vec![View::Image(
                    "pixelart/font/hiragana/re.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ろ' => vec![View::Image(
                    "pixelart/font/hiragana/ro.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'わ' => vec![View::Image(
                    "pixelart/font/hiragana/wa.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'を' => vec![View::Image(
                    "pixelart/font/hiragana/wo.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ん' => vec![View::Image(
                    "pixelart/font/hiragana/n.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ぁ' => vec![View::Image(
                    "pixelart/font/hiragana/xa.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ぃ' => vec![View::Image(
                    "pixelart/font/hiragana/xi.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ぅ' => vec![View::Image(
                    "pixelart/font/hiragana/xu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ぇ' => vec![View::Image(
                    "pixelart/font/hiragana/xe.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ぉ' => vec![View::Image(
                    "pixelart/font/hiragana/xo.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ゃ' => vec![View::Image(
                    "pixelart/font/hiragana/xya.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ゅ' => vec![View::Image(
                    "pixelart/font/hiragana/xyu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ょ' => vec![View::Image(
                    "pixelart/font/hiragana/xyo.png".to_string(),
                    x + i * 8,
                    y,
                )],
                '、' => vec![View::Image(
                    "pixelart/font/hiragana/touten.png".to_string(),
                    x + i * 8,
                    y,
                )],
                '。' => vec![View::Image(
                    "pixelart/font/hiragana/kuten.png".to_string(),
                    x + i * 8,
                    y,
                )],
                'ー' => vec![View::Image(
                    "pixelart/font/hiragana/choonpu.png".to_string(),
                    x + i * 8,
                    y,
                )],
                _ => vec![],
            }
        })
        .collect()
}
