use super::*;

pub fn text(x: i32, y: i32, z: i32, msg: String) -> Vec<View> {
    msg.chars()
        .enumerate()
        .flat_map(|(i, c)| {
            let i = i as i32;

            if 0x20 as char <= c && c < 0x7F as char {
                let c = c as u8;
                vec![View::Image(
                    format!("pixelart/font/ascii/{:X?}.png", c),
                    x + i * 8,
                    y,
                    z,
                )]
            } else {
                match c {
                    'あ' => vec![View::Image(
                        "pixelart/font/hiragana/a.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'い' => vec![View::Image(
                        "pixelart/font/hiragana/i.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'う' => vec![View::Image(
                        "pixelart/font/hiragana/u.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'え' => vec![View::Image(
                        "pixelart/font/hiragana/e.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'お' => vec![View::Image(
                        "pixelart/font/hiragana/o.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'か' => vec![View::Image(
                        "pixelart/font/hiragana/ka.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'き' => vec![View::Image(
                        "pixelart/font/hiragana/ki.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'く' => vec![View::Image(
                        "pixelart/font/hiragana/ku.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'け' => vec![View::Image(
                        "pixelart/font/hiragana/ke.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'こ' => vec![View::Image(
                        "pixelart/font/hiragana/ko.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'が' => vec![
                        View::Image("pixelart/font/hiragana/ka.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぎ' => vec![
                        View::Image("pixelart/font/hiragana/ki.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぐ' => vec![
                        View::Image("pixelart/font/hiragana/ku.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'げ' => vec![
                        View::Image("pixelart/font/hiragana/ke.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ご' => vec![
                        View::Image("pixelart/font/hiragana/ko.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'さ' => vec![View::Image(
                        "pixelart/font/hiragana/sa.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'し' => vec![View::Image(
                        "pixelart/font/hiragana/si.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'す' => vec![View::Image(
                        "pixelart/font/hiragana/su.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'せ' => vec![View::Image(
                        "pixelart/font/hiragana/se.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'そ' => vec![View::Image(
                        "pixelart/font/hiragana/so.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ざ' => vec![
                        View::Image("pixelart/font/hiragana/sa.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'じ' => vec![
                        View::Image("pixelart/font/hiragana/si.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ず' => vec![
                        View::Image("pixelart/font/hiragana/su.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぜ' => vec![
                        View::Image("pixelart/font/hiragana/se.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぞ' => vec![
                        View::Image("pixelart/font/hiragana/so.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'た' => vec![View::Image(
                        "pixelart/font/hiragana/ta.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ち' => vec![View::Image(
                        "pixelart/font/hiragana/ti.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'つ' => vec![View::Image(
                        "pixelart/font/hiragana/tu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'て' => vec![View::Image(
                        "pixelart/font/hiragana/te.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'と' => vec![View::Image(
                        "pixelart/font/hiragana/to.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'っ' => vec![View::Image(
                        "pixelart/font/hiragana/xtu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'だ' => vec![
                        View::Image("pixelart/font/hiragana/ta.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぢ' => vec![
                        View::Image("pixelart/font/hiragana/ti.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'づ' => vec![
                        View::Image("pixelart/font/hiragana/tu.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'で' => vec![
                        View::Image("pixelart/font/hiragana/te.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ど' => vec![
                        View::Image("pixelart/font/hiragana/to.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'な' => vec![View::Image(
                        "pixelart/font/hiragana/na.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'に' => vec![View::Image(
                        "pixelart/font/hiragana/ni.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ぬ' => vec![View::Image(
                        "pixelart/font/hiragana/nu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ね' => vec![View::Image(
                        "pixelart/font/hiragana/ne.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'の' => vec![View::Image(
                        "pixelart/font/hiragana/no.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'は' => vec![View::Image(
                        "pixelart/font/hiragana/ha.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ひ' => vec![View::Image(
                        "pixelart/font/hiragana/hi.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ふ' => vec![View::Image(
                        "pixelart/font/hiragana/hu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'へ' => vec![View::Image(
                        "pixelart/font/hiragana/he.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ほ' => vec![View::Image(
                        "pixelart/font/hiragana/ho.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ば' => vec![
                        View::Image("pixelart/font/hiragana/ha.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'び' => vec![
                        View::Image("pixelart/font/hiragana/hi.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぶ' => vec![
                        View::Image("pixelart/font/hiragana/hu.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'べ' => vec![
                        View::Image("pixelart/font/hiragana/he.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぼ' => vec![
                        View::Image("pixelart/font/hiragana/ho.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぱ' => vec![
                        View::Image("pixelart/font/hiragana/ha.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぴ' => vec![
                        View::Image("pixelart/font/hiragana/hi.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぷ' => vec![
                        View::Image("pixelart/font/hiragana/hu.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぺ' => vec![
                        View::Image("pixelart/font/hiragana/he.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ぽ' => vec![
                        View::Image("pixelart/font/hiragana/ho.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/hiragana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ま' => vec![View::Image(
                        "pixelart/font/hiragana/ma.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'み' => vec![View::Image(
                        "pixelart/font/hiragana/mi.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'む' => vec![View::Image(
                        "pixelart/font/hiragana/mu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'め' => vec![View::Image(
                        "pixelart/font/hiragana/me.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'も' => vec![View::Image(
                        "pixelart/font/hiragana/mo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'や' => vec![View::Image(
                        "pixelart/font/hiragana/ya.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ゆ' => vec![View::Image(
                        "pixelart/font/hiragana/yu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'よ' => vec![View::Image(
                        "pixelart/font/hiragana/yo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ら' => vec![View::Image(
                        "pixelart/font/hiragana/ra.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'り' => vec![View::Image(
                        "pixelart/font/hiragana/ri.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'る' => vec![View::Image(
                        "pixelart/font/hiragana/ru.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'れ' => vec![View::Image(
                        "pixelart/font/hiragana/re.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ろ' => vec![View::Image(
                        "pixelart/font/hiragana/ro.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'わ' => vec![View::Image(
                        "pixelart/font/hiragana/wa.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'を' => vec![View::Image(
                        "pixelart/font/hiragana/wo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ん' => vec![View::Image(
                        "pixelart/font/hiragana/n.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ぁ' => vec![View::Image(
                        "pixelart/font/hiragana/xa.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ぃ' => vec![View::Image(
                        "pixelart/font/hiragana/xi.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ぅ' => vec![View::Image(
                        "pixelart/font/hiragana/xu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ぇ' => vec![View::Image(
                        "pixelart/font/hiragana/xe.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ぉ' => vec![View::Image(
                        "pixelart/font/hiragana/xo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ゃ' => vec![View::Image(
                        "pixelart/font/hiragana/xya.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ゅ' => vec![View::Image(
                        "pixelart/font/hiragana/xyu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ょ' => vec![View::Image(
                        "pixelart/font/hiragana/xyo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    '、' => vec![View::Image(
                        "pixelart/font/hiragana/touten.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    '。' => vec![View::Image(
                        "pixelart/font/hiragana/kuten.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ー' => vec![View::Image(
                        "pixelart/font/hiragana/choonpu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    '「' => vec![View::Image(
                        "pixelart/font/hiragana/left-corner-bracket.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    '」' => vec![View::Image(
                        "pixelart/font/hiragana/right-corner-bracket.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ア' => vec![View::Image(
                        "pixelart/font/katakana/a.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'イ' => vec![View::Image(
                        "pixelart/font/katakana/i.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ウ' => vec![View::Image(
                        "pixelart/font/katakana/u.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ヴ' => vec![
                        View::Image("pixelart/font/katakana/u.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'エ' => vec![View::Image(
                        "pixelart/font/katakana/e.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'オ' => vec![View::Image(
                        "pixelart/font/katakana/o.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'カ' => vec![View::Image(
                        "pixelart/font/katakana/ka.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'キ' => vec![View::Image(
                        "pixelart/font/katakana/ki.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ク' => vec![View::Image(
                        "pixelart/font/katakana/ku.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ケ' => vec![View::Image(
                        "pixelart/font/katakana/ke.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'コ' => vec![View::Image(
                        "pixelart/font/katakana/ko.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ガ' => vec![
                        View::Image("pixelart/font/katakana/ka.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ギ' => vec![
                        View::Image("pixelart/font/katakana/ki.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'グ' => vec![
                        View::Image("pixelart/font/katakana/ku.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ゲ' => vec![
                        View::Image("pixelart/font/katakana/ke.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ゴ' => vec![
                        View::Image("pixelart/font/katakana/ko.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'サ' => vec![View::Image(
                        "pixelart/font/katakana/sa.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'シ' => vec![View::Image(
                        "pixelart/font/katakana/si.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ス' => vec![View::Image(
                        "pixelart/font/katakana/su.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'セ' => vec![View::Image(
                        "pixelart/font/katakana/se.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ソ' => vec![View::Image(
                        "pixelart/font/katakana/so.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ザ' => vec![
                        View::Image("pixelart/font/katakana/sa.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ジ' => vec![
                        View::Image("pixelart/font/katakana/si.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ズ' => vec![
                        View::Image("pixelart/font/katakana/su.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ゼ' => vec![
                        View::Image("pixelart/font/katakana/se.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ゾ' => vec![
                        View::Image("pixelart/font/katakana/so.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'タ' => vec![View::Image(
                        "pixelart/font/katakana/ta.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'チ' => vec![View::Image(
                        "pixelart/font/katakana/ti.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ツ' => vec![View::Image(
                        "pixelart/font/katakana/tu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'テ' => vec![View::Image(
                        "pixelart/font/katakana/te.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ト' => vec![View::Image(
                        "pixelart/font/katakana/to.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ッ' => vec![View::Image(
                        "pixelart/font/katakana/xtu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ダ' => vec![
                        View::Image("pixelart/font/katakana/ta.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ヂ' => vec![
                        View::Image("pixelart/font/katakana/ti.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ヅ' => vec![
                        View::Image("pixelart/font/katakana/tu.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'デ' => vec![
                        View::Image("pixelart/font/katakana/te.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ド' => vec![
                        View::Image("pixelart/font/katakana/to.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ナ' => vec![View::Image(
                        "pixelart/font/katakana/na.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ニ' => vec![View::Image(
                        "pixelart/font/katakana/ni.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ヌ' => vec![View::Image(
                        "pixelart/font/katakana/nu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ネ' => vec![View::Image(
                        "pixelart/font/katakana/ne.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ノ' => vec![View::Image(
                        "pixelart/font/katakana/no.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ハ' => vec![View::Image(
                        "pixelart/font/katakana/ha.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ヒ' => vec![View::Image(
                        "pixelart/font/katakana/hi.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'フ' => vec![View::Image(
                        "pixelart/font/katakana/hu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ヘ' => vec![View::Image(
                        "pixelart/font/katakana/he.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ホ' => vec![View::Image(
                        "pixelart/font/katakana/ho.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'バ' => vec![
                        View::Image("pixelart/font/katakana/ha.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ビ' => vec![
                        View::Image("pixelart/font/katakana/hi.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ブ' => vec![
                        View::Image("pixelart/font/katakana/hu.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ベ' => vec![
                        View::Image("pixelart/font/katakana/he.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ボ' => vec![
                        View::Image("pixelart/font/katakana/ho.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/dakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'パ' => vec![
                        View::Image("pixelart/font/katakana/ha.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ピ' => vec![
                        View::Image("pixelart/font/katakana/hi.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'プ' => vec![
                        View::Image("pixelart/font/katakana/hu.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ペ' => vec![
                        View::Image("pixelart/font/katakana/he.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'ポ' => vec![
                        View::Image("pixelart/font/katakana/ho.png".to_string(), x + i * 8, y, z),
                        View::Image(
                            "pixelart/font/katakana/handakuten.png".to_string(),
                            x + i * 8,
                            y - 6,
                            z,
                        ),
                    ],
                    'マ' => vec![View::Image(
                        "pixelart/font/katakana/ma.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ミ' => vec![View::Image(
                        "pixelart/font/katakana/mi.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ム' => vec![View::Image(
                        "pixelart/font/katakana/mu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'メ' => vec![View::Image(
                        "pixelart/font/katakana/me.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'モ' => vec![View::Image(
                        "pixelart/font/katakana/mo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ヤ' => vec![View::Image(
                        "pixelart/font/katakana/ya.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ユ' => vec![View::Image(
                        "pixelart/font/katakana/yu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ヨ' => vec![View::Image(
                        "pixelart/font/katakana/yo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ラ' => vec![View::Image(
                        "pixelart/font/katakana/ra.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'リ' => vec![View::Image(
                        "pixelart/font/katakana/ri.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ル' => vec![View::Image(
                        "pixelart/font/katakana/ru.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'レ' => vec![View::Image(
                        "pixelart/font/katakana/re.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ロ' => vec![View::Image(
                        "pixelart/font/katakana/ro.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ワ' => vec![View::Image(
                        "pixelart/font/katakana/wa.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ヲ' => vec![View::Image(
                        "pixelart/font/katakana/wo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ン' => vec![View::Image(
                        "pixelart/font/katakana/n.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ァ' => vec![View::Image(
                        "pixelart/font/katakana/xa.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ィ' => vec![View::Image(
                        "pixelart/font/katakana/xi.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ゥ' => vec![View::Image(
                        "pixelart/font/katakana/xu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ェ' => vec![View::Image(
                        "pixelart/font/katakana/xe.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ォ' => vec![View::Image(
                        "pixelart/font/katakana/xo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ャ' => vec![View::Image(
                        "pixelart/font/katakana/xya.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ュ' => vec![View::Image(
                        "pixelart/font/katakana/xyu.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    'ョ' => vec![View::Image(
                        "pixelart/font/katakana/xyo.png".to_string(),
                        x + i * 8,
                        y,
                        z,
                    )],
                    _ => vec![],
                }
            }
        })
        .collect()
}

pub fn text_green(x: i32, y: i32, z: i32, msg: String) -> Vec<View> {
    msg.chars()
        .enumerate()
        .flat_map(|(i, c)| {
            let i = i as i32;

            if 0x20 as char <= c && c < 0x7F as char {
                let c = c as u8;
                vec![View::Image(
                    format!("pixelart/font/ascii_green/{:X?}.png", c),
                    x + i * 8,
                    y,
                    z,
                )]
            } else {
                vec![]
            }
        })
        .collect()
}
