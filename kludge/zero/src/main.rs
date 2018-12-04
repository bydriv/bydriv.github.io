extern crate rand;
use rand::{Rng};
use rand::prelude::{ThreadRng};

/// 認証局
/// 情報は公開される
struct Center {
  q  : u32,                  /// q <- Z
  Zq : std::ops::Range<u32>, /// Zq = [0, q)
  g  : u64,                  /// G = {g^0, g^1, .., g^{q-1}}
  h  : u64,                  /// h = g^s (公開鍵)
}

/// アリス
/// 情報はアリスのみぞ知る
struct Alice {
  s : u32 /// s <- Zq (秘密情報)
}

/// アリスは s を知っているが、ボブは s を知らない
/// アリスは s をボブに知らせずに(ゼロ知識性)
/// ボブに "アリスは s を知っている" ことを証明したい
/// (ボブは s を知らずに、 "アリスが s を知っている" かどうか検証したい)
fn proof(rng : &mut ThreadRng, center : &Center, alice : &Alice) -> f64 {
  let n : u32 = 100;
  let mut success : u32 = 0;

  for _ in 0..n {
    /// アリスの証明手順1: x = g^r を生成
    /// このとき、 r をボブに知らせてはいけない
    let (r, x) = proof1(rng, center, alice);

    /// ボブの検証手順1: c <- {0, 1} をランダムに生成
    let c = verify1(rng, center, x);

    /// アリスの証明手順2: y = (r + sc) mod q を計算
    let y = proof2(rng, center, alice, r, x, c);

    /// ボブの検証手順2: g^y = xh^c が成立するか検証
    let result = verify2(rng, center, x, c, y);

    if result {
      success += 1;
    }
  }

  (success as f64) / (n as f64)
}

/// アリスの証明手順1: x = g^r を生成
/// このとき、 r をボブに知らせてはいけない
fn proof1(rng : &mut ThreadRng, center : &Center, alice : &Alice) -> (u32, u64) {
  let r = rng.gen_range(0, center.q);
  let x = center.g.pow(r);
  (r, x)
}

/// ボブの検証手順1: c <- {0, 1} をランダムに生成
fn verify1(rng : &mut ThreadRng, center : &Center, x : u64) -> u32 {
  rng.gen_range(0, 2)
}

/// アリスの証明手順2: y = (r + sc) mod q を計算
fn proof2(rng : &mut ThreadRng, center : &Center, alice : &Alice, r : u32, x : u64, c : u32) -> u32 {
  let y = (r + alice.s * c) % center.q;
  y
}

/// ボブの検証手順2: g^y = xh^c が成立するか検証
fn verify2(rng : &mut ThreadRng, center : &Center, x : u64, c : u32, y : u32) -> bool {
  center.g.pow(y) == x * center.h.pow(c)
}

/// キャロルもボブも s を知らない
/// キャロルはボブに "キャロルは s を知っている" ことを証明したい
/// (ボブは s を知らずに、 "キャロルが s を知っている" かどうか検証したい)
fn attack(rng : &mut ThreadRng, center : &Center) -> f64 {
  let n : u32 = 100;
  let mut success : u32 = 0;

  for _ in 0..n {
    /// キャロルの攻撃手順1: y <- Zq を生成し、 x = (g^y)/h を計算
    let (x, y) = attack1(rng, center);

    /// ボブの検証手順1: c <- {0, 1} をランダムに生成
    let c = verify1(rng, center, x);

    /// キャロルの攻撃手順2: 手順1 で生成した y を返す
    let y = attack2(rng, center, x, y, c);

    /// ボブの検証手順2: g^y = xh^c が成立するか検証
    let result = verify2(rng, center, x, c, y);

    if result {
      success += 1;
    }
  }

  (success as f64) / (n as f64)
}

/// キャロルの攻撃手順1: y <- Zq を生成し、 x = (g^y)/h を計算
fn attack1(rng : &mut ThreadRng, center : &Center) -> (u64, u32) {
  let y = rng.gen_range(0, center.q);
  let x = center.g.pow(y) / center.h;
  (x, y)
}

/// キャロルの攻撃手順2: 手順1 で生成した y を返す
fn attack2(rng : &mut ThreadRng, center : &Center, x : u64, y : u32, c : u32) -> u32 {
  y
}

fn alice_proof(rng : &mut ThreadRng, center : &Center, alice : &Alice) {
  println!("Alice: {}%", proof(rng, center, alice) * 100.0);
}

fn carol_attack(rng : &mut ThreadRng, center : &Center) {
  println!("Carol: {}%", attack(rng, center) * 100.0);
}

fn main() {
  let mut rng = rand::thread_rng();

  let (center, alice) = {
    let q = 30;
    let Zq = 0..q;
    let g : u64 = 2;
    let s = rng.gen_range(0, q);
    let h = g.pow(s);
    let center = Center { q: q, Zq: Zq, g: g, h: h };
    let alice = Alice { s: s };
    (center, alice)
  };

  alice_proof(&mut rng, &center, &alice);
  carol_attack(&mut rng, &center);
}
