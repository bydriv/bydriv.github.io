mod A {
  pub mod M {
    pub struct S {}
  }

  pub mod N {
    pub trait T {
      fn f(&self);
    }
  }

  pub mod R {
    use A::M;
    use A::N;

    impl N::T for M::S {
      fn f(&self) {
        println!("hi!");
      }
    }
  }

  pub fn main1() {
    // s.f(); のように書きたいときは、 use N::T; のようにトレイトを使う
    use A::N::T;

    // これはだめ。 impl したモジュールを use するわけではない。もちろん use なしで f は使えない
    // use R;

    // これもだめ。念のため。
    // use M;

    let s = M::S {};
    s.f();
  }

  pub fn main2() {
    use A::M;
    use A::N;

    // use A::N::T; しなくても、クラスメソッド的な呼びかたもできる
    let s = M::S {};
    N::T::f(&s);
  }
}

mod B {
  pub mod M {
    pub struct S {}
  }

  pub mod N {
    pub struct S {}
  }

  pub mod R {
    use B::M;
    use B::N;

    impl M::S {
      pub fn f(&self, x : u32) {
        println!("hi!");
      }
    }

    impl N::S {
      pub fn f(&self, x : &str) {
        println!("hm.");
      }
    }
  }

  pub fn main1() {
    // 必要なし
    // use B::M::S;
    // use B::N::S;

    // s1.f() も s2.f() も use なしで使える
    // 型が違っても問題ない
    // トレイトと表記は似ているが名前の解決がまったく別物
    let s1 = M::S {};
    let s2 = N::S {};
    s1.f(42);
    s2.f("answer");

    // こちらもクラスメソッド的な呼びかたもできる
    M::S::f(&s1, 42);
    N::S::f(&s2, "answer");
  }
}

mod C {
  pub mod M {
    pub trait T {
      fn f(&self, u32);
    }
    pub struct S {}
  }

  pub mod N {
    pub trait T {
      fn f(&self, &str);
    }
    pub struct S {}
  }

  pub mod R {
    use C::M;
    use C::N;

    impl M::T for M::S {
      fn f(&self, x : u32) {
        println!("hi!");
      }
    }

    impl N::T for N::S {
      fn f(&self, x : &str) {
        println!("hm.");
      }
    }
  }

  pub fn main1() {
    // 疑問: ただ単に型を書きたいという目的で trait は使えないのか

    // 必要あり
    use C::M::T as T1;
    use C::N::T as T2;

    // use すれば s1.f() も s2.f() も使える
    // 型が違っても問題ない (なぜ？？？？ Haskell ではこういう解決はできない)
    let s1 = M::S {};
    let s2 = N::S {};

    // C::M::T::f が選ばれる
    s1.f(42);

    // C::N::T::f が選ばれる
    s2.f("answer");

    // f という名前ではなく、 s1/s2 という値の型に応じて選択している
    // Haskell の場合、 \x -> show x みたいなのも推論したいので、
    // show の型を変えることはできない (値の型ではなく名前で選択する)

    // こちらもクラスメソッド的な呼びかたもできる
    M::T::f(&s1, 42);
    N::T::f(&s2, "answer");
  }
}

fn main() {
  A::main1();
  A::main2();
  B::main1();
  C::main1();
}
