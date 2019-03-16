# proptest で異なる言語間の json schema が一致しているか検証する話

**要件**: Rust と他言語(例: TypeScript)間で json をやりとりするため (de)serializer で構造体にマッピングしている。
そこで Rust のモデル定義と他言語のモデル定義が一致しているかどうかをテストしたい。

たとえば、 Rust でこんな感じの型を定義したとして

```rs
#[derive(Serialize, Deserialize, Debug)]
struct Person {
    name: String,
    age: u8,
    phones: Vec<String>,
}
```

ほかの言語が生成する json をこの構造体として受けとったり、逆にこの構造体を json としてほかの言語に与えたい。
言語間のインターフェースは JSON Schema によってつぎのように定義されている:

**schema.json**:

```rs
{
    "type": "object",
    "properties": {
        "name": {
            "type": "string"
        },
        "age": {
            "type": "number"
        },
        "phones": {
            "type": "array",
            "items": {
                "type": "string"
            }
        }
    }
}
```

しかし、 Rust のモデル定義とこの JSON Schema が果たして一致しているかどうかというのは、テストされていない。
そこでテストしたいというわけである。

## 方法

proptest を使用し (Rust の) Person 型の値をランダムに生成し、それを json に変換してみて、
上記の JSON Schema でチェックしてみる。

こんな感じ:

```
proptest! {
    #[test]
    fn test_person(p: Person) {
        use serde_json::Value;
        use valico::json_schema;
        use std::fs::File;

        let json_v4_schema: Value = serde_json::from_reader(File::open("schema.json").unwrap()).unwrap();
        let mut scope = json_schema::Scope::new();
        let schema = scope.compile_and_return(json_v4_schema.clone(), false).unwrap();

        let value = serde_json::from_str(&serde_json::to_string(&p).unwrap()).unwrap();

        assert!(schema.validate(&value).is_valid());
    }
}
```

これを走らせるのは簡単:

```
$ cargo test
running 1 test
test test_person ... ok

test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out

   Doc-tests 2019-03-16-proptest

running 0 tests

test result: ok. 0 passed; 0 failed; 0 ignored; 0 measured; 0 filtered o
```

試しに JSON Schema の age の型を `"number"` から `"string"` に変更してみる。

```
$ sed -i 's/"number"/"string"/' schema.json
$ cargo test
running 1 test
test test_person ... FAILED

failures:

---- test_person stdout ----
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
note: Run with `RUST_BACKTRACE=1` for a backtrace.
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'assertion failed: schema.validate(&value).is_valid()', src/lib.rs:31:9
thread 'test_person' panicked at 'Test failed: assertion failed: schema.validate(&value).is_valid(); minimal failing input: p = Person { name: "", age: 0, phones: [] }
        successes: 0
        local rejects: 0
        global rejects: 0
', src/lib.rs:18:1


failures:
    test_person

test result: FAILED. 0 passed; 1 failed; 0 ignored; 0 measured; 0 filtered out

error: test failed, to rerun pass '--lib'
```

これで「Rust の構造体として妥当ならば schema.json としても妥当である」ことがテストできた。
できれば「schema.json として妥当ならば Rust の構造体としても妥当である」こともテストしたい。
しかし、実用上は前者だけでもじゅうぶん効果的なので、ひとまずここまでできただけでもよしとする。
