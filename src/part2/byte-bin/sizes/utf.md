# A Note on UTF Strings

The UTF-8, UTF-16, and UTF-32 types are special cases that require no unit specification. They know their own sizes based on the character encoding, which is rather like how a towel knows it should be exactly towel-sized:

```lfe
lfe> (binary ((#"Hello" utf8)))
#B(72 101 108 108 111)

lfe> (binary ((#"你好" utf8)))
#B(228 189 160 229 165 189)  ; Chinese "hello" in UTF-8
```
