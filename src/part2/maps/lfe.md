# LFE-Specific Conveniences

LFE provides several forms that make working with maps more pleasant than it has any right to be:

## The (map) Constructor
```lfe
(map 'a 1 'b 2 'c 3)
#M(a 1 b 2 c 3)
```

## The (mref Map Key) Function
Safely access values with optional defaults:
```lfe
(mref '#M(a 1 b 2) 'a)
1

(mref '#M(a 1 b 2) 'z 'default)
default
```

## The (mset Map Key Value) Function
Update or add a key-value pair:
```lfe
(mset '#M(a 1 b 2) 'c 3)
#M(a 1 b 2 c 3)
```

## The (mupd Map Key Value) Function
Update an existing key (fails if key doesn't exist):
```lfe
(mupd '#M(a 1 b 2) 'a 42)
#M(a 42 b 2)
```

## The (map-get Map Key) Macro
Pattern matching extraction in let bindings:
```lfe
(let (((map-get '#M(a 1 b 2) a val)))
  val)
1
```
