# Variables in Segments

You can use variables in segment specifications:

```lfe
lfe> (let ((x 42)
        (y 17))
    (binary (x (size 8)) (y (size 8))))
#B(42 17)
```

The variable must be bound at the point it's used. Attempting to use unbound variables will result in the usual "unbound variable" error, which is the compiler's polite way of saying "I have no idea what you're talking about and neither, I suspect, do you."

More interestingly, you can use variables for *sizes* in pattern matching, which allows extraordinarily flexible binary parsing:

```lfe
(let (((binary (size (size 8)) (data (size size) binary)) some-packet))
  data)
```

Here, the first byte tells us the size, and then we extract exactly that many bytes into `data`. The size is used both to extract a value (the first 8 bits) and to control the size of a subsequent segment. This kind of self-describing binary format is common in network protocols, where the packet tells you how long it is so you know when you've received all of it.

This is pattern matching at its finest: you're not writing parsing code, you're *describing* the structure of the data, and the system figures out how to extract the values. It's declarative programming for binary formats, which sounds like it should be impossible until you realize it works perfectly well and has done so for decades.
