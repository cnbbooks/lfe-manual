# Pattern Matching in Function Clauses

Pattern matching binaries truly shines when used in function clauses, allowing you to handle different packet types with elegant dispatch:

```lfe
(defun handle-message
  ; Ping message
  ([(binary (#x01 rest binary))]
   `#(ping ,rest))

  ; Pong message
  ([(binary (#x02 rest binary))]
   `#(pong ,rest))

  ; Data message with length prefix
  ([(binary (#x03 (len (size 16) big) (payload (size len) binary) rest binary))]
   `#(data ,payload ,rest))

  ; Unknown message type
  ([(binary (type rest binary))]
   `#(unknown ,type ,rest)))
```

Each clause matches a different binary pattern, and the first matching clause wins. This is protocol parsing at its most readable:

```lfe
lfe> (handle-message (binary (#x01 #"PING DATA")))
#(ping #B(80 73 78 71 32 68 65 84 65))

lfe> (handle-message (binary (#x03 (5 (size 16) big) #"HELLO" #"EXTRA")))
#(data #B(72 69 76 76 79) #B(69 88 84 82 65))
```
