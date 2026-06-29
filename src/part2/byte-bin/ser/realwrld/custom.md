# Example 2: Building a Custom Binary Protocol

Let's design something more modern: a protocol for communicating with an army of towel-dispensing robots. (Every good spacecraft needs one.)

## The Towel Service Protocol (TSP)

Our protocol needs to handle:

- Status queries
- Towel requests (with size preferences)
- Inventory updates
- Emergency towel deployment

We'll use a simple framed format:

```
+--------+--------+--------+--------+--------+---...---+
| Magic  | Version| MsgType| Length | MsgID  | Payload |
| (16)   | (8)    | (8)    | (16)   | (32)   | (...)   |
+--------+--------+--------+--------+--------+---...---+
```

Here's our LFE implementation:

```lfe
(defmodule towel-protocol
  (export (encode-message 3)
          (decode-message 1)
          (create-status-query 1)
          (create-towel-request 3)
          (create-inventory-update 2)))

;; Protocol constants
(defun magic-number () 16#FADE)  ; Frood-Approved Data Exchange
(defun protocol-version () 1)

;; Message types
(defun msg-status-query () 1)
(defun msg-towel-request () 2)
(defun msg-inventory-update () 3)
(defun msg-emergency-deploy () 255)

(defun encode-message (msg-type msg-id payload)
  "Encodes a message into TSP binary format.
  Returns a binary ready for transmission.
  Does not include shipping charges or existential considerations."
  (let* ((payload-bin (term-to-binary payload))
         (payload-len (byte_size payload-bin)))
    (binary
      ((magic-number) 16)
      ((protocol-version) 8)
      (msg-type 8)
      (payload-len 16)
      (msg-id 32)
      (payload-bin binary))))

(defun decode-message (binary)
  "Decodes a TSP message.
  Returns (tuple 'ok msg-type msg-id payload) or 'error.
  May return 'error if the magic number is wrong, which suggests
  either transmission errors or you're not talking to a towel robot."
  (case binary
    ((binary
       ((magic-num 16) (version 8) (msg-type 8)
        (payload-len 16) (msg-id 32)
        (payload-bin payload-len binary)))
     (if (and (== magic-num (magic-number))
              (== version (protocol-version)))
         (tuple 'ok msg-type msg-id (binary-to-term payload-bin))
         (tuple 'error 'invalid-magic-or-version)))
    (_
     (tuple 'error 'malformed-message))))

;; Message constructors
(defun create-status-query (msg-id)
  "Creates a status query message.
  Robots love being asked how they're doing. It's only polite."
  (encode-message (msg-status-query) msg-id (tuple 'query 'status)))

(defun create-towel-request (msg-id size color)
  "Creates a towel request message.
  Size: 'small | 'medium | 'large | 'hoopy-frood-sized
  Color: An atom representing a color, preferably one that exists"
  (encode-message (msg-towel-request) msg-id
                  (tuple 'request 'towel
                         (map 'size size 'color color))))

(defun create-inventory-update (msg-id inventory-map)
  "Creates an inventory update message.
  inventory-map should be a map of towel-type to count."
  (encode-message (msg-inventory-update) msg-id
                  (tuple 'update 'inventory inventory-map)))
```

Usage example:

```lfe
lfe> ;; Create a towel request
lfe> (set request (towel-protocol:create-towel-request 42 'large 'mostly-harmless-blue))
#B(250 222 1 2 0 42 0 0 0 42 131 104 3 ...)

lfe> ;; Decode it (simulating receiving the message)
lfe> (towel-protocol:decode-message request)
#(ok 2 42
  #(request towel
    #M(size large color mostly-harmless-blue)))

lfe> ;; Create an inventory update
lfe> (set inventory-data
...>   (map 'standard 42
...>        'deluxe 7
...>        'emergency 3
...>        'substandard 0))  ; We don't stock these
#M(standard 42 deluxe 7 emergency 3 substandard 0)

lfe> (set update (towel-protocol:create-inventory-update 43 inventory-data))
#B(250 222 1 3 0 47 0 0 0 43 131 116 ...)

lfe> (towel-protocol:decode-message update)
#(ok 3 43
  #(update inventory
    #M(standard 42 deluxe 7 emergency 3 substandard 0)))
```

## Why This Design Works

1. **Magic number**: Helps detect when you're trying to parse the wrong kind of data
2. **Version field**: Allows protocol evolution
3. **Length prefix**: Enables streaming protocols where messages arrive in chunks
4. **Message ID**: Supports request/response correlation
5. **Flexible payload**: Using `term-to-binary` means we can send any Erlang term as payload

This pattern—a simple fixed header followed by a length-prefixed payload—is extremely common in protocol design. It's used by:

- Many RPC systems
- Message queue protocols (like AMQP)
- Database wire protocols
- Custom application protocols
