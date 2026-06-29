# Example 1: Dissecting IPv4 Datagrams

The Internet Protocol version 4 (IPv4) has been shuffling packets around the globe since 1981, which in computing terms makes it roughly as ancient as the pyramids. Its header format is a masterclass in bit-level data packing, born from an era when every bit mattered because memory was measured in kilobytes and network bandwidth in... well, let's just say "optimistically."

## The IPv4 Header: A Map of the Territory

An IPv4 header looks like this (each row is 32 bits):

```
 0                   1                   2                   3
 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|Version|  IHL  |Type of Service|          Total Length         |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|         Identification        |Flags|      Fragment Offset    |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|  Time to Live |    Protocol   |         Header Checksum       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                       Source Address                          |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    Destination Address                        |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
|                    Options (if IHL > 5)                       |
+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
```

Notice how nothing lines up nicely on byte boundaries except by accident. The Flags field is 3 bits. THREE BITS. The Fragment Offset is 13 bits. This is what protocols designed by actual engineers look like, as opposed to the sort designed by people who think everything should align on 64-bit boundaries because it's "cleaner."

## Parsing IPv4 in LFE: One Pattern to Rule Them All

Here's how we parse this in LFE, using the bit syntax to handle all those awkwardly-sized fields:

```lfe
;; Constants, because magic numbers are poor documentation
(defun ipv4-version () 4)
(defun ipv4-min-header-length () 5)  ; In 32-bit words

(defun parse-ipv4-datagram (datagram)
  "Parses an IPv4 datagram into its constituent parts.
  Returns a tuple of parsed fields, or 'error if the datagram is malformed.
  Does not validate checksums, because this is an example, not production code.
  (Though in fairness, production code often skips that too.)"
  (let ((dgram-size (byte_size datagram)))
    (case datagram
      ;; The big pattern match that does all the work
      ((binary (version 4) (header-length 4) (service-type 8) (total-length 16)
               (identification 16) (flags 3) (fragment-offset 13)
               (time-to-live 8) (protocol 8) (header-checksum 16)
               (source-ip 32) (dest-ip 32)
               (rest binary))
       ;; Validate header length
       (if (and (>= header-length (ipv4-min-header-length))
                (<= (* 4 header-length) dgram-size))
           ;; Calculate options size and extract
           (let* ((opts-len (* 4 (- header-length (ipv4-min-header-length))))
                  ((binary (options opts-len binary) (payload binary)) rest))
             (tuple 'ok
                    (map 'version version
                         'header-length header-length
                         'service-type service-type
                         'total-length total-length
                         'identification identification
                         'flags (parse-flags flags)
                         'fragment-offset fragment-offset
                         'ttl time-to-live
                         'protocol (protocol-name protocol)
                         'checksum header-checksum
                         'source (ip-to-string source-ip)
                         'destination (ip-to-string dest-ip)
                         'options options
                         'payload payload)))
           'error))
      ;; Anything else is malformed
      (_ 'error))))

(defun parse-flags (flags-bits)
  "Parses the 3-bit flags field.
  Bit 0: Reserved (must be zero, or RFC police will write stern letters)
  Bit 1: Don't Fragment (DF)
  Bit 2: More Fragments (MF)"
  (let (((binary (reserved 1) (dont-fragment 1) (more-fragments 1))
         (binary (flags-bits 3))))
    (map 'reserved (== reserved 1)
         'dont-fragment (== dont-fragment 1)
         'more-fragments (== more-fragments 1))))

(defun protocol-name (protocol-num)
  "Maps protocol numbers to names.
  Only handles the interesting ones. There are 256 possible values,
  and frankly, most of them are boring or deprecated."
  (case protocol-num
    (1 'icmp)   ; For when you want to ping something
    (6 'tcp)    ; The reliable one
    (17 'udp)   ; The fast one
    (41 'ipv6)  ; The future, allegedly
    (58 'ipv6-icmp)
    (n (tuple 'unknown n))))

(defun ip-to-string (ip-int)
  "Converts a 32-bit IP address to dotted-decimal notation.
  Returns a string, because that's how humans like to read IP addresses."
  (let (((binary (a 8) (b 8) (c 8) (d 8)) (binary (ip-int 32))))
    (lists:flatten (io_lib:format "~w.~w.~w.~w" (list a b c d)))))
```

Let's see this in action with a real packet (or at least, a facsimile thereof):

```lfe
lfe> ;; Construct a minimal IPv4 packet
lfe> ;; Source: 192.168.1.42, Dest: 10.0.0.1, TCP packet, TTL=64
lfe> (set test-packet
...>   (binary
...>     (4 4)           ; Version 4, header length 5 words
...>     (0 8)           ; Service type
...>     (60 16)         ; Total length (including payload)
...>     (12345 16)      ; Identification
...>     (0 3) (0 13)    ; Flags and fragment offset
...>     (64 8)          ; TTL
...>     (6 8)           ; Protocol (TCP)
...>     (0 16)          ; Checksum (we're lazy)
...>     ;; Source IP: 192.168.1.42
...>     (192 8) (168 8) (1 8) (42 8)
...>     ;; Dest IP: 10.0.0.1
...>     (10 8) (0 8) (0 8) (1 8)
...>     ;; Minimal TCP payload (not parsed here)
...>     "Hello, packet-switching world!"))

lfe> (parse-ipv4-datagram test-packet)
#(ok
  #M(version 4
     header-length 5
     service-type 0
     total-length 60
     identification 12345
     flags #M(reserved false dont-fragment false more-fragments false)
     fragment-offset 0
     ttl 64
     protocol tcp
     checksum 0
     source "192.168.1.42"
     destination "10.0.0.1"
     options #B()
     payload #B(72 101 108 108 111 ...)))
```

## What This Demonstrates

1. **Non-byte-aligned fields**: The flags (3 bits) and fragment offset (13 bits) fields don't align on byte boundaries, but the bit syntax handles this transparently.

2. **Conditional parsing**: The options field size depends on the header length field that was parsed earlier.

3. **Nested patterns**: We use pattern matching twice—once for the main header, once for the flags.

4. **Real-world messiness**: IP headers have evolved over decades, with various fields falling into disuse while remaining in the spec for backwards compatibility.
