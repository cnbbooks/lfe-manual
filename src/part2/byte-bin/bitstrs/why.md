# Why Bitstrings Matter

Bitstrings exist because the universe of data is not always byte-aligned. Network protocols pack bits tightly to save bandwidth. Compression algorithms produce variable-length codes. Hardware interfaces have registers with odd bit widths. Video codecs use fractional-bit encoding schemes (not literally fractional bits, but encoding schemes where different symbols use different numbers of bits).

Without bitstrings, you'd be forced to:

1. Manually shift and mask bits (error-prone, unreadable)
2. Waste space padding everything to bytes (inefficient)
3. Give up and accept that some things are just hard (admitting defeat)

LFE's bitstring support means you can work at whatever granularity the problem demands, rather than being forced into the byte-sized boxes that most languages provide.
