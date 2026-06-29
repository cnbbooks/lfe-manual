# What Binaries Are

Before we start constructing and dissecting binaries with reckless abandon, it's worth pausing to understand what they actually *are*—in memory, on the page, and in relation to their slightly less respectable cousins, the bitstrings. The sections that follow examine the nature of the beast, how the runtime represents binaries, how the REPL displays them, the binary/bitstring distinction, why you'd reach for a binary instead of a list, and the immutability that makes all of it safe to pass between processes without a second thought.
