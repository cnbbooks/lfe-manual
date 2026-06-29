# A Note on Immutability and Illusions

Unlike JavaScript or Python dictionaries, Erlang maps are immutable. When you "update" a map, you create a new map. The VM optimizes this behind the scenes through structural sharing, so it's nowhere near as expensive as it sounds. 

This immutability prevents the sorts of aliasing bugs that plague mutable languages—you can't accidentally change a map through a reference held by another part of your program. Each variable maintains its own consistent view of reality, which is rather nice when debugging at 3 AM.

As one text eloquently puts it: "The Erlang system copies only those parts of the internal structures necessary to maintain the illusion that a copy has been created, so creating what appears to be deep copies of an object is an extremely lightweight operation."
