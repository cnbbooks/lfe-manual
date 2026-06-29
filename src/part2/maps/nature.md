# The Fundamental Nature of Maps (Or: What Maps Actually Are When Nobody's Looking)

Maps possess several properties that distinguish them from the lesser data structures wandering about the BEAM:

* Keys can be any fully ground term. Not mostly ground. Not nearly ground. Not "I'm working on it" ground. Fully ground—meaning no unbound variables lurking in the shadows like existential uncertainties.

* Elements are ordered by their keys, which means maps have opinions about sequence, much like a particularly fastidious librarian.

* Updating a map without changing its keys is space-efficient, a bit like renovating your house without moving to a new address.

* Looking up values is efficient, which is computer science speak for "pleasantly quick without making a fuss about it."

* Maps have well-defined ordering semantics, meaning you can compare them without philosophical angst.
