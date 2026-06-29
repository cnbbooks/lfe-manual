# In Conclusion: The Legacy Lives On

The three modules we've examined—orddict, dict, and gb_trees—represent different eras and philosophies in Erlang's evolution. Orddict championed transparency. Dict championed pragmatic performance. GB_trees championed algorithmic elegance. Maps, the newcomer, have usurped most of their use cases but haven't eliminated the need for any of them.

Understanding these modules gives you:
1. The ability to maintain legacy code
2. Options when maps don't quite fit
3. Appreciation for how Erlang's abstractions have evolved
4. Tools for those rare cases where their specific strengths matter

Use maps for new code, but know that orddict, dict, and gb_trees stand ready should you need them. They're not deprecated, not obsolete, and not broken—merely specialized. In software as in life, having options is valuable even when you rarely exercise them.

The answer to the ultimate question of legacy key-value stores, the universe, and everything is not 42. It's "it depends on your requirements, dataset size, and whether you need ordered access."

Which is considerably less pithy, but more useful.
