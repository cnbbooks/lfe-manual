# Choosing Your Key-Value Store: A Decision Framework

## In Which We Synthesize Our Options Into Actionable Wisdom

Having examined three different approaches to the "I have keys, I have values, now what?" problem, let's construct a decision tree:

### For Small Datasets (< 75 pairs)

**Use orddict** if:
- You want transparency
- You're debugging
- Human readability matters
- You need ordered output
- Simplicity trumps everything

**Use maps** if:
- You want modern syntax
- Pattern matching would be useful
- You're writing new code
- Integration with other modern code matters

### For Medium Datasets (75-10,000 pairs)

**Use maps** unless:
- You're maintaining legacy code (then use what's there)
- You specifically need gb_trees features

**Use gb_trees** if:
- You need min/max operations
- Ordered iteration is central to your algorithm
- You're implementing a tree-based algorithm

**Avoid**:
- orddict (too slow)
- dict (deprecated by maps)

### For Large Datasets (10,000+ pairs)

**Use maps** unless:
- You have specific needs only gb_trees fulfill
- You're maintaining legacy dict code

**Use gb_trees** if:
- Min/max operations are critical
- Your algorithm fundamentally requires tree operations
- You've benchmarked and gb_trees is measurably better

**Never use**:
- orddict (will ruin your performance)
- dict (unless forced to by legacy constraints)

### Special Considerations

**Use dict** only if:
- You're maintaining pre-R17 code
- You must support Erlang versions without maps
- You're gradually migrating to maps and need interim compatibility

**Use gb_trees** even in preference to maps when:
- Finding minimum/maximum keys is a frequent operation
- You need ordered traversal without separate sorting
- Your algorithm is naturally tree-based
- You've measured and gb_trees performs better for your specific use case

## A Note On Proplists

The document you provided mentions proplists, which are worth a brief note here. A proplist is simply a list of tuples: `'(#(key1 val1) #(key2 val2))`. They're not a formal data structure so much as a common pattern, particularly for passing options to functions.

Proplists are appropriate for:
- Function arguments with optional parameters
- Configuration that humans will write
- Very small datasets where the loose structure is acceptable
- Compatibility with code expecting this pattern

For anything more structured, use one of the modules discussed above.

## Performance Isn't Everything

A final philosophical point: the fastest data structure is not always the best choice. Consider:

- **Maintainability**: Code using orddict is often clearer than code using gb_trees
- **Debuggability**: Transparent structures are easier to inspect
- **Integration**: Matching your team's conventions matters
- **Future-proofing**: Maps are the present and future; choose them when practical

Premature optimization remains, as ever, a source of evil. Choose the simplest structure that meets your needs, measure if performance matters, and optimize only when measurement proves it necessary.
