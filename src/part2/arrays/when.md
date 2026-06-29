# When to Use Arrays

The crucial question remains: when should you actually use arrays instead of the more idiomatic lists, tuples, or maps?

**Use arrays when:**

- You need indexed access to a large collection with changing size
- You're implementing algorithms that naturally think in terms of arrays
- You need to grow a fixed-index structure dynamically
- You're translating code from imperative languages and arrays make the translation clearer

**Avoid arrays when:**

- You're primarily doing sequential processing (use lists)
- You have a fixed number of elements (use tuples)
- You need key-value storage with non-numeric keys (use maps)
- You need actual constant-time indexed access (consider port drivers or NIFs)

In practice, arrays are rarely seen in idiomatic Erlang/LFE code. They occupy a niche for specific use cases where the alternatives are genuinely worse, rather than just different. Think of them as the specialized tool you keep in the back of the toolbox—you're glad it's there when you need it, but most days you reach for something else.
