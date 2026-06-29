# The Secret Sauce: What Gets Preserved

The truly remarkable thing about `term-to-binary` is that it preserves *everything*:

- **Atoms** remain atoms
- **Numbers** (integers, floats, bignums) maintain their precision
- **Tuples** preserve their structure and arity
- **Lists** (proper and improper) keep their shape
- **Binaries** get nested (yes, really—binaries within binaries)
- **PIDs, ports, and references** get special treatment in distributed contexts

The only things that *don't* serialize well are:

1. **Functions/lambdas**: These are code, not data. You can't serialize a function and expect it to work on another machine that doesn't have that code.
2. **Open file handles**: These are OS resources and don't travel well.
3. **The improbability drive**: This was never specified in the original ETF, a notable oversight.
