# The Shell and Records: A Slightly Awkward Relationship

Records are a compile-time feature—they're entirely transformed into tuples before your code runs. This creates an interesting situation in the REPL, where there is no compile time, only an eternal present tense of evaluation.

To use records in the shell, you must first import the record definitions from a module. In LFE, you do this by loading the module and then using the `(record-name ...)` syntax directly, as the record definitions come along for the ride:

```lfe
lfe> (c "robots.lfe")
#(module robots)

lfe> (make-robot name "Mechatron" type 'handmade)
#(robot "Mechatron" handmade undefined ())
```

The shell understands record syntax once the module is loaded, which is convenient if not particularly magical.
