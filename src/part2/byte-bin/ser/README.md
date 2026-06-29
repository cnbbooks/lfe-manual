# Serialization

> "The history of every major computing civilization tends to move from chaos through complexity to the ultimate simplification of cramming everything into a byte stream and hoping for the best."
> — *Somewhat Improbable Musings on Data Persistence*

There comes a moment in every Erlang (and by extension, LFE) programmer's life when they must confront an uncomfortable truth: not all data can exist in the beautiful, ephemeral world of process memory. Sometimes—and this is where things get philosophically sticky—data must be *serialized*.

Serialization is the process of taking a perfectly happy, multi-dimensional data structure living its best life in RAM and squashing it flat into a sequence of bytes. It's rather like explaining a joke to someone who wasn't there: technically all the information is preserved, but something ineffable is lost in the translation.

Fortunately, LFE (via Erlang's runtime) provides `term-to-binary` and `binary-to-term`, two functions that perform this existential compression and decompression with remarkable grace.
