# Creating Proplists

Creating a proplist requires no special syntax, no macro invocations, no appeals to higher powers. You simply construct a list with the appropriate structure:

```lfe
lfe> '((active true) (buffer_size 1024) read write binary)
((active true) (buffer_size 1024) read write binary)

lfe> (list '(timeout 5000) 'async '(encoding utf8))
((timeout 5000) async (encoding utf8))

lfe> (cons '(priority high) '((retries 3) verbose))
((priority high) (retries 3) verbose)
```

Notice how boolean options can be expressed as bare atoms (`read`, `write`, `binary`), which will be automatically treated as `{read, true}`, `{write, true}`, and `{binary, true}` when accessed through the `proplists` module. This is convenience masquerading as syntax.

Options passed to Erlang's standard library functions demonstrate the proplist philosophy in action:

```lfe
;; File operations
(file:open "towel.txt" '(read write append (encoding utf8)))

;; Network options
(inet:setopts socket '((active true) binary (packet 4)))
```

The mixing of tuple pairs and bare atoms creates a pleasant visual heterogeneity—the data structure equivalent of a well-curated bookshelf where some books stand upright while others lie flat.
