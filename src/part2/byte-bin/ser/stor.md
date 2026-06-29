# Storage Patterns: Files and Mnesia

The most common use of serialization is writing to files:

```lfe
(defun save-state (filename state)
  "Persists application state to disk.
  Returns ok on success, which is more optimistic than most things return."
  (let ((binary (term-to-binary state)))
    (file:write_file filename binary)))

(defun load-state (filename)
  "Loads application state from disk.
  May fail if the universe has ended since the last save."
  (case (file:read_file filename)
    ((tuple 'ok binary)
     (tuple 'ok (binary-to-term binary)))
    ((tuple 'error reason)
     (tuple 'error reason))))
```

Usage:

```lfe
lfe> (set game-state
...>   (tuple 'player "Arthur Dent"
...>          'inventory (list 'tea 'towel 'no-tea)
...>          'location "Earth"
...>          'status 'confused))
#(player "Arthur Dent" inventory (tea towel no-tea) ...)

lfe> (save-state "game.sav" game-state)
ok

;; Later, perhaps after Earth's demolition...
lfe> (load-state "game.sav")
#(ok #(player "Arthur Dent" inventory (tea towel no-tea) ...))
```
