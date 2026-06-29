# Don't Care Variables

Sometimes you need to skip over fields you don't care about. Use the anonymous variable `_` for this:

```lfe
(defun extract-timestamp (log-entry)
  "Extract just the timestamp, ignoring other fields."
  (binary ((_ (size 16))              ; Skip magic number
           (timestamp (size 64))       ; Extract timestamp
           (_ binary))                 ; Ignore the rest
          log-entry)
  timestamp)
```

This is the binary equivalent of the "skip to the good part" button on a remote control, except more reliable and less likely to cause arguments.
