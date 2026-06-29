# The JSON Connection

Unlike maps, which map naturally to JSON objects, proplists require translation. The common pattern is:

```lfe
;; Proplist to JSON (via map)
(defun proplist-to-json (plist)
  (jiffy:encode 
    (proplists:to_map plist)))

;; JSON to proplist (via map)
(defun json-to-proplist (json)
  (proplists:from_map 
    (jiffy:decode json '(return_maps))))
```

This works, but it's not seamless. If you're building web APIs or working with JSON extensively, consider using maps directly rather than converting back and forth.
