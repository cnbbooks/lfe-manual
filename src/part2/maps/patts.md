# Pattern Matching: The Art of Selective Attention

Maps can be destructured in patterns, allowing you to extract precisely the information you need while ignoring the rest with dignified indifference:

```lfe
(defun get-age (person)
  (let (((map 'age age) person))
    age))

;; Or using the match-map form
(defun get-age (person)
  (match-map person
    ((map 'age a) a)))

;; Example usage
(set henry8 '#M(class king born 1491 died 1547))
(get-age henry8)
;; 1491
```

The number of keys in your pattern need not match the number in the map. Maps are generous that way—they don't insist you acknowledge every detail of their existence.
