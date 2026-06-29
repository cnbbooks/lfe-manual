# Practical Patterns: Working With Records Idiomatically

## The Builder Pattern

```lfe
(defun make-person-from-data (data)
  (make-person 
    name (proplists:get_value 'name data "Unknown")
    age (proplists:get_value 'age data 0)
    phone (proplists:get_value 'phone data "")))
```

## The Updater Pattern

```lfe
(defun update-if-valid (person new-age)
  (if (>= new-age 0)
    (set-person-age person new-age)
    person))
```

## The Transformer Pattern

```lfe
(defun person-to-proplist
  (((match-person name n age a phone p))
   `(#(name ,n) #(age ,a) #(phone ,p))))
```
