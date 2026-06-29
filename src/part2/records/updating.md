# Updating Records: The Philosophy of Selective Modification

Records are immutable (as everything in functional programming must be, by ancient decree), so "updating" means creating a new record with some fields changed:

```lfe
(defun update-contact (person new-phone)
  (set-person-phone person new-phone))
```

The `set-person-phone` function is another compiler-generated convenience. It creates a new record copying all fields from the original except `phone`, which gets the new value.

You can update multiple fields at once using the `set` macro:

```lfe
(set-person person
  age 37
  phone "999-999")
```

This is particularly useful when you need to update several fields and want to avoid a chain of nested `set-person-fieldname` calls that would make even a Lisp enthusiast's eyes water.
