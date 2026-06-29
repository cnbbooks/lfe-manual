# Practical Examples: Tuples in the Wild

Let's conclude with some real-world examples of tuple usage, because theory is all well and good until you actually need to store a person's favorite color.

## Example 1: HTTP Responses

```lfe
;; Representing HTTP responses as tuples
(defun handle-request (url)
  (case (fetch-url url)
    (#(ok body)
     (process-body body))
    (#(error 'not-found)
     (show-404-page))
    (#(error 'timeout)
     (try-again-later))
    (_ (show-generic-error))))
```

## Example 2: Coordinate Systems

```lfe
;; 2D points
(set origin #(point 0 0))
(set corner #(point 100 100))

;; 3D points with tagged fields
(set spaceship-location
  #(position
    #(x 42.5)
    #(y -15.8)
    #(z 1337.0)
    #(velocity #(30 0 -5))))
```

## Example 3: Return Values

```lfe
(defun divide (a b)
  (if (=:= b 0)
    #(error 'division-by-zero)
    #(ok (/ a b))))

;; Usage:
(case (divide 10 2)
  (#(ok result)
   (io:format "Result: ~p~n" (list result)))
  (#(error reason)
   (io:format "Error: ~p~n" (list reason))))
```
