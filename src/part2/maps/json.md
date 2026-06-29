# The JSON Connection

Maps map naturally (pun absolutely intended) to JSON objects, making them ideal for web APIs and data interchange:

```lfe
;; Convert map to JSON
(maps:to_json '#M(name "Arthur" age 42 hobbies ("tea" "towels")))
;; #B("{\"age\":42,\"hobbies\":[\"tea\",\"towels\"],\"name\":\"Arthur\"}")

;; Convert JSON to map
(maps:from_json #B("{\"name\":\"Ford\",\"species\":\"Betelgeusian\"}"))
;; #M(name "Ford" species "Betelgeusian")
```

The `safe_from_json/1` variant protects against atom exhaustion attacks by requiring all atoms to exist before conversion—a sensible precaution when dealing with external data.
