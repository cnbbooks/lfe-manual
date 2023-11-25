# Intent not Content

You should name a variable according to the high-level concept that it represents (intent), not according to the low-level implementation details of how the concept is represented (content).

Thus, you should avoid embedding data structure or aggregate type names, such as `list`, `array`, or `hash-table` as part of the variable names, unless you're writing a generic algorithm that applies to arbitrary lists, arrays, hash-tables, etc. In that case it's perfectly OK to name a variable `list` or `array`.

For example, if a variable's value is always a row (or is either a row or NIL), it's good to call it `row` or `first-row` or something like that. 

*Be consistent*. If a variable is named `row` in one function, and its value is being passed to a second function, then call it `row` rather than, say, `value`.
