# Lists

Use the appropriate functions when manipulating lists.

For simple access to list data, you can use `car`, `cdr`, `cadr`, etc. to access list elements and segments. For common pattern-matching in function heads, `receive`, `let`, etc., use `cons` to access the head and tail of a list (e.g., `(,head . ,tail)`).

Additionally, don't forget the `lists` Erlang module for accessing list elements.

You should avoid using a list as anything besides a container of elements of like type. Avoid using a list as method of passing multiple separate values of different types in and out of function calls. Sometimes it is convenient to use a list as a little ad hoc structure, i.e. "the first element of the list is a `foo`, and the second is a `bar`", but this should be used minimally since it gets harder to remember the little convention. You should only use a list that way when destructuring the list of arguments from a function, or creating a list of arguments to which to `apply` a function.

The proper way to pass around an object comprising several values of heterogeneous types is to use a record created via `defrecord`.
