# Maps vs. Proplists: The Evolution of Options

Proplists have served Erlang well for passing options, but they're showing their age. Consider:

```erlang
% Erlang proplist style
[{active, true}, binary, {timeout, 5000}]

% Maps style
#{active => true, binary => true, timeout => 5000}
```

Maps provide simpler access patterns (`Opts#{timeout}` rather than `proplists:get_value(timeout, Opts, Default)`) and the merge/2 function elegantly handles defaults. However, literal options like `read`, `write`, and `binary` look cleaner in proplist form. 

The ecosystem is gradually transitioning. For new code, consider using maps for options that are mostly pairs, and reserve proplists for compatibility or when literal atoms dominate.
