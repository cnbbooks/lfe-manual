# Special Variables

```lisp
LFE shell built-in variables

+/++/+++      -- the tree previous expressions
*/**/***      -- the values of the previous expressions
-             -- the current expression output
$ENV          -- the current LFE environment
```

Most of these variables are taken directly from Common Lisp and have the same exact meaning. From the Common Lisp HyperSpec for [`+,++,+++`](http://www.lispworks.com/documentation/HyperSpec/Body/v_pl_plp.htm):

> The variables +, ++, and +++ are maintained by the Lisp read-eval-print loop to save forms that were recently evaluated.
>
> The value of + is the last form that was evaluated, the value of ++ is the previous value of +, and the value of +++ is the previous value of ++.

And for [`*,**,***`](http://www.lispworks.com/documentation/HyperSpec/Body/v__stst_.htm):

> The variables *, **, and *** are maintained by the Lisp read-eval-print loop to save the values of results that are printed each time through the loop.
>
> The value of * is the most recent primary value that was printed, the value of ** is the previous value of *, and the value of *** is the previous value of **.

Lastly, for [`-`](http://www.lispworks.com/documentation/HyperSpec/Body/v__.htm):

> The value of - is the form that is currently being evaluated by the Lisp read-eval-print loop.

The `$ENV` variable in the LFE REPL is a critical tool for debugging particularly tricky issues in the REPL (especially useful when creating complex macros).
