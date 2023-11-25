# Attention Required

For comments requiring special attention, such as incomplete code, todo items, questions, breakage, and danger, include a `TODO` or `XXX` comment indicating the type of problem, its nature, and any notes on how it may be addressed.

The comments begin with `TODO` or `XXX` in all capital letters, followed by the name, e-mail address, or other identifier of the person with the best context about the problem referenced by the `TODO` or `XXX`. The main purpose is to have a consistent `TODO` or `XXX` that can be searched to find out how to get more details upon request. A `TODO` or `XXX` is not a commitment that the person referenced will fix the problem. Thus when you create a `TODO` or `XXX`, it is almost always your name that is given.

Generally, `TODO` and `XXX` commands are differentiated in that `TODO` items represent normal code tasks around such things as incomplete features and `XXX` items represent a bug, potential bug, pitfalls, incorrectness, inelegance, uncertainty about part of the code, etc. Common synonyms for `XXX` include `BUG`, `FIXME` and sometimes `HACK` (this last especially for incorrectness or inelegance).

When signing comments, you should use your username (for code within the company) or full email address (for code visible outside the company), not just initials.

```lisp
;; --- TODO (alice@gmail.com): Refactor to provide a better API.
```

```lisp
;; --- TODO (bob): Remove this code after release 1.7 or before 2012-11-30.
```

If there is an associated issue or bug ticket with the given `TODO` or `XXX` item, be sure to include that in a following line:

```lisp
;; --- XXX (carol): There is a serious issue here, causing problems in other
;;                  areas of the code. We haven't decided upon the best
;;                  approach yet. See the following ticket for details:
;;
;;                   * https://github.com/examplecom/api/issues/42
;;
```
