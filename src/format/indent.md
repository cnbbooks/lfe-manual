# Indentation

In general, use your text editor's indentation capabilities. If you are contributing to a particular library, be sure to ask the maintainers what standard they use, and follow those same guidelines, thus saving everyone from the drudgery of whitespace fixes later.

In particular, you'll want to do everything you can to follow the conventions laid out in the Emacs LFE mode supplied in the [LFE source](https://github.com/rvirding/lfe/tree/develop/emacs).

Maintain a consistent indentation style throughout a project.

Indent carefully to make the code easier to understand.

 Use indentation to make complex function applications easier to read. When an application does not fit on one line or the function takes many arguments, consider inserting newlines between the arguments so that each one is on a separate line. However, do not insert newlines in a way that makes it hard to tell how many arguments the function takes or where an argument form starts and ends.

Bad:
```text
(do-something first-argument second-argument (lambda (x)
    (frob x)) fourth-argument last-argument)
```

Better:
```lisp
(do-something first-argument
              second-argument
              (lambda (x) (frob x))
              fourth-argument
              last-argument)
```
