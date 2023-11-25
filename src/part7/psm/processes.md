# Processes

## Implement a process in one module

Code for implementing a single process should be contained in one module. A process can call functions in any library routines but the code for the "top loop" of the process should be contained in a single module. The code for the top loop of a process should not be split into several modules - this would make the flow of control extremely difficult to understand. This does not mean that one should not make use of generic server libraries, these are for helping structuring the control flow.

Conversely, code for no more than one kind of process should be implemented in a single module. Modules containing code for several different processes can be extremely difficult to understand. The code for each individual process should be broken out into a separate module.

## Use processes for structuring the system

Processes are the basic system structuring elements. But don't use processes and message passing when a function call can be used instead.

## Registered processes

Registered processes should be registered with the same name as the module. This makes it easy to find the code for a process.

Only register processes that should live a long time.

## Assign exactly one parallel process to each true concurrent activity in the system

When deciding whether to implement things using sequential or parallel processes then the structure implied by the intrinsic structure of the problem should be used. The main rule is:

"Use one parallel process to model each truly concurrent activity in the real world"

If there is a one-to-one mapping between the number of parallel processes and the number of truly parallel activities in the real world, the program will be easy to understand.

## Each process should only have one "role"

Processes can have different roles in the system, for example in the client-server model.

As far as possible a process should only have one role, i.e. it can be a client or a server but should not combine these roles.

Other roles which process might have are:

Supervisor: watches other processes and restarts them if they fail.
Worker: a normal work process (can have errors).
Trusted Worker: not allowed to have errors.

## Use the process dictionary with extreme care

Do not use get and put etc. unless you know exactly what you are doing! Use get and put etc., as little as possible.

A function that uses the process dictionary can be rewritten by introducing a new argument.

Don't program like this:

```lisp
(defun tokenize
  ((`(,head . ,tail))
    ...)
  (('())
    (case (get-characters-from-device (get 'device))
      ('eof
        '())
      (`#(value ,chars)
        (tokenize chars)))))
```

The correct solution:

```lisp
(defun tokenize
  ((device (,head . ,tail))
    ...)
  ((device '())
    (case (get-characters-from-device device)
      ('eof
        '())
      (`#(value, chars)
        (tokenize device chars)))))
```

The use of get and put will cause a function to behave differently when called with the same input at different occasions. This makes the code hard to read since it is non-deterministic. Debugging will be more complicated since a function using get and put is a function of not only of its argument, but also of the process dictionary. Many of the run time errors in LFE (for example bad_match) include the arguments to a function, but never the process dictionary.
