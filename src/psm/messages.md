# Messages

## Tag messages

All messages should be tagged. This makes the order in the receive statement less important and the implementation of new messages easier.

Don't program like this:

```lisp
(defun loop (state)
  (receive
    ...
    (`#(,mod ,funcs ,args)
     (erlang:apply mod funcs args)
     (loop state))
    ...))
```

The new message ``` `#(get_status_info ,from ,option)``` will introduce a conflict if it is placed below the ``` `#(,mod ,funcs ,args)``` message.

If messages are synchronous, the return message should be tagged with a new atom, describing the returned message. Example: if the incoming message is tagged get_status_info, the returned message could be tagged status_info. One reason for choosing different tags is to make debugging easier.

This is a good solution:

```lisp
(defun loop (state)
  (receive
    ...
    (`#(execute ,mod ,funcs ,args)
     (erlang:apply mod funcs args)
     (loop state))
    (`#(get_status_info ,from ,option)
     (! from `#(status_info ,(get-status-info option state)))
     (loop state))
    ...))
```

## Use tagged return values

Use tagged return values.

Don't program like this:

```lisp
(defun keysearch
  ((key `(#(,key ,value) . ,tail))
    value)
  ((key `(cons `#(,wrong-key ,wrong-value) . ,tail))
    (keysearch key '()))
  ((key '())
    'false))
```

Then (tuple key, value) cannot contain the false value.

This is the correct solution:

```lisp
(defun keysearch
  ((key `(#(,key ,value) . ,tail))
    `#(value ,value))
  ((key `(#(,wrong-key ,wrong-value) . ,tail))
    (keysearch key '()))
  ((key '())
    'false))
```

## Flush unknown messages

Every server should have an Other alternative in at least one receive statement. This is to avoid filling up message queues. Example:

```lisp
(defun main-loop ()
  (receive
    (`#(msg1 ,msg1)
     ...
     (main-loop))
    (`#(msg2 ,msg2)
     ...
     (main-loop))
    (other  ; flush the message queue
     (logger:error "Process ~w got unknown msg ~w~n" `(,(self) ,other))
     (main-loop))))
```

## Interface functions

Use functions for interfaces whenever possible, avoid sending messages directly. Encapsulate message passing into interface functions. There are cases where you can't do this.

The message protocol is internal information and should be hidden to other modules.

Example of interface function:

```lisp
(defmodulee fileserver
  (export
   (start 0)
   (stop 0)
   (open-file 1)
   ...))

(defun open-file (server-pid filename)
  (! serever-pid `#(open-file-request ,filename))
  (receive
    (`#(open-file-response ,result) result)))

...
```

## Time-outs

Be careful when using after in receive statements. Make sure that you handle the case when the message arrives later.

## Trapping exits

As few processes as possible should trap exit signals. Processes should either trap exits or they should not. It is usually very bad practice for a process to "toggle" trapping exits.
