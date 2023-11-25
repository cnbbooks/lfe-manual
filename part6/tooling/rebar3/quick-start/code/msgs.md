# Message-Passing

We glossed over this in the previous section, but in LFE (and Erlang) you can compile on-the-fly in a REPL session. This is super-convenient when prototyping functionality for a new project where you want to use the REPL, but you also want the benefits of using a file, so you don't lose your work.

We made some changes to the sample code in the last section; let's compile it and take it for a spin:

```lisp
> (c "src/my-test-lib.lfe")
#(module my-test-lib)
```

Next, let's send two messages to another Erlang process (in this case, we'll
send it to our REPL process, `(self)`:

```lisp
> (my-test-lib:send-message (self) "And what does it say now?")
#(<0.26.0> "And what does it say now?")
Received message: 'And what does it say now?'
Sending message to process <0.26.0> ...
> (my-test-lib:send-message (self) "Mostly harmless.")
#(<0.26.0> "Mostly harmless.")
Received message: 'Mostly harmless.'
Sending message to process <0.26.0> ...
```

In the above calls, for each message sent we got a reply acknowledging the
message (because the example was coded like that). But what about the receiver
itself? What did it, our REPL process, see? We can flush the message
queue in the REPL to find out.

What, what? Does each ...

Yup, every process in LFE (and Erlang, of course) has an inbox. You can see
how many messages a given process has by looking at the process' info:

```lisp
lfe> (erlang:process_info (self) 'message_queue_len)
#(message_queue_len 2)
```

And there you can see that our REPL process has two messages queued up in its
inbox. Let's take a look!

```lisp
> (c:flush)
Shell got {"And what does it say now?"}
Shell got {"Mostly harmless."}
ok
```

If you found this last bit interesting and want to step through a tutorial on
Erlang's light-weight threads in more detail, you may enjoy
[this tutorial](http://lfe.io/tutorial/concurrent/processes.html).

### Next Stop

We did promise a bit more information, so we're going to do that
next and then wrap up the quick start and point you in some
directions for your next LFE adventures ...
