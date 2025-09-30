# LFE/OTP 'Hello, World!'

What have been demonstrated so far are fairly vanilla Hello-World examples; there's nothing particularly interesting about them, which puts them solidly iin the company of the millions of other Hello-World programs. As mentioned before, this approach is particularly vexing in the case of LFE/OTP, since it lures the prospective developer into the preconception that BEAM languages are just like other programming languages. They most decidedly are not.

What makes them, and in this particular case LFE, special is OTP. There's nothing quite like it, certainly not another language with OTP's feature set baked into its heart and soul. Most useful applications you will write in LFE/OTP will be composed of some sort of long-running service or server, something that manages that server and restarts it in the event of errors, and lastly, a context that contains both -- usually referred to as the "application" itself.

As such, a _real_ Hello-World in LFE would be honest and let the prospective developer know what they are in for (and what power will be placed at their fingertips). _That_ is what we will show now, an LFE OTP Hello-World example.

If you are still in the directory of the previous Hello-World project, let's get out of that:

```bash
cd ../
```

Now we're going to create a new project, one utilising the some very basic OTP patterns:

```bash
rebar3 new lfe-app hello-otp-world
cd ./hello-otp-world
```

We won't look at the code for this right now, since there are chapters dedicated to that in the second half of the book. But let's brush the surface with a quick run in the REPL:

```bash
rebar3 lfe repl
```

To start your new hello-world application, use the OTP `application` module:

```lisp
lfe> (application:ensure_all_started 'hello-otp-world)
;; #(ok (hello-otp-world))
```

That message lets you know that not only was the `hello-otp-word` application and server started without issue, any applications upon which it depends were also started. Furthermore, there is a supervisor for our server, and it has started as well. Should our Hello-World server crash for any reason, the supervisor will restart it.

To finish the demonstration, and display the clichéd if classic message:

```lisp
(hello-otp-world:echo "Hello, OTP World!")
;; "Hello, OTP World!"
```

And that, dear reader, is a _true_ LFE/OTP Hello-World program, complete with message-passing and pattern-matching!

Feel free to poke around in the code that was generated for you, but know that eventually all its mysteries will be revealed, and by the end of this book, that program's magic will just seem like ordinary code to you, ordinary, dependable, fault-tolerant, highly-availble, massively-concurrent code.
