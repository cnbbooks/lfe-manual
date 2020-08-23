# LFE/OTP 'Hello, World!'

What has been demonstrated so far is a fairly vanilla Hello-World; there's nothing particularly interesting about it, just like any other Hello-World program. As mentioned before, this is particularly tricking in the case of LFE/OTP, since it lures the prospective developer into the preconception that BEAM languages are just like other programming languages, and they are not.

What makes them, and in this particular case LFE, special is OTP. There's really nothing quite like it, certainly not baked into the heart and soul of another programming language. Most useful applications you will write in LFE/OTP will be composed of some sort of long-running service or server, something that manages that server and restarts it in the event of errors, and lastly, a context that contains both referred to as the "application" itself.

As such, a _real_ Hello-World in LFE would be honest and let the prospective developer know what they are info (and what power will be placed at their fingertips). And that is what we will show now.

If you are still in the directory of the previous Hello-World project, let's get out of that:

```shell
$ cd ../
```

Now we're going to create a new project, one utilising the some very basic OTP patterns:

```shell
$ rebar3 new lfe-app hello-otp-world
$ cd ./hello-otp-world
```

We won't look at the code for this right now, since there are chapters dedicated to that in the second half of the book. But let's brush the surface with a quick run in the REPL:

```shell
$ rebar3 lfe repl
```

To start your new hello-world application, use the OTP `application` module:

```lisp
lfe> (application:ensure_all_started 'hello-otp-world)
```
```lisp
#(ok (hello-otp-world))
```

That message lets you know that not only was the `hello-otp-word` application and server started without issue, any applications upon which it depends were also started. Furthermore, there is a supervisor for our server, and it has started as well. Should our Hello-World server crash for any reason, the supervisor will restart it.

To finish the demonstration, and display the clichéd if classic message:

```lisp
(hello-otp-world:echo "Hello, OTP World!")
```
```lisp
"Hello, OTP World!"
```

And that, dear reader, is a _true_ LFE/OTP Hello-World program, complete with message-passing and pattern-matching!

Feel free to poke around in the code that was generated for you, but know that eventually all its mysteries will be revealed, and by the end of this book this program's magic will just seem like ordinary code to you, ordinary, dependable, fault-tolerant, highly-availble, massively-concurrent code.
