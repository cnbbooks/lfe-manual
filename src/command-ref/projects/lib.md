# Creating LFE Libraries

Library projects are those with no running applications or scripts; they simply
provide some core bit of functionality intended for use by applications,
scripts, or other libraries.

To create a library project with the default name:

```shell
rebar3 new lfe-lib
```

 This will generate the following output:

```text
===> Writing my-lfe-lib/README.md
===> Writing my-lfe-lib/LICENSE
===> Writing my-lfe-lib/rebar.config
===> Writing my-lfe-lib/.gitignore
===> Writing my-lfe-lib/src/my-lfe-lib.lfe
===> Writing my-lfe-lib/src/my-lfe-lib.app.src
```

You can also explicitely name your project:

```shell
rebar3 new lfe-lib forty-two
```

Which will produce the following:

```text
===> Writing forty-two/README.md
===> Writing forty-two/LICENSE
===> Writing forty-two/rebar.config
===> Writing forty-two/.gitignore
===> Writing forty-two/src/forty-two.lfe
===> Writing forty-two/src/forty-two.app.src
```

As mentioned abouve, the REPL offers a nice way to quickly interact your new
project.

Start the REPL:

```shell
cd forty-two
rebar3 lfe repl
```

Call the generated/sample LFE function:

```lisp
lfe> (mything:my-fun)
hello-world
```
