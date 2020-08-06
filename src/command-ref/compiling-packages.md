# Package Support

The LFE rebar3 plugin provides support for pseudo-packages. There is no such thing as a pckage in Erlang, but using this plugin, you can emulate some of the behaviours of packages.

This is accomplished by traversing top-level source directories for any subdirectories: if the plugin finds any `.lfe` or `.erl` files in subdirectories under configured source directories, it will create a dotted module name composed of the relative path to that file, and write that name to the `ebin` directory after successful compilation.

Here are some examples of how combinations of subdirectories and files will be transformed in their final form as `.beam` files:

```text
./src/my.package1.lfe                      -> ebin/my.package1.beam
./src/my/package2.lfe                      -> ebin/my.package2.beam
./src/my/other/package.lfe                 -> ebin/my.other.package.beam
./src/my/really/deeply/nested/package1.lfe -> ebin/my.really.deeply.nested.package1.beam
./src/my/really/deeply/nested.package2.lfe -> ebin/my.really.deeply.nested.package2.beam
./src/my/really.deeply.nested.package3.lfe -> ebin/my.really.deeply.nested.package3.beam
./src/my.really.deeply.nested.package4.lfe -> ebin/my.really.deeply.nested.package4.beam
```
