The trepanning debugger gdb-like debugger. As such, it is both a high-level and low-level debugger. It is a also a rewrite of *ruby-debug*. But to provide all of the functionality that it has, it requires a patched version of MRI Ruby 2,1, 1.9.3 or 1.9.2 found the [ruby-debugger-runtime project](https://sourceforge.net/projects/ruby-debugger-runtime/). The additional run-time support in the MRI is what gives this [powerful features](https://github.com/rocky/rb-trepanning/wiki/Cool-things) that you won't find in other MRI 2.1 or 1.9 debuggers that don't use this runtime.

See the [installation instructions](https://github.com/rocky/rb-trepanning/wiki/How-to-Install-rb-trepanning).

There is a [google group mailing list](http://groups.google.com/group/ruby-debugger) for Ruby debuggers.

If *trepanning* is installed, here is how to run:

```console
   $ trepan ruby-program [program]
```

If your program needs options of its own:

```console
   $ trepan -- ruby-program [program args...]
```

If you want to run from the source tree you can do that too:

```console

  cd place-where-trepan-is-installed
 ./bin/trepan -- ruby-program [program args...]
```

Running from inside *irb*:

```ruby
 require 'trepanning'
 Trepan.debug { your code }
```

The return value from Trepan is the return value of the block, i.e. the final value in the block.

You can run the same thing inside your Ruby program, but probably you don't want to give a block. Instead, you may want to have debugging start on the next statement in the code:

```ruby
require 'trepanning'
...
debugger # stop here
```


There is extensive on-line help, much in markdown format that displays nicely in a terminal. Run `help` inside the debugger.
