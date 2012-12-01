A modular, testable debugger for MRI Ruby 1.9.3 or 1.9.2. A total rewrite of *ruby-debug*.

__In order to use this debugger, you'll need a patched MRI Ruby 1.9.3 or 1.9.2 and some additional packages.__ See the [installation instructions](https://github.com/rocky/rb-trepanning/wiki/How-to-Install-rb-trepanning).

There is a ["google group mailing list](http://groups.google.com/group/ruby-debugger for Ruby debuggers.)

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
 require 'trepan' 
 Trepan.debug # Don't stop here...
 work # but stop here.
```

The above is really shorthand for something like:

```ruby
  $trepan = Trepan.new
  $trepan.debugger
```

The global variable *$trepan* set holds debugger settings, such as `autolist" or `autoeval` settings and breakpoint information.

Due to the line-event orientation in ruby-debug, it occasionally was convenient to add a synchronous stop in your program. I don't think that will be necessary here, but if you do call to the debugger at the point of the call rather than the subsequent stopping point, set `opts[:immediate]` to *true*. Example:

```ruby

 # ... work, work, work 
 mydbg.debugger(:immediate=>true) # enter debugger here 
 # ... work, work, work
```

There is extensive on-line help. Run `help` inside the debugger.
