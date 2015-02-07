Syntax for Indicating a Filename
================================

There are two ways you can give a file name:

* unadorned (without double-quotes) with possible escapes
* as a double-quoted string with possible escapes in the string

Probably most of the time a file name will be specified in the first
form, without using quotes. If the file name however has a space or a
colon in it, escape that character with a backslash. Also, if you need
to enter a backslash and the character followinng that is unlucky
enough to be a colon, space, or backslash use two backslashes.

Filename Examples:
------------------

    irb.rb       => irb.rb
    /tmp/irb.rb  =>  /tmp/irb.rb
    C\:irb.rb    =>  C:irb.rb
    C\:\irb.rb   =>  C:\irb.rb
    C\:\\irb.rb  =>  C:\irb.rb  # Note: double slash not needed
    \\new.rb    =>  \new.rb     # Note: double slash, or filename has newline
    my\ file.rb  =>  my file.rb


The quoted string is useful if you have a file name that contains
several characters that normally confuse the debugger parser, notably
a space, newline, or a colon. The quoted string starts with a double
quote ("). Escape sequences are allowed inside the string to be able
to enter tabs or newlines, or a double quote inside the string. The
list of translations is as follows:


    \t => <tab>
    \n => <newline>
    \" => "
    \\ => \

Quoted Filename Examples:
-------------------------

    "This is a file with blanks.rb" => This is a file with blanks.rb
    "/tmp/RubyProgram \"foo\".rb    => /tmp/RubyProgram "foo".rb
    "/Ruby\nProgram.rb"             => /tmp/Ruby
	Program.rb
