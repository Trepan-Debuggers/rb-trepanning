# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
# -*- coding: utf-8 -*-
require 'linecache'
require 'rbconfig'
require_relative '../command'
require_relative '../list'

class Trepan::Command::ListCommand < Trepan::Command
    unless defined?(HELP)
        NAME = File.basename(__FILE__, '.rb')
        HELP = <<-HELP
**#{NAME}[>]** [*module*] [*first* [*num*]]
**#{NAME}[>]** *location [*num*]

List source code.

Without arguments, prints lines centered around the current line. If
this is the first #{NAME} command issued since the debugger command
loop was entered, then the current line is the current frame. If a
subsequent #{NAME} command was issued with no intervening frame
changing, then that is start the line after we last one previously
shown.

If the command has a ">" suffix, then line centering is disabled and
listing begins at the specificed location.

The number of lines to show is controlled by the debugger "listsize"
setting. Use 'set max list' or 'show max list' to see or set the
value.

\"#{NAME} -\" shows lines before a previous listing.

A *location* is a either:

* number, e.g. 5,
* a function, e.g. *join* or *os.path.join*
* a module, e.g. *os* or *os.path*
* a filename, colon, and a number, e.g. *foo.rb:5*,
* or a module name and a number, e.g,. *os.path:5*.
* a "." for the current line number
* a "-" for the lines before the current line number

If the location form is used with a subsequent parameter, the
parameter is the starting line number.  When there two numbers are
given, the last number value is treated as a stopping line unless it
is positive and less than the start line. In this case, it is taken to
mean the number of lines to list instead. If last is negative, we
start that many lines back from first and list to first.

Wherever a number is expected, it does not need to be a constant,
just something that evaluates to a positive integer.

Examples:
---------

    #{NAME} 5            # List centered around line 5
    #{NAME} @5           # List lines centered around bytecode offset 5.
    #{NAME} 5>           # List starting at line 5
    #{NAME} foo.rb:5     # List centered around line 5 of foo.rb
    #{NAME} foo.rb 5     # Same as above.
    #{NAME}> foo.rb:5    # List starting around line 5 of foo.rb
    #{NAME} foo.rb  5 6  # list lines 5 and 6 of foo.rb
    #{NAME} foo.rb  5 2  # Same as above, since 2 < 5.
    #{NAME} foo.rb:5 2   # Same as above
    #{NAME} foo.rb 15 -5 # List lines 10..15 of foo
    #{NAME} FileUtils.cp # List lines around the FileUtils.cp function.
    #{NAME} .            # List lines centered from where we currently are stopped
    #{NAME} . 3          # List 3 lines starting from where we currently are stopped
                         # if . > 3. Otherwise we list from . to 3.
    #{NAME} -            # List lines previous to those just shown

The output of the #{NAME} command gives a line number, and some status
information about the line and the text of the line. Here is some
hypothetical #{NAME} output modeled roughly around line 251 of one
version of this code:

    251    	  cmd.proc.frame_setup(tf)
    252  ->	  brkpt_cmd.run(['break'])
    253 B01   	  line = __LINE__
    254 b02   	  cmd.run(['list', __LINE__.to_s])
    255 t03   	  puts '--' * 10

Line 251 has nothing special about it. Line 252 is where we are
currently stopped. On line 253 there is a breakpoint 1 which is
enabled, while at line 255 there is an breakpoint 2 which is
disabled.

See also:
---------

`set max list`, `show max list`, `disassemble`, `help syntax location`
    HELP

        ALIASES       = %W(l #{NAME}> l> cat)
        CATEGORY      = 'files'
        MAX_ARGS      = 3
        SHORT_HELP    = 'List source code'
    end

    def run(args)
        if args.size == 1 and not @proc.frame
            errmsg("No Ruby program loaded.")
            return
        end
        listsize = settings[:maxlist]
        center_correction =
            if args[0][-1..-1] == '>'
                0
            else
                (listsize-1) / 2
            end

        iseq, filename, first, last =
            @proc.parse_list_cmd(@proc.cmd_argstr, listsize, center_correction)
        return unless filename
        unless @proc.text_file?(filename)
            errmsg("File \"%s\" doesn't seem to contain source text." % filename)
            if [RbConfig::CONFIG['DLEXT'],
                RbConfig::CONFIG['DLEXT2']].member?(File.basename(filename))
                msg("File has dynamic loader extension")
            end
            return
        end
        container = iseq ? iseq.source_container : ['file', filename]
        breaklist = @proc.brkpts.line_breaks(container)

        # We now have range information. Do the listing.
        max_line = LineCache::size(filename)
        unless max_line
            errmsg('File "%s" not found.' % filename)
            return
        end

        if first > max_line
            errmsg('Bad line range [%d...%d]; file "%s" has only %d lines' %
                   [first, last, filename, max_line])
            return
        end

        if last > max_line
            # msg('End position changed to last line %d ' % max_line)
            last = max_line
        end

        begin
            opts = {
                :reload_on_change => settings[:reload],
                :output => settings[:highlight]
            }
            frame = @proc.frame
            first.upto(last).each do |lineno|
                line = LineCache::getline(filename, lineno, opts)
                unless line
                    msg('[EOF]')
                    break
                end
                line.chomp!
                s = '%3d' % lineno
                s = s + ' ' if s.size < 4
                s += if breaklist.member?(lineno)
                         bp = breaklist[lineno]
                         a_pad = '%02d' % bp.id
                         bp.icon_char
                     else
                         a_pad = '  '
                         ' '
                     end
                s += (frame && lineno == @proc.frame_line &&
                      filename == frame.source_container[1]) ? '->' : a_pad
                msg(s + "\t" + line, {:unlimited => true})
                @proc.line_no = lineno
            end
        rescue => e
            errmsg e.to_s if settings[:debugexcept]
        end
    end
end

if __FILE__ == $0
    if !(ARGV.size == 1 && ARGV[0] == 'noload')
        ARGV[0..-1]    = ['noload']
        load(__FILE__)
    else
        require_relative '../location'
        require_relative '../mock'
        require_relative '../frame'
        dbgr, cmd = MockDebugger::setup
        cmd.proc.send('frame_initialize')

        def run_cmd(cmd, args)
            cmd.proc.instance_variable_set('@cmd_argstr', args[1..-1].join(' '))
            cmd.run(args)
        end

        LineCache::cache(__FILE__)
        run_cmd(cmd, [cmd.name])
        run_cmd(cmd, [cmd.name, __FILE__ + ':10'])

        def run_cmd2(cmd, args)
            seps = '--' * 10
            puts "%s %s %s" % [seps, args.join(' '), seps]
            run_cmd(cmd,args)
        end


        load 'tmpdir.rb'
        run_cmd2(cmd, %w(list tmpdir.rb 10))
        run_cmd2(cmd, %w(list tmpdir.rb))

        # cmd.proc.frame = sys._getframe()
        # cmd.proc.setup()
        # run_cmd2(['list'])

        run_cmd2(cmd, %w(list .))
        run_cmd2(cmd, %w(list 30))

        # run_cmd2(['list', '9+1'])

        run_cmd2(cmd, %w(list> 10))
        run_cmd2(cmd, %w(list 3000))
        run_cmd2(cmd, %w(list run_cmd2))

        p = Proc.new do
            |x,y| x + y
        end
        require 'thread_frame'
        tf = RubyVM::Frame.current
        cmd.proc.frame_setup(tf)
        run_cmd2(cmd, %w(list p))

        # Function from a file found via an instruction sequence
        run_cmd2(cmd, %w(list Columnize.columnize))

        # Use Class/method name. 15 isn't in the function - should this be okay?
        run_cmd2(cmd, %W(#{cmd.name} Columnize.columnize 15))

        # Start line and count, since 3 < 30
        run_cmd2(cmd, %W(#{cmd.name} Columnize.columnize 30 3))

        # Start line finish line
        run_cmd2(cmd, %W(#{cmd.name} Columnize.columnize 40 50))

        # puts '--' * 10
        # run_cmd2([cmd.name, os.path.abspath(__file__)+':3', '4'])
        # puts '--' * 10
        # run_cmd2([cmd.name, os.path.abspath(__file__)+':3', '12-10'])
        # run_cmd2([cmd.name, 'os.path:5'])

        brkpt_cmd = cmd.proc.instance_variable_get('@commands')['break']
        brkpt_cmd.run(['break'])
        line = __LINE__
        run_cmd2(cmd, [cmd.name, __LINE__.to_s])

        disable_cmd = cmd.proc.instance_variable_get('@commands')['disable']
        disable_cmd.run(['disable', '1'])

        run_cmd2(cmd, [cmd.name, line.to_s])
        run_cmd2(cmd, %W(#{cmd.name} run_cmd2))
        run_cmd2(cmd, %W(#{cmd.name} run_cmd2))
        run_cmd2(cmd, %W(#{cmd.name} @0))
    end
end
