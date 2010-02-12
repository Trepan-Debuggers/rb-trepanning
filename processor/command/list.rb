# -*- coding: utf-8 -*-
require 'linecache'
require_relative %w(base cmd)

class Debugger::Command::ListCommand < Debugger::Command
  unless defined?(HELP)
    HELP = 
"list [MODULE] [FIRST [NUM]]
list LOCATION [NUM]

List source code. 

Without arguments, print LISTSIZE lines centered around the current
line. If this is the first list command issued since the debugger
command loop was entered, then the current line is the current
frame. If a subsequent list command was issued with no intervening
frame changing, then that is start the line after we last one
previously shown.

\"list -\" shows LISTSIZE lines before a previous listing. 

A LOCATION is a either 
  - number, e.g. 5, 
  - a function, e.g. join or os.path.join
  - a module, e.g. os or os.path
  - a filename, colon, and a number, e.g. foo.rb:5,  
  - or a module name and a number, e.g,. os.path:5.  
  - a '.' for the current line number
  - a '-' for the lines before the current line number

If the location form is used with a subsequent parameter, the
parameter is the starting line number and LISTSIZE lines are
used. When there two numbers are given, the last number value is
treated as a stopping line unless it is less than the start line, in
which case it is taken to mean the number of lines to list instead.

Wherever a number is expected, it does not need to be a constant --
just something that evaluates to a positive integer.

Some examples:

list 5            # List centered around line 5
list 4+1          # Same as above.
list 5>           # List starting at line 5
list foo.rb:5     # List centered around line 5 of foo.rb
list foo.rb 5     # Same as above.
list foo.rb:5>    # List starting around line 5 of foo.rb
list foo.rb  5 6  # list lines 5 and 6 of foo.rb
list foo.rb  5 2  # Same as above, since 2 < 5.
list foo.rb:5 2   # Same as above
list FileUtils.cp # List lines around the FileUtils.cp function.
list .            # List lines centered from where we currently are stopped
list -            # List lines previous to those just shown

The number of line to show is controled by the debugger listsize
setting. Use 'set listsize' or 'show listsize' to see or set the
value.

The output of the list command give a line number, and some status
information about the line and the text of the line. Here is some 
hypothetical list output modeled roughly around line 251 of one
version of this code:

  251    	  cmd.proc.frame_setup(tf)
  252  ->	  brkpt_cmd.run(['break'])
  253 B01   	  line = __LINE__
  254 b02   	  cmd.run(['list', __LINE__.to_s])
  255 t03   	  puts '--' * 10

Line 251 has nothing special about it. Line 252 is where we are
currently stopped. On line 253 there is a breakpoint 1 which is
enabled, while at line 255 there is an breakpoint 2 which is
disabled."

    ALIASES       = %w(l list> l>)
    CATEGORY      = 'files'
    MAX_ARGS      = 3
    NAME          = File.basename(__FILE__, '.rb')
    SHORT_HELP    = 'List source code'
  end

  # What a f*cking mess. Necessitated I suppose because we want to 
  # allow somewhat flexible parsing with either module names, files or none
  # and optional line counts or end-line numbers.
  
  # Parses arguments for the "list" command and returns the tuple:
  # filename, start, last
  # or sets these to nil if there was some problem.
  def parse_list_cmd(args, listsize, center_correction)
    
    frame = @proc.frame
    
    container = @proc.frame_container(frame, false)
    filename = container[1]
    
    last = nil
    if args.empty? and not frame
      errmsg("No Ruby program loaded.")
      return nil, nil, nil
    end
    
    if args.size > 0
      if args[0] == '-'
        return no_frame_msg unless @proc.line_no
        first = [1, @proc.line_no - 2*listsize - 1].max
      elsif args[0] == '.'
        return no_frame_msg unless @proc.line_no
        first = [1, @proc.frame_line - center_correction].max
      else
        modfunc, container, first = @proc.parse_position(args[0])
        if first == nil and modfunc == nil
          # error should have been shown previously
          return nil, nil, nil
        end
        if args.size == 1
          first = 1 if !first and modfunc
          first = [1, first - center_correction].max
        elsif args.size == 2 or (args.size == 3 and modfunc)
          msg = 'Starting line expected, got %s.' % args[-1]
          num = @proc.get_an_int(args[1], msg)
          return nil, nil, nil if num is nil
          if modfunc
            if first
              first = num
              if args.size == 3 and modfunc
                msg = ('last or count parameter expected, ' +
                       'got: %s.' % args[2])
                last = @proc.get_an_int(args[2], msg)
              end
            else
              last = num
            end
          else
            last = num
          end
          if last and last < first
            # Assume last is a count rather than an end line number
            last = first + last - 1
          end
        elsif not modfunc
          errmsg('At most 2 parameters allowed when no module' +
                  ' name is found/given. Saw: %d parameters' % args.size)
          return nil, nil, nil
        else
          errmsg(('At most 3 parameters allowed when a module' +
                  ' name is given. Saw: %d parameters') % args.size)
          return nil, nil, nil
        end
      end
    elsif !@proc.line_no and frame
      first = [1, @proc.frame_line - center_correction].max
    else
      first = [1, @proc.line_no - center_correction].max + 1
    end
    last = first + listsize - 1 unless last
  
    return container, first, last
  end

  def no_frame_msg
    errmsg("No Ruby program loaded.")
    return nil, nil, nil
  end
    
  def run(args)
    listsize = settings[:listsize]
    center_correction = 
      if args[0][-1..-1] == '>'
        0
      else
        listsize / 2
      end

    container, first, last = 
      parse_list_cmd(args[1..-1], listsize, center_correction)
    frame = @proc.frame
    return unless container
    breaklist = @proc.brkpts.line_breaks(container)

    # We now have range information. Do the listing.
    max_line = LineCache::size(container[1])
    unless max_line 
      errmsg('No file %s found' % container[1])
      return
    end

    if first > max_line
      errmsg('Bad line range [%d...%d]; file "%s" has only %d lines' %
             [first, last, container[1], max_line])
      return
    end

    if last > max_line
      # msg('End position changed to last line %d ' % max_line)
      last = max_line
    end

    begin
      first.upto(last+1).each do |lineno|
        line = LineCache::getline(container[1], lineno).chomp
        unless line
          msg('[EOF]')
          break
        end
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
              container == frame.source_container) ? '->' : a_pad
        msg(s + "\t" + line)
        @proc.line_no = lineno
      end
    rescue
    end
  end
end

if __FILE__ == $0
  if  not (ARGV.size == 1 && ARGV[0] == 'noload')
    ISEQS__ = {}
    ARGV[0..-1] = ['noload']
    load(__FILE__)
  else    
    require_relative %w(.. location)
    require_relative %w(.. mock)
    name = File.basename(__FILE__, '.rb')
    dbgr, cmd = MockDebugger::setup(name)
    cmd.proc.instance_variable_set('@remap_container', {})
    LineCache::cache(__FILE__)
    cmd.run(['list'])
    cmd.run(['list', __FILE__ + ':10'])
    puts '--' * 10
    # cmd.run(['list', 'os', '10'])
    # cmd.proc.frame = sys._getframe()
    # cmd.proc.setup()
    # puts '--' * 10
    # cmd.run(['list'])
    # puts '--' * 10
    cmd.run(['list', '.'])
    puts '--' * 10
    cmd.run(['list', '10'])
    puts '--' * 10
    # cmd.run(['list', '9+1'])
    # puts '--' * 10
    cmd.run(['list', '10>'])
    puts '--' * 10
    cmd.run(['list', '1000'])
    def foo()
      return 'bar'
    end
    cmd.run(['list', 'foo'])
    puts '--' * 10

    p = Proc.new do 
      |x,y| x + y
    end
    cmd.run(['list', 'p'])
    puts '--' * 10

    # puts '--' * 10
    # cmd.run(['list', 'os.path', '15'])
    # puts '--' * 10
    # cmd.run(['list', 'os.path', '30', '3'])
    # puts '--' * 10
    # cmd.run(['list', 'os.path', '40', '50'])
    # puts '--' * 10
    # cmd.run(['list', os.path.abspath(__file__)+':3', '4'])
    # puts '--' * 10
    # cmd.run(['list', os.path.abspath(__file__)+':3', '12-10'])
    # cmd.run(['list', 'os.path:5'])

    require 'thread_frame'
    tf = RubyVM::ThreadFrame.current
    cmd.proc.frame_setup(tf)
    brkpt_cmd = cmd.proc.instance_variable_get('@commands')['break']
    brkpt_cmd.run(['break'])
    line = __LINE__
    cmd.run(['list', __LINE__.to_s])
    puts '--' * 10
    disable_cmd = cmd.proc.instance_variable_get('@commands')['disable']
    disable_cmd.run(['disable', '1'])
    cmd.run(['list', line.to_s])
    puts '--' * 10

    ISEQS__['parse_list_cmd'].each{|i| p i.source_container}
    cmd.run(['list', 'parse_list_cmd'])
    # puts '--' * 10
  end
end
