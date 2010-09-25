require 'thread_frame'
require 'trace'
require_relative '../../lib/trepanning'
require_relative '../../io/string_array'

module FnTestHelper
  include Trace

  # Synchronous events without C frames or instructions
  TEST_STEP_EVENT_MASK = LINE_EVENT_MASK | CLASS_EVENT_MASK | CALL_EVENT_MASK |
    RETURN_EVENT_MASK  

  # Common setup to create a debugger with String Array I/O attached
  def strarray_setup(debugger_cmds, insn_stepping=false)
    stringin               = Trepan::StringArrayInput.open(debugger_cmds)
    stringout              = Trepan::StringArrayOutput.open
    d_opts                 = {:input  => stringin, :output => stringout,
                              :nx     => true}
    d_opts[:core_opts]     = {:step_events => TEST_STEP_EVENT_MASK}
    d_opts[:core_opts][:step_events] ||= INSN_EVENT_MASK if insn_stepping
    d                      = Trepan.new(d_opts)

    # Remove vm and switch from unmaskable events to increase predictability
    # of test results
    d.core.instance_variable_set('@unmaskable_events', %w(brkpt raise))

    d.settings[:basename]  = true
    d.settings[:different] = false
    d.settings[:autoeval]  = false
    return d
  end

  unless defined?(TREPAN_PROMPT)
    TREPAN_PROMPT = /^\(trepan\): /
    TREPAN_LOC    = /.. \(.+:\d+\)/
  end

  # Return the caller's line number
  def get_lineno
    RubyVM::ThreadFrame.current.prev.source_location[0]
  end

  def compare_output(right, d, debugger_cmds)
    # require_relative '../../lib/trepanning'
    # Trepan.debug(:set_restart => true)
    got = filter_line_cmd(d.intf[-1].output.output)
    if got != right
      got.each_with_index do |got_line, i|
        if i < right.size and got_line != right[i]
          # dbgr.debugger
          puts "! #{got_line}"
        else
          puts "  #{got_line}"
        end
      end
      puts '-' * 10
      right.each_with_index do |right_line, i|
        if i < got.size and got[i] != right_line
          # dbgr.debugger
          puts "! #{right_line}"
        else
          puts "  #{right_line}"
        end
      end
    end
    assert_equal(right, got, caller[0])
  end

  # Return output with source lines prompt and command removed
  def filter_line_cmd(a, show_prompt=false)
    # Remove debugger prompt
    a = a.map do |s|
     s =~ TREPAN_PROMPT ? nil : s
    end.compact unless show_prompt

    # Remove debugger location lines. 
    # For example: 
    #   -- (/src/external-vcs/trepan/tmp/gcd.rb:4)
    # becomes:
    #   -- 
    a2 = a.map do |s|
      s =~ TREPAN_LOC ? s.gsub(/\(.+:\d+\)\n/, '').chomp : s.chomp
    end

    # Remove VM offset locations. 
    # For example: 
    #	VM offset 2 of instruction sequence "block in compare_output".
    # becomes 
    #   VM offset 55 of instruction sequence "block in compare_output".
    a3 = a2.map do |s|
      s.gsub(/VM offset \d+/, 'VM offset 55')
    end
    return a3
  end

end

# Demo it
if __FILE__ == $0
  include FnTestHelper
  strarray_setup(%w(eh bee see))
  puts get_lineno()
  p '-- (/src/external-vcs/trepan/tmp/gcd.rb:4)' =~ TREPAN_LOC
  p '(trepan): exit' =~ TREPAN_PROMPT
  output='
-- (/src/external-vcs/trepan/tmp/gcd.rb:4)
(trepan): s
-- (/src/external-vcs/trepan/tmp/gcd.rb:18)
(trepan): s
-- (/src/external-vcs/trepan/tmp/gcd.rb:19)
(trepan): s
.. (/src/external-vcs/trepan/tmp/gcd.rb:0)
(trepan): s
-> (/src/external-vcs/trepan/tmp/gcd.rb:4)
'.split(/\n/)
  puts filter_line_cmd(output)
  puts '-' * 10
  puts filter_line_cmd(output, true)
end
