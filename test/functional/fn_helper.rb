require 'thread_frame'
require 'trace'
require_relative %w(.. .. lib rbdbgr)
require_relative %w(.. .. io string_array)

module FnTestHelper
  include Trace

  # Synchronous events without C frames or instructions
  TEST_STEP_EVENT_MASK = LINE_EVENT_MASK | CLASS_EVENT_MASK | CALL_EVENT_MASK |
    RETURN_EVENT_MASK  

  # Common setup to create a debugger with String Array I/O attached
  def strarray_setup(debugger_cmds, insn_stepping=false)
    stringin               = Debugger::StringArrayInput.open(debugger_cmds)
    stringout              = Debugger::StringArrayOutput.open
    d_opts                 = {:input  => stringin, :output => stringout,
                              :nx     => true}
    d_opts[:core_opts]     = {:step_events => TEST_STEP_EVENT_MASK}
    d_opts[:core_opts][:step_events] ||= INSN_EVENT_MASK if insn_stepping
    d                      = Debugger.new(d_opts)
    d.settings[:basename]  = true
    d.settings[:different] = false
    d.settings[:autoeval]  = false
    return d
  end

  unless defined?(RBDBGR_PROMPT)
    RBDBGR_PROMPT = /^\(rbdbgr\): /
    RBDBGR_LOC    = /.. \(.+:\d+\)/
  end

  # Return the caller's line number
  def get_lineno
    RubyVM::ThreadFrame.current.prev.source_location[0]
  end

  def compare_output(right, d, debugger_cmds)
    # require_relative %w(.. .. lib rbdbgr)
    # dbgr = Debugger.new(:set_restart => true)
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
    assert_equal(right, got)
  end

  # Return output with source lines prompt and command removed
  def filter_line_cmd(a, show_prompt=false)
    # Remove line locations and extra leading spaces.
    # For example:
    # -- 42         y = 5
    # becomes
    # -- y = 5
    a = a.map do |s|
     s =~ RBDBGR_PROMPT ? nil : s
    end.compact unless show_prompt

    # Remove debugger location lines. 
    # For example: 
    # -- (/src/external-vcs/rbdbgr/tmp/gcd.rb:4)
    a2 = a.map do |s|
      s =~ RBDBGR_LOC ? s.gsub(/\(.+:\d+\)\n/, '').chomp : s.chomp
    end

    # Remove VM offset locationss. 
    # For example: 
    # -- (/src/external-vcs/rbdbgr/tmp/gcd.rb:4)
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
  p '-- (/src/external-vcs/rbdbgr/tmp/gcd.rb:4)' =~ RBDBGR_LOC
  p '(rbdbgr): exit' =~ RBDBGR_PROMPT
  output='
-- (/src/external-vcs/rbdbgr/tmp/gcd.rb:4)
(rbdbgr): s
-- (/src/external-vcs/rbdbgr/tmp/gcd.rb:18)
(rbdbgr): s
-- (/src/external-vcs/rbdbgr/tmp/gcd.rb:19)
(rbdbgr): s
.. (/src/external-vcs/rbdbgr/tmp/gcd.rb:0)
(rbdbgr): s
-> (/src/external-vcs/rbdbgr/tmp/gcd.rb:4)
'.split(/\n/)
  puts filter_line_cmd(output)
  puts '-' * 10
  puts filter_line_cmd(output, true)
end
