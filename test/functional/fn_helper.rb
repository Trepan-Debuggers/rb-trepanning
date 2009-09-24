require 'thread_frame'
require_relative %w(.. .. rbdbgr)
require_relative %w(.. .. io string_array)

module FnTestHelper
  # Common setup to create a debugger with stringio attached
  def strarray_setup(debugger_cmds)
    stringin               = Debugger::StringArrayInput.open(debugger_cmds)
    stringout              = Debugger::StringArrayOutput.open
    d_opts                 = {:input  => stringin, :output => stringout}
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
    got = filter_line_cmd(d.intf[-1].output.output)
    if got != right
      got.each_with_index do |got_line, i|
        if i < right.size and got_line != right[i]
          puts "! #{got_line}"
        else
          puts "  #{got_line}"
        end
      end
      puts '-' * 10
      right.each_with_index do |right_line, i|
        if i < got.size and got[i] != right_line
          puts "! #{right_line}"
        else
          puts "  #{right_line}"
        end
      end
    end
    assert_equal(right, got)
  end

  # Return output with source lines prompt and command removed
  def filter_line_cmd(a)
    # Remove line locations and extra leading spaces.
    # For example:
    # -- 42         y = 5
    # becomes
    # -- y = 5
    ## a1 = a.map do |s|
    ##  s.gsub(/ \d+\s+/, '') if s =~ RBDBGR_PROMPT
    ## end
    # Remove debugger location lines. 
    # For example: 
    # -- (/src/external-vcs/rbdbgr/tmp/gcd.rb:4)
    a2 = a.map do |s|
      s =~ RBDBGR_LOC ? s.gsub(/\(.+:\d+\)/, '(file:999)') : s
    end
    return a2
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
end
