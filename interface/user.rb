# -*- coding: utf-8 -*-
# Interface when communicating with the user in the same process as
# the debugged program.

# Our local modules

require_relative 'base_intf'
require_relative %w(.. io input)

# Moutput    = import_relative('dbg_output', '..io', 'pydbgr')

# Interface when communicating with the user in the same
# process as the debugged program.
class Debugger::UserInterface < Debugger::Interface

  FILE_HISTORY = '.rbdbgr_hist' unless defined?(FILE_HISTORY)

  def initialize(inp=nil, out=nil, opts={})
    at_exit { finalize }
    super(inp, out, opts)
    @input = if inp.class.ancestors.member?(Debugger::InputBase)
               inp
             else
               Debugger::UserInput.open(inp)
             end
  end

  # Closes both input and output

  # Called when a dangerous action is about to be done, to make
  # sure it's okay. Expect a yes/no answer to `prompt' which is printed,
  # suffixed with a question mark and the default value.  The user
  # response converted to a boolean is returned.
  def confirm(prompt, default)
    default_str = default ? 'Y/n' : 'N/y'
    while true do
      begin 
        response = readline('%s (%s) ' % [prompt, default_str])
      rescue EOFError
        return default
      end
      response = response.strip.downcase

      # We don't catch "Yes, I'm sure" or "NO!", but I leave that 
      # as an exercise for the reader.
      break if YES_OR_NO.member?(response)
      msg "Please answer 'yes' or 'no'. Try again."
    end
    return YES.member?(response)
  end

  def finalize(last_wishes=nil)
    # print exit annotation
    # save history
    super
  end

  def interactive? ; @input.interactive? end

  def read_command(prompt='')
    line = readline(prompt)
    # FIXME: Do something with history?
    return line
  end
end

# Demo
if __FILE__ == $0
  intf = Debugger::UserInterface.new
  intf.errmsg("Houston, we have a problem here!")
  if ARGV.size > 0
    begin
      line = intf.readline("Type something: ")
    rescue EOFError
      puts "No input, got EOF"
    else
      puts "You typed: #{line}"
    end
    puts "EOF is now: %s" % intf.input.eof?.inspect
    unless intf.input.eof?
      line = intf.confirm("Are you sure", false)
      puts "You typed: #{line}"
      puts "EOF is now: %s" % intf.input.eof?.inspect
      line = intf.confirm("Are you not sure", true)
      puts "You typed: #{line}"
      puts "EOF is now: %s" % intf.input.eof?.inspect
    end
  end
end
