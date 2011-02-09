# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>

# Interface when communicating with the user in the same process as
# the debugged program.

# Our local modules

require_relative 'base_intf'
require_relative '../io/input'

# Interface when communicating with the user in the same
# process as the debugged program.
class Trepan::UserInterface < Trepan::Interface

  FILE_HISTORY = '.trapan_hist' unless defined?(FILE_HISTORY)

  def initialize(inp=nil, out=nil, opts={})
    super(inp, out, opts)
    @input = if inp.class.ancestors.member?(Trepan::InputBase)
               inp
             else
               Trepan::UserInput.open(inp)
             end
     if Trepan::GNU_readline? && opts[:complete]
      Readline.completion_proc = opts[:complete]
      # Use gdb's default setting
      @opts[:history_length] ||= 
        ENV['HISTSIZE'] ? ENV['HISTSIZE'].to_i : 256  
      Readline.completion_proc = @opts[:complete]
      @history_path = File.expand_path("~/.trepanx")

      if File.exists?(@history_path)
        File.readlines(@history_path).each do |line|
          Readline::HISTORY << line.strip
        end
        @history_io = File.new(@history_path, "a")
      else
        @history_io = File.new(@history_path, "w")
      end
      @history_io.sync = true
      @history_save = true
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

  def save_history 
    iface.histfile ||= File.join(ENV['HOME']||ENV['HOMEPATH']||'.', 
                                 FILE_HISTORY)
    open(iface.histfile, 'w') do |file|
      Readline::HISTORY.to_a.last(iface.history_length).each do |line|
        file.puts line unless line.strip.empty?
      end if defined?(iface.history_save) and iface.history_save
    end rescue nil
  end

  def finalize(last_wishes=nil)
    # print exit annotation
    if Trepan::GNU_readline? && @history_save
      save_history 
    end
    super
  end

  def interactive? ; @input.interactive? end

  def read_command(prompt='')
    line = readline(prompt)
    # FIXME: Do something with history?
    return line
  end

  def readline(prompt='')
    @output.flush
    line = 
      if @input.line_edit
        @input.readline(prompt)
        # FIXME: Do something with history?
      else
        @output.write(prompt) if prompt and prompt.size > 0
        @input.readline
      end
    return line
  end

end

# Demo
if __FILE__ == $0
  intf = Trepan::UserInterface.new
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
