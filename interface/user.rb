# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Interface when communicating with the user.

# Our local modules
require_relative 'base_intf'
require_relative '../io/input'

# Interface when communicating with the user.
class Trepan::UserInterface < Trepan::Interface

  DEFAULT_USER_OPTS = {
    :histsize => 256,                   # Use gdb's default setting
    :file_history   => '.trepan_hist',  # where history file lives
                                        # Note a directory will 
                                        # be appended
    :history_save   => true             # do we save the history?
  } unless defined?(DEFAULT_USER_OPTS)

  def initialize(inp=nil, out=nil, opts={})
    super(inp, out, opts)
    @opts = DEFAULT_USER_OPTS.merge(opts)
    @input = if inp.class.ancestors.member?(Trepan::InputBase)
               inp
             else
               Trepan::UserInput.open(inp)
             end
    if Trepan::GNU_readline?
      Readline.completion_proc = opts[:complete] if opts[:complete]
      read_history
    end
    at_exit { finalize }
  end

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

  # Read a saved Readline history file into Readline. The history
  # file will be created if it doesn't already exist.
  # Much of this code follows what's done in ruby-debug.
  def read_history 
    unless @histfile
      dirname = ENV['HOME'] || ENV['HOMEPATH'] || File.expand_path('~')
      @histfile = File.join(dirname, @opts[:file_history])
    end
    @histsize ||= (ENV['HISTSIZE'] ? ENV['HISTSIZE'].to_i : @opts[:histsize])
    Readline.completion_proc = @opts[:complete]
    if File.exists?(@histfile)
      lines = IO::readlines(@histfile).last(@histsize).collect do  
        |line| line.chomp 
      end
      Readline::HISTORY.push(*lines)
      @history_io = File.new(@histfile, "a")
    else
      @history_io = File.new(@histfile, "w")
    end
    @history_io.sync = true
    @history_save = @opts[:history_save]
  end

  def save_history 
    if @histfile
      lines = Readline::HISTORY.to_a
      lines = lines[-@histsize, @histsize] if lines.size > @histsize
      lines = lines.select
      File::open(@histfile, 'w') do |file| 
        file.puts lines
      end if defined?(@history_save) and @history_save
      begin
        open(@histfile, 'w') do |file|
          Readline::HISTORY.to_a.last(@histsize).each do |line|
            file.puts line
          end 
        end if defined?(@history_save) and @history_save
      rescue
      end
    end
  end

  def finalize(last_wishes=nil)
    # ?? print gdb-style exit annotation if annotate = 2?
    if Trepan::GNU_readline? && @history_save
      save_history 
    end
    super
  end

  def interactive? ; @input.interactive? end

  def read_command(prompt=''); readline(prompt) end

  def readline(prompt='')
    @output.flush
    if @input.line_edit
      @input.readline(prompt)
    else
      @output.write(prompt) if prompt and prompt.size > 0
      @input.readline
    end
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
