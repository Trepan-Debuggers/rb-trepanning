# -*- coding: utf-8 -*-
# Debugger user/command-oriented input possibly attached to IO-style
# input or GNU Readline.
# 

require_relative 'base_io'

class Debugger

  # Debugger user/command-oriented input possibly attached to IO-style
  # input or GNU Readline.
  class UserInput < Debugger::InputBase

    def initialize(inp, opts={})
      @opts      = DEFAULT_OPTS.merge(opts)
      @input     = inp || STDIN
      @eof       = false
    end

    def eof?; @eof end

    def interactive? 
      @input.respond_to?(:isatty) && @input.isatty
    end

    # Read a line of input. EOFError will be raised on EOF.  
    # 
    # Note that we don't support prompting first. Instead, arrange
    # to call Debugger::Output.write() first with the prompt. 
    def readline
      # FIXME we don't do command completion.
      raise EOFError if eof?
      begin 
        line = @opts[:line_edit] ? Readline.readline : @input.gets
        @eof = !line
      rescue
        @eof = true
      end
      raise EOFError if eof?
      return line
    end
    
    class << self
      # Use this to set where to read from. 
      #
      # Set opts[:line_edit] if you want this input to interact with
      # GNU-like readline library. By default, we will assume to try
      # using readline. 
      def open(inp=nil, opts={})
        inp ||= STDIN
        inp = File.new(inp, 'r') if inp.is_a?(String)
        opts[:line_edit] = false unless
          inp.respond_to?(:isatty) && inp.isatty && Debugger::GNU_readline?
        self.new(inp, opts)
      end
    end
  end
end

def Debugger::GNU_readline?
  begin
    require 'readline'
    return true
  rescue LoadError
    return false
  end
end
    
# Demo
if __FILE__ == $0
  puts 'Have GNU is: %s'  % Debugger::GNU_readline?
  inp = Debugger::UserInput.open(__FILE__, :line_edit => false)
  line = inp.readline
  puts line
  inp.close
  filename = 'input.py'
  begin
    Debugger::UserInput.open(filename)
  rescue
    puts "Can't open #{filename} for reading: #{$!}"
  end
  inp = Debugger::UserInput.open(__FILE__, :line_edit => false)
  while true
    begin
      inp.readline
    rescue EOFError
      break
    end
  end
  begin
    inp.readline
  rescue EOFError
      puts 'EOF handled correctly'
  end

  if ARGV.size > 0
    inp = Debugger::UserInput.open
    begin
      print "Type some characters: "
      line = inp.readline()
      puts "You typed: %s" % line
    rescue EOFError
      puts 'Got EOF'
    end
  end
end


