# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Debugger user/command-oriented input possibly attached to IO-style
# input or GNU Readline.
# 

require_relative 'base_io'

class Trepan

  # Debugger user/command-oriented input possibly attached to IO-style
  # input or GNU Readline.
  class UserInput < Trepan::InputBase

    @@readline_finalized = false

    def initialize(inp, opts={})
      @opts         = DEFAULT_OPTS.merge(opts)
      @input        = inp || STDIN
      @eof          = false
      @line_edit    = @opts[:line_edit]
      @use_readline = @opts[:readline]
    end

    def closed?; @input.closed? end
    def eof?; @eof end

    def interactive? 
      @input.respond_to?(:isatty) && @input.isatty
    end
    # Read a line of input. EOFError will be raised on EOF.  
    def readline(prompt='')
      raise EOFError if eof?
      begin 
        if @line_edit && @use_readline
          line = Readline.readline(prompt, true)
        else
          line = @input.gets
          end
      rescue Interrupt
        return ''
      rescue EOFError
      rescue => e
        puts $!.backtrace
        puts "Exception caught #{e.inspect}"
        @eof = true
      end
      @eof = !line
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
        opts[:line_edit] = @line_edit = 
          inp.respond_to?(:isatty) && inp.isatty && Trepan::GNU_readline?
        self.new(inp, opts)
      end

      def finalize
       if defined?(RbReadline) && !@@readline_finalized
          begin 
            RbReadline.rl_cleanup_after_signal()
          rescue
          end
          begin 
            RbReadline.rl_deprep_terminal()
          rescue
          end
          @@readline_finalized = true
        end
      end
    end
  end
end

def Trepan::GNU_readline?
  begin
    return @have_readline unless @have_readline.nil?
    @have_readline = require 'readline'
    at_exit { Trepan::UserInput::finalize }
    return true
  rescue LoadError
    return false
  end
end
    
# Demo
if __FILE__ == $0
  puts 'Have GNU is: %s'  % Trepan::GNU_readline?
  inp = Trepan::UserInput.open(__FILE__, :line_edit => false)
  line = inp.readline
  puts line
  inp.close
  filename = 'input.py'
  begin
    Trepan::UserInput.open(filename)
  rescue
    puts "Can't open #{filename} for reading: #{$!}"
  end
  inp = Trepan::UserInput.open(__FILE__, :line_edit => false)
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
    inp = Trepan::UserInput.open
    begin
      print "Type some characters: "
      line = inp.readline()
      puts "You typed: %s" % line
    rescue EOFError
      puts 'Got EOF'
    end
  end
end


