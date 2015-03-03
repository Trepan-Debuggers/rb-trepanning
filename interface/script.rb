# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>

# Module for reading debugger scripts

# Our local modules
require_relative '../interface'
require_relative '../io/input'
require_relative '../io/string_array'

# Interface when reading debugger scripts
class Trepan::ScriptInterface < Trepan::Interface

  DEFAULT_OPTS = {
    :abort_on_error => true,
    :confirm_val    => false,
    :verbose        => false
  } unless defined?(DEFAULT_OPTS)

  def initialize(script_name, out=nil, opts={})

    @opts = DEFAULT_OPTS.merge(opts)

    at_exit { finalize }
    @script_name     = script_name
    @input_lineno    = 0
    @input           = opts[:input] ||
      Trepan::UserInput.open(script_name, :line_edit => false)
    @buffer_output   = []
    unless @opts[:verbose] or out
      out = Trepan::StringArrayOutput.open(@buffer_output)
    end
    super(@input, out, @opts)
  end

  # Closes input only.
  def close
    # @input.close unless @input.closed?
  end

  # Called when a dangerous action is about to be done, to make
  # sure it's okay.
  #
  # Could look also look for interactive input and
  # use that. For now, though we'll simplify.
  def confirm(prompt, default)
    @opts[:default_confirm]
  end

  # Common routine for reporting debugger error messages.
  #
  def errmsg(msg, prefix="*** ")
    #  self.verbose shows lines so we don't have to duplicate info
    #  here. Perhaps there should be a 'terse' mode to never show
    #  position info.
    mess = if not @opts[:verbose]
             location = ("%s:%s: Error in source command file" %
                         [@script_name, @input_lineno])
             "%s%s:\n%s%s" % [prefix, location, prefix, msg]
           else
             "%s%s" % [prefix, msg]
           end
    msg(mess)
    # FIXME: should we just set a flag and report eof? to be more
    # consistent with File and IO?
    raise IOError if @opts[:abort_on_error]
  end

  def interactive? ; false end

  # Script interface to read a command. `prompt' is a parameter for
  # compatibilty and is ignored.
  def read_command(prompt='')
    @input_lineno += 1
    line = readline
    if @opts[:verbose]
      location = "%s line %s" % [@script_name, @input_lineno]
      msg('+ %s: %s' % [location, line])
    end
    # Do something with history?
    return line
  end

  # Script interface to read a line. `prompt' is a parameter for
  # compatibilty and is ignored.
  #
  # Could decide make this look for interactive input?
  def readline(prompt='')
    begin
      return input.readline().chomp
    rescue EOFError
      @eof = true
      raise EOFError
    end
  end
end

# Demo
if __FILE__ == $0
  intf = Trepan::ScriptInterface.new(__FILE__)
  line = intf.readline()
  print "Line read: ", line
end
