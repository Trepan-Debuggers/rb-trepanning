# -*- coding: utf-8 -*-
# Copyright (C) 2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require 'rubygems'
require 'pp'
require_relative '../command'
class Trepan::Command::PPCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
**#{NAME}** *Ruby-expression*

Prtty Print the value of the *expression*. Variables accessible are
those of the environment of the selected stack frame, plus globals.

If the length output string large, the first part of the value is
shown and "..." indicates it has been truncated.

See also:
---------
`set max string` to change the string truncation limit.
HELP
    CATEGORY      = 'data'
    SHORT_HELP    = 'pretty print expression truncating long output'
  end

  def run(args)
    obj = @proc.debug_eval(@proc.cmd_argstr)
    msg (obj.respond_to?(:pretty_inspect) ? obj.pretty_inspect : obj.inspect)
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  ['(0..10).to_a', '$LOADED_FEATURES'].each do |expr_str|
    cmd_argstr = expr_str
      cmd.proc.instance_variable_set('@cmd_argstr', cmd_argstr)
    cmd.run([cmd.name, cmd_argstr])
    puts '-' * 20
  end
end
