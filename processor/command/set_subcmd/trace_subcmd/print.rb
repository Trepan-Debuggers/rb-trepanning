# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetTracePrint < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD} [on|off]

Set printing trace events. This is similar to "set -x" tracing in 
POSIX shells.
    EOH

    MIN_ABBREV   = 'p'.size  
    SHORT_HELP   = 'Set print trace events, like "set -x" of POSIX shell'
  end

  def run(args)
    super
    if settings[:traceprint]
      @proc.unconditional_prehooks.insert_if_new(-1, *@proc.trace_hook)
    else
      @proc.unconditional_prehooks.delete_by_name('trace')
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../trace'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetTrace,
                                   Trepan::SubSubcommand::SetTracePrint)
  %w(off on 1 0).each do |arg|
      cmd.run([cmd.name, arg])
  end
  puts cmd.save_command()
end

