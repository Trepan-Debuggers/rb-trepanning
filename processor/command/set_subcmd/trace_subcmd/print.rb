# -*- coding: utf-8 -*-
# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../trace'
class Trepan::SubSubcommand::SetTracePrint < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    HELP         = 
"set trace print [on|off|1|0]

Set printing trace events."

    MIN_ABBREV   = 'p'.size  
    NAME         = File.basename(__FILE__, '.rb')
    PREFIX       = %w(set trace print)
    SHORT_HELP   = 'Set print trace events'
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
  %w(off on 0 1).each { |arg| cmd.run([cmd.name, arg]) }
  puts '-' * 10
end

