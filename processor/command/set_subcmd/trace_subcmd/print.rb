# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'
require_relative '../trace'
class Trepan::SubSubcommand::SetTracePrint < Trepan::SetBoolSubSubcommand
  Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
  unless defined?(HELP)
    HELP         = <<-EOH
"#{CMD} [on|off|1|0]

Set printing trace events.
    EOH

    MIN_ABBREV   = 'p'.size  
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
  %w(off on 1 0).each do |arg|
      cmd.run([cmd.name, arg])
  end
  puts cmd.save_command()
end

