# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../../base/subsubcmd'

class Trepan::SubSubcommand::SetTraceBuffer < Trepan::SetBoolSubSubcommand
  unless defined?(HELP)
    Trepanning::SubSubcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD} [on|off|1|0]

Set saving trace events in a buffer
    EOH
    MIN_ABBREV   = 'b'.size  
    SHORT_HELP   = 'Set saving trace events in a buffer'
  end

  def run(args)
    super
    if settings[:tracebuffer]
      # @proc.start_capture
      @proc.unconditional_prehooks.insert_if_new(-1, *@proc.tracebuf_hook)
    else
      @proc.unconditional_prehooks.delete_by_name('tracebuffer')
      # @proc.stop_capture
    end
  end

end

if __FILE__ == $0
  # Demo it.
  require_relative '../../../mock'
  require_relative '../trace'
  cmd = MockDebugger::subsub_setup(Trepan::SubSubcommand::SetTrace,
                                   Trepan::SubSubcommand::SetTraceBuffer)
  %w(off on 1 0).each do |arg|
      cmd.run([cmd.name, arg])
  end
  puts cmd.save_command()
end

