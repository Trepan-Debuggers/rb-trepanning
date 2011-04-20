# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'

class Trepan::Subcommand::InfoRuby < Trepan::Subcommand
  unless defined?(HELP)
    Trepanning::Subcommand.set_name_prefix(__FILE__, self)
    HELP         = <<-EOH
#{CMD}

Show Ruby version information such as you'd get from "ruby -v", which
is really just the value of RUBY_RELEASE_DATE.

See also constants: RUBY_DESCRIPTION, RUBY_VERSION, and RUBY_RELEASE_DATE."
    EOH
    MIN_ABBREV   = 'ru'.size
    NEED_STACK   = false
    MIN_ARGS     = 0
    MAX_ARGS     = 0
    SHORT_HELP   = 'Ruby version information'
   end

  # def parse_options(args) # :nodoc
  #   options = {}
  #   parser = OptionParser.new do |opts|
  #     opts.on("-v", 
  #             "--[no-]verbose", "show additional version information") do
  #       |v| 
  #       options[:verbose] = v
  #     end
  #   end
  #   parser.parse(args)
  #   return options
  # end

  def run(args)
    # options = parse_options(args[2..-1])
    msg RUBY_DESCRIPTION
    # if options[:verbose]
    #   msg "Options:"
    #   msg "  Interpreter type: #{Rubinius::INTERPRETER}"
    #   if jit = Rubinius::JIT
    #     msg "  JIT enabled: #{jit.join(', ')}"
    #   else
    #     msg "  JIT disabled"
    #   end
    # end
  end

end

if __FILE__ == $0
  # Demo it.
  $0 = __FILE__ + 'notagain' # So we don't run this again
  require_relative '../../mock'
  cmd = MockDebugger::sub_setup(Trepan::Subcommand::InfoRuby, false)
  cmd.run(cmd.prefix)
  %w(-v --verbose --no-verbose).each do |opt|
    puts '-' * 10 + " #{opt}"
    cmd.run(cmd.prefix + [opt])
  end
end
