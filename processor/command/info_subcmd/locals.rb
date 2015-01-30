# -*- coding: utf-8 -*-
# Copyright (C) 2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../base/subcmd'
require_relative './variables_subcmd/locals'

class Trepan::Subcommand::InfoLocals < Trepan::Subcommand
    unless defined?(HELP)
        Trepanning::Subcommand.set_name_prefix(__FILE__, self)
        HELP = 'Same thing as "info variables locals"'
        NEED_LOCALS   = true
    end

    def complete(prefix)
        if @proc && @proc.frame
            argc =
                if 'CFUNC' == @proc.frame.type
                    @proc.frame.argc
                else
                    iseq = @proc.frame.iseq
                    iseq.local_size - 2
                end
            ary = (0..argc).map{|i| i.to_s}
            ['names'] + ary
        else
            []
        end
    end

    def run(args)
        @proc.commands['info'].run(%w(info variables) + args[1..-1])
    end
end

if __FILE__ == $0
    # Demo it.
    require_relative '../../mock'
    # ???
end
