class Debugger
  module Help

    def abbrev_stringify(name, min_abbrev)
      "(#{name[0..min_abbrev-1]})#{name[min_abbrev..-1]}"
    end

    def summary_help(subcmd)
      # Set class constant SHORT_HELP to be the first line of HELP
      # unless it has been defined in the class already.
      # The below was the simplest way I could find to do this since
      # we are the super class but want to set the subclass's constant.
      # defined? didn't seem to work here.
      c = subcmd.class.constants
      if c.member?(:HELP) and !c.member?(:SHORT_HELP)
        short_help = subcmd.class.const_get('HELP').split("\n")[0].chomp('.')
        subcmd.class.const_set(:SHORT_HELP, short_help)
      end
      
      msg('  %-12s -- %s' %
          [abbrev_stringify(obj_const(subcmd, :NAME), 
                            obj_const(subcmd, :MIN_ABBREV)),
           obj_const(subcmd, :SHORT_HELP)])
    end

    # We were given cmd without a subcommand; cmd is something
    # like "show", "info" or "set". Generally this means list
    # all of the subcommands.
    def summary_list(name, subcmds)
      msg "List of #{name} commands (with minimum abbreviation in parenthesis):"
      subcmds.list.each do |subcmd_name|
        # Some commands have lots of output.
        # they are excluded here because 'in_list' is false.
        msg summary_help(subcmds.subcmds[subcmd_name])
      end
    end


    # Error message when subcommand asked for but doesn't exist
    def undefined_subcmd(cmd, subcmd)
      errmsg(('Undefined "%s" subcommand: "%s". ' + 
              'Try "help %s *".') % [cmd, subcmd, cmd])
    end

  end
end

if __FILE__ == $0
  class TestClass
    include Debugger::Help
    HELP       = 'TestClass HELP.

Long description goes here.'
    MIN_ABBREV = 1
    NAME       = File.basename(__FILE__)
    def obj_const(obj, name)
      obj.class.const_get(name) 
    end
    def msg(mess)
      puts mess
    end
    def errmsg(mess)
      puts "***#{mess}"
    end
    def initialize
      summary_help(self)
      undefined_subcmd('foo', 'bar')
    end
  end
  TestClass.new
end
