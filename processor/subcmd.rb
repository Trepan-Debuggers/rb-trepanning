# gdb-like subcommand processing.

class Debugger
  class Subcmd

    attr_reader :subcmds
    def initialize(cmd)
      @cmd     = cmd
      @subcmds = {}
      @cmdlist = []
    end
    
    # Find subcmd in self.subcmds
    def lookup(subcmd_prefix)
      pat = /^#{subcmd_prefix}/
      @subcmds.each do |subcmd_name, subcmd|
        if subcmd_name =~ pat &&
            subcmd_prefix.size >= subcmd.class.const_get(:MIN_ABBREV)
          return subcmd
        end
      end
      return nil
    end

    # Show short help for a subcommand.
    def short_help(subcmd_cb, subcmd_name, label=false)
      entry = self.lookup(subcmd_name)
      if entry
        if label
          prefix = entry.name
        else
          prefix = ''
        end
        if entry.respond_to?(:short_help)
          prefix += ' -- ' if prefix 
          @proc.msg(prefix + entry.short_help)
        end
      else
        @proc.undefined_subcmd("help", subcmd_name)
      end
    end

    # Add subcmd to the available subcommands for this object.
    # It will have the supplied docstring, and subcmd_cb will be called
    # when we want to run the command. min_len is the minimum length
    # allowed to abbreviate the command. in_list indicates with the
    # show command will be run when giving a list of all sub commands
    # of this object. Some commands have long output like "show commands"
    # so we might not want to show that.
    def add(subcmd_cb, subcmd_name=nil)
      subcmd_name ||= subcmd_cb.name
      @subcmds[subcmd_name] = subcmd_cb

      # We keep a list of subcommands to assist command completion
      @cmdlist << subcmd_name
    end

    # Run subcmd_name with args using obj for the environent
    def run( subcmd_name, arg)
      entry=lookup(subcmd_name)
      if entry
        entry['callback'].send(arg)
      else
        @proc.undefined_cmd(entry.__class__.name, subcmd_name)
      end
    end

    # help for subcommands
    # Note: format of help is compatible with ddd.
    def help(*args)

      msg args
      subcmd_prefix = args[0]
      if not subcmd_prefix or subcmd_prefix.size == 0
        @proc.msg(self.doc)
        @proc.msg("\nList of %s subcommands:\n" % [@name])
        @list.each do |subcmd_name|
          subcmd_helper(subcmd_name, obj, true, true)
        end

        entry = lookup(subcmd_prefix)
        if entry and entry.respond_to? :help
          entry.help(args)
        else
          @proc.errmsg("Unknown 'help %s' subcommand %s" %
                       [@name, subcmd_prefix])
        end
      end
    end

    def list
      @subcmds.keys.sort
    end
    
    # Error message when a subcommand doesn't exist.
    def undefined_subcmd(cmd, subcmd)
      @proc.errmsg('Undefined "%s" command: "%s". Try "help".' % 
                   [cmd, subcmd])
    end
  end
end

# When invoked as main program, invoke the debugger on a script
if __FILE__ == $0

  require_relative('mock')
  require_relative(%w(command base_cmd))

  class Debugger::TestCommand < Debugger::Command
    
    HELP = 'Help string string for testing'
    CATEGORY = 'data'
    MIN_ARGS = 0
    MAX_ARGS = 5
    NAME_ALIASES = %w(test)
    
    def initialize(proc); @proc  = proc end
    
    def run(args); puts 'test command run' end
  end
  
  class TestTestingSubcommand
    HELP = 'Help string for test testing subcommand'
    
    def initialize; @name  = 'testing' end
    
    SHORT_HELP = 'This is short help for test testing'
    MIN_ABREV = 4
    IN_LIST   = true
    def run(args); puts 'test testing run' end
  end

  d = MockDebugger::MockDebugger.new
  testcmd    = Debugger::TestCommand.new(nil)
  # testcmd.debugger = d
  testcmd.proc     = d.core.processor
  # testcmdMgr = Subcmd.new('test', testcmd)
  # testsub = TestTestingSubcommand.new
  # testcmdMgr.add(testsub)
  
  # %w(tes test testing testing1).each do |prefix|
  #   x = testcmdMgr.lookup(prefix)
  #   puts x ? x.name : 'Non'
  # end
  
  # testcmdMgr.short_help(testcmd, 'testing')
  # testcmdMgr.short_help(testcmd, 'test', true)
  # testcmdMgr.short_help(testcmd, 'tes')
  # puts testcmdMgr.list()
  # testsub2 = TestTestingSubcommand.new
  # testsub2.name = 'foobar'
  # testcmdMgr.add(testsub2)
  # puts testcmdMgr.list()
end
