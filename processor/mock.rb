# Mock setup for commands.
require_relative 'main'
require_relative %w(.. lib core)
require_relative %w(.. lib default)
require_relative %w(.. interface user)  # user interface (includes I/O)

module MockDebugger
  class MockDebugger
    attr_accessor :core         # access to Debugger::Core instance
    attr_accessor :intf         # The way the outside world interfaces with us.
    attr_accessor :restart_argv # How to restart us, empty or nil. 
    # Note restart[0] is typically $0.
    attr_reader   :settings     # Hash[:symbol] of things you can configure

    def initialize(debugger, settings={})
      @settings = DbgSettings::DEFAULT_SETTINGS.merge(settings)
      @intf     = [Debugger::UserInterface.new]
      @core     = Debugger::Core.new(self)
    end

  end

  # Common Mock debugger setup 
  def setup(name)
    if ARGV.size > 0 
      if ARGV[0] == 'debug'
        require_relative %w(.. rbdbgr)
        dbgr = Debugger.new()
        dbgr.debugger
      end
    else
      dbgr = MockDebugger.new(nil)
    end
    cmds = dbgr.core.processor.commands
    cmd  = cmds[name]

    def cmd.msg(message)
      puts message
    end
    def cmd.errmsg(message)
      puts "Error: #{message}"
    end
    def cmd.confirm(prompt, default)
      true
    end

    return dbgr, cmd
  end
  module_function :setup

  def show_special_class_constants(cmd)
    puts 'ALIASES: %s' % [cmd.class.const_get('ALIASES').inspect] if
      cmd.class.constants.member?(:ALIASES)
    %w(CATEGORY HELP MIN_ARGS MAX_ARGS 
       NAME NEED_STACK SHORT_HELP).each do |name|
      puts '%s: %s' % [name, cmd.class.const_get(name).inspect]
    end
    puts '- - -'
  end
  module_function :show_special_class_constants

end
