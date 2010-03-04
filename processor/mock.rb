# Mock setup for commands.
require_relative 'main'
require_relative %w(.. app core)
require_relative %w(.. app default)
require_relative %w(.. interface user)  # user interface (includes I/O)

SCRIPT_ISEQS__ = {} unless 
  defined?(SCRIPT_ISEQS__) && SCRIPT_ISEQS__.is_a?(Hash)
ISEQS__        = {} unless 
  defined?(ISEQS__) && ISEQS__.is_a?(Hash)

module MockDebugger
  class MockDebugger
    attr_accessor :trace_filter # Procs/Methods we ignore.

    attr_accessor :core         # access to Debugger::Core instance
    attr_accessor :intf         # The way the outside world interfaces with us.
    attr_reader   :initial_dir  # String. Current directory when program
                                # started. Used in restart program.
    attr_accessor :restart_argv # How to restart us, empty or nil. 
                                # Note restart[0] is typically $0.
    attr_reader   :settings     # Hash[:symbol] of things you can configure

    def initialize(settings={})
      @before_cmdloop_hooks = []
      @settings             = Rbdbgr::DEFAULT_SETTINGS.merge(settings)
      @intf                 = [Debugger::UserInterface.new]
      @core                 = Debugger::Core.new(self)
      @trace_filter         = []
    end

  end

  # Common Mock debugger setup 
  def setup(name, show_constants=true)
    if ARGV.size > 0 && ARGV[0] == 'debug'
      require_relative %w(rbdbgr)
      dbgr = Debugger.new
      dbgr.debugger
    else
      dbgr = MockDebugger.new
    end

    cmds = dbgr.core.processor.commands
    cmd  = cmds[name]
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current.prev)
    show_special_class_constants(cmd) if show_constants

    def cmd.msg(message)
      puts message
    end
    def cmd.msg_nocr(message)
      print message
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

if __FILE__ == $0
  dbgr = MockDebugger::MockDebugger.new
end
