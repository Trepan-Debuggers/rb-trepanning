# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
# Mock setup for commands.
require_relative 'main'

require_relative '../app/core'
require_relative '../app/default'
require_relative '../app/frame'
require_relative '../interface/user'  # user interface (includes I/O)

SCRIPT_ISEQS__ = {} unless 
  defined?(SCRIPT_ISEQS__) && SCRIPT_ISEQS__.is_a?(Hash)
ISEQS__        = {} unless 
  defined?(ISEQS__) && ISEQS__.is_a?(Hash)

module MockDebugger
  class MockDebugger
    attr_accessor :trace_filter # Procs/Methods we ignore.

    attr_accessor :frame        # Actually a "Rubinius::Location object
    attr_accessor :core         # access to Debugger::Core instance
    attr_accessor :intf         # The way the outside world interfaces with us.
    attr_reader   :initial_dir  # String. Current directory when program
                                # started. Used in restart program.
    attr_accessor :restart_argv # How to restart us, empty or nil. 
                                # Note restart[0] is typically $0.
    attr_reader   :settings     # Hash[:symbol] of things you can configure
    attr_accessor :processor

    # FIXME: move more stuff of here and into Trepan::CmdProcessor
    # These below should go into Trepan::CmdProcessor.
    attr_reader :cmd_argstr, :cmd_name, :vm_locations, :current_frame, 
                :debugee_thread

    def initialize(settings={})
      @before_cmdloop_hooks = []
      @settings             = Trepan::DEFAULT_SETTINGS.merge(settings)
      @intf                 = [Trepan::UserInterface.new]
      @core                 = Trepan::Core.new(self)
      @trace_filter         = []

      # Don't allow user commands in mocks.
      @core.processor.settings[:user_cmd_dir] = nil 

    end

    def frame(num)
      @frames[num] ||= Trepan::Frame.new(self, num, @vm_locations[num])
    end
  end

  # Common Mock debugger setup 
  def setup(name=nil, show_constants=true)
    unless name
      tf = RubyVM::ThreadFrame.current.prev
      name = File.basename(tf.source_container[1], '.rb')
    end
    if ARGV.size > 0 && ARGV[0] == 'debug'
      require_relative '../lib/trepanning'
      dbgr = Trepan.new
      dbgr.debugger
    else
      dbgr = MockDebugger.new
    end

    cmds = dbgr.core.processor.commands
    cmd  = cmds[name]
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current.prev)
    show_special_class_constants(cmd) if show_constants

    def cmd.confirm(prompt, default)
      true
    end
    def cmd.errmsg(message)
      puts "Error: #{message}"
    end
    def cmd.msg(message)
      puts message
    end
    def cmd.msg_nocr(message)
      print message
    end
    def cmd.section(message, opts={})
      puts "Section: #{message}"
    end

    return dbgr, cmd
  end
  module_function :setup

  def sub_setup(sub_class, run=true)
    sub_name = sub_class.const_get('PREFIX')
    dbgr, cmd = setup(sub_name[0], false)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current.prev)
    sub_cmd = sub_class.new(cmd)
    sub_cmd.summary_help(sub_cmd.name)
    puts
    sub_cmd.run([cmd.name]) if run
    return sub_cmd
  end
  module_function :sub_setup

  def subsub_setup(sub_class, subsub_class, run=true)
    subsub_name = subsub_class.const_get('PREFIX')
    dbgr, cmd = setup(subsub_name[0], false)
    cmd.proc.frame_setup(RubyVM::ThreadFrame::current.prev)
    sub_cmd = sub_class.new(dbgr.core.processor, cmd)
    subsub_cmd = subsub_class.new(cmd.proc, sub_cmd, subsub_name.join(''))
    subsub_cmd.summary_help(subsub_cmd.name)
    puts
    subsub_cmd.run([]) if run
    return subsub_cmd
  end
  module_function :subsub_setup

  def show_special_class_constants(cmd)
    puts 'ALIASES: %s' % [cmd.class.const_get('ALIASES').inspect] if
      cmd.class.constants.member?(:ALIASES)
    %w(CATEGORY MIN_ARGS MAX_ARGS 
       NAME NEED_STACK SHORT_HELP).each do |name|
      puts '%s: %s' % [name, cmd.class.const_get(name).inspect]
    end
    puts '-' * 30
    puts cmd.class.const_get('HELP')
    puts '=' * 30
  end
  module_function :show_special_class_constants

end

# To get Trepan::CmdProcessor defined and with the 
# with the correct initialize parameters.
class Trepan
  class << CmdProcessor
    def initialize(core, settings={})
      @core            = core
      @settings        = settings
    end
  end
end

if __FILE__ == $0
  dbgr = MockDebugger::MockDebugger.new
  p dbgr.settings
  puts '=' * 10
  p dbgr.core.processor.settings
end
