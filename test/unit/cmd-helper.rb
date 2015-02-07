require 'test/unit'
require_relative '../../app/core'
require_relative '../../app/mock'
require_relative '../../processor/frame'

module UnitHelper

  def common_setup
    @dbg      ||= Trepan::MockDebugger.new(:nx => true)
    @core     ||= Trepan::Core.new(@dbg)
    @cmdproc  ||= @core.processor = Trepan::CmdProcessor.new(@core)
    @cmds     ||= @cmdproc.commands

    def @cmdproc.msg(message, opts={})
      @msgs << message
    end
    def @cmdproc.markdown(message, opts={})
      @msgs << message
    end
    def @cmdproc.msgs
      @msgs
    end
    def @cmdproc.errmsg(message, opts={})
      @errmsgs << message
    end
    def @cmdproc.errmsgs
      @errmsgs
    end
    def @cmdproc.section(message, opts={})
      @msgs << message
    end
    reset_cmdproc_vars
  end
  module_function :common_setup

  def reset_cmdproc_vars
    @cmdproc.instance_variable_set('@msgs', [])
    @cmdproc.instance_variable_set('@errmsgs', [])
  end
  module_function :reset_cmdproc_vars
end

if __FILE__ == $0
  include UnitHelper
  common_setup
  p @cmdproc.msgs
  p @dbg
end
