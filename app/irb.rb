# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
# This code comes more or less from ruby-debug.
require 'irb'
module IRB # :nodoc:
  module ExtendCommand # :nodoc:
    # FIXME: should we read these out of a directory to 
    #        make this more user-customizable? 

    # A base command class that resume execution
    class DebuggerResumeCommand
      def self.execute(conf, *opts)
        name = 
          if self.name =~ /IRB::ExtendCommand::(\S+)/
            $1.downcase
          else
            'unknown'
          end
        $trepan_args = opts
        $trepan_command = 
          if $trepan_irb_statements 
            $trepan_irb_statements
          else
            ([name] + opts).join(' ')
          end

        throw :IRB_EXIT, name.to_sym
      end
    end

    class Continue < DebuggerResumeCommand ; end
    class Finish   < DebuggerResumeCommand ; end
    class Next     < DebuggerResumeCommand ; end
    class Quit     < DebuggerResumeCommand ; end
    class Step     < DebuggerResumeCommand ; end

    # Issues a comamnd to the debugger without continuing
    # execution. 
    class Dbgr
      def self.execute(conf, *opts)
        $trepan_command = 
          if opts.size == 1 && opts[0].is_a?(String)
            $trepan_args = opts[0]
          else
            opts.join(' ')
          end
        dbg_cmdproc = conf.workspace.instance_variable_get('@dbg_cmdproc')
        dbg_cmdproc.run_command($trepan_command)
      end
    end

  end
  if defined?(ExtendCommandBundle)
    # New irb Commands which are the same name as their debugger
    # counterpart
    %w(Dbgr Finish Step).each do |name|
      command = name.downcase
      sym     = name.to_sym
      ExtendCommandBundle.def_extend_command command, sym
    end
    # New irb Commands which are the slightly different from their
    # debugger counterpart
    [['cont',   :Continue],
     ['ne',     :Next],
     ['q',      :Quit]].each do |command, sym|
      ExtendCommandBundle.def_extend_command command, sym
    end
  end
  
  def self.start_session(binding, dbg_cmdproc, conf={})
    unless @__initialized

      # Set to run the standard rbdbgr IRB profile
      irbrc = File.expand_path(File.join(File.dirname(__FILE__), 
                                         %w(.. data irbrc)))
      ENV['IRBRC'] = irbrc

      args = ARGV.dup
      ARGV.replace([])
      IRB.setup(nil)
      ARGV.replace(args)
      
      # If the user has a IRB profile, run that now.
      if ENV['RBDBGR_IRB']
        ENV['IRBRC'] = ENV['RBDBGR_IRB']
        @CONF[:RC_NAME_GENERATOR]=nil
        IRB.run_config
      end

      @__initialized = true
    end
    
    workspace = WorkSpace.new(binding)
    workspace.instance_variable_set('@dbg_cmdproc', dbg_cmdproc)

    irb = Irb.new(workspace)

    @CONF[:IRB_RC].call(irb.context) if @CONF[:IRB_RC]
    @CONF[:MAIN_CONTEXT] = irb.context
    conf.each {|k, v| @CONF[k] = v}
    # A copy of this back_trace_limit is already active. How? 
    IRB.CurrentContext.back_trace_limit = @CONF[:BACK_TRACE_LIMIT]

    catch(:IRB_EXIT) do
      irb.eval_input
    end
  end
end

if __FILE__ == $0
  # Demo it.
  IRB.start_session(binding, nil) if ARGV.size > 0
end
