# This code comes directly from ruby-debug.
require 'irb'
module IRB # :nodoc:
  module ExtendCommand # :nodoc:
    class Continue # :nodoc:
      def self.execute(conf)
        throw :IRB_EXIT, :cont
      end
    end
    class Next # :nodoc:
      def self.execute(conf)
        throw :IRB_EXIT, :next
      end
    end
    class Step # :nodoc:
      def self.execute(conf)
        throw :IRB_EXIT, :step
      end
    end
  end

  if defined?(ExtendCommandBundle)
    ExtendCommandBundle.def_extend_command 'cont', :Continue
    ExtendCommandBundle.def_extend_command 'n', :Next
    ExtendCommandBundle.def_extend_command 'step', :Step
  end
  
  def self.start_session(binding)
    unless @__initialized

      # Set to run the standard rbdbgr IRB profile
      irbrc = File.expand_path(File.join(File.dirname(__FILE__), %w(.. irbrc)))
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

    irb = Irb.new(workspace)

    @CONF[:IRB_RC].call(irb.context) if @CONF[:IRB_RC]
    @CONF[:MAIN_CONTEXT] = irb.context

    catch(:IRB_EXIT) do
      irb.eval_input
    end
  end
end

if __FILE__ == $0
  # Demo it.
  IRB.start_session(binding) if ARGV.size > 0
end
