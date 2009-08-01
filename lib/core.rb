require 'thread_frame'
require_relative File.join(%w(.. processor cmdproc))
class Debugger
  # This class contains the Debugger core routines, such as an event
  # processor is responsible of handling what to do when an event is
  # triggered.
  # 
  # See also 'rdbgr' the top-level Debugger class and command-line routine
  # which ultimately will call this.
  class Core
    attr_accessor :frame, :threads2frames
    def initialize
      @processor = CmdProcessor.new(self)
    end

    # A trace-hook processor with the interface a trace hook should have.
    def event_processor(event, frame, arg=nil)
      # FIXME: Block all other threads
      @arg   = arg
      @event = event
      @frame = frame

      @processor.process_commands(@frame)

      # FIXME: unblock other threads
    end

    # A Ruby 1.8-style event processor. We don't use file, line, id, bind. 
    def old_event_processor(event, file, line, id, bind, klass)
      event_processor(event, RubyVM::ThreadFrame.current.prev)
    end

    # Call this from inside the program you want to get a synchronous
    # call to the debugger.
    def debugger
      frame = RubyVM::ThreadFrame.current.prev
      event_processor('debugger-call', frame)
    end
    
  end
end
if __FILE__ == $0
  dc = Debugger::Core.new()
  if ARGV.size > 0
    def foo
      p 'foo here'
    end
    dc.debugger
  end
end
