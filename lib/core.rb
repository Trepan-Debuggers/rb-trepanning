require 'thread_frame'
require_relative File.join(%w(.. processor cmdproc))
class Debugger
  class Core
    def initialize
      @processor = CmdProcessor.new()
    end

    def event_processor(event, frame)
      # FIXME: Block all other threads
      @event = event
      @frame = frame
      @processor.process_commands(@frame)
      # FIXME: unblock other threads
      # Remove access to @frame. 
      @frame = nil 
    end

    # A Ruby 1.8-style event processor. We don't use file, line, id, bind. 
    def old_event_processor(event, file, line, id, bind, klass)
      event_processor(event, RubyVM::ThreadFrame.current.prev)
    end
    
    def debugger
      frame = RubyVM::ThreadFrame.current.prev
      p frame.source_location, frame.source_container
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
