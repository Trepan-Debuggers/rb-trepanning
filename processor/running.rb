class Debugger
  class CmdProcessor

    # Does whatever needs to be done to set to continue program
    # execution.
    def continue
      @next_level      = 32000 # I'm guessing the stack size can't
                               # ever reach this
      @next_thread     = nil
      @core.step_count = -1    # No more event stepping
      @leave_cmd_loop  = true  # Break out of the processor command loop.
    end

    # Does whatever needs to be done to set to "next" program
    # execution.
    def next(step_count=1, opts={})
      step(step_count, opts)
      @next_level      = @top_frame.stack_size
      @next_thread     = Thread.current
    end

    # Does whatever needs to be done to set to step program
    # execution.
    def step(step_count=1, opts={})
      continue
      @core.step_count = step_count
      @different_pos   = opts[:different_pos] if 
        opts.member?(:different_pos)
      @stop_events     = opts[:stop_events]   if 
        opts.member?(:stop_events)
    end

    def parse_next_step_suffix(step_cmd)
      opts = {}
      case step_cmd[-1..-1]
      when '-'
        opts[:different_pos] = false
      when '+'
        opts[:different_pos] = true
      when '!'
        opts[:stop_events] = Set.new(%w(raise))
      when '<'
        opts[:stop_events] = Set.new(%w(c-return return))
      when '>'
        opts[:stop_events] = Set.new(%w(c-call call))
        if step_cmd.size > 1 && step_cmd[-2..-2] == '<'
          opts[:stop_events] = Set.new(%w(c-call c-return call return))
        else
          opts[:stop_events] = Set.new(%w(c-call call))
        end
      end
      return opts
    end
  end
end
