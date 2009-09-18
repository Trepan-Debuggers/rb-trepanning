class Debugger
  class CmdProcessor

    # Does whatever needs to be done to set to continue program
    # execution.
    def continue
      @core.step_count = -1    # No more event stepping
      @leave_cmd_loop  = true  # Break out of the processor command loop.
    end

    # Does whatever needs to be done to set to step program
    # execution.
    def step(step_count, opts)
      @core.step_count = step_count
      @different_pos   = opts[:different_pos] if opts[:different_pos]
      @stop_events     = opts[:stop_events]   if opts[:stop_events]
      @leave_cmd_loop  = true  # Break out of the processor command loop.
    end
  end
end
