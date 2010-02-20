require_relative %w(.. app display)
class Debugger
  class CmdProcessor
    attr_reader   :displays

    def display_initialize
      @displays = DisplayMgr.new
    end

    def run_eval_display(args={})
      for line in @displays.display(@frame) do 
        msg(line)
      end
    end
  end
end
