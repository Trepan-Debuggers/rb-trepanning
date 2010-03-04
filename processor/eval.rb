class Debugger
  class CmdProcessor
    def debug_eval(str)
      begin
        b = @frame.binding if @frame 
        b ||= binding
        eval(str, b)
      rescue StandardError, ScriptError => e
        exception_dump(e, settings[:stack_trace_on_error], $!.backtrace)
      end
    end

    def debug_eval_no_errmsg(str)
      begin
        b = @frame.binding if @frame 
        b ||= binding
        eval(str, b)
      rescue StandardError, ScriptError => e
        nil
      end
    end

    def exception_dump(e, stack_trace, backtrace)
      str = "#{e.class} Exception: #{e.message}"
      if stack_trace
        str += "\n" + backtrace.map{|l| "\t#{l}"}.join("\n") rescue nil
      end
      errmsg str
      # throw :debug_error
    end
  end
end
