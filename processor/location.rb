require_relative 'msg'
require_relative %w(.. app frame)
class Debugger
  class CmdProcessor
    include Frame
    # Get line +line_number+ from file named +filename+. Return "\n"
    # there was a problem. Leaking blanks are stripped off.
    def line_at(filename, line_number) # :nodoc:
      @reload_on_change=nil unless defined?(@reload_on_change)
      line = LineCache::getline(filename, line_number, @reload_on_change)
      return "\n" unless line
      return line.gsub(/^\s+/, '').chomp
    end

    def print_location
      if %w(c-call call).member?(@core.event)
        msg format_stack_call(@frame, {}) 
      elsif 'raise' == @core.event
        msg @core.hook_arg.inspect if @core.hook_arg # Exception object
      end

      text      = nil
      source_container = frame_container(@frame, false)
      ev        = if @core.event.nil? || @frame_index != 0 
                    '  ' 
                  else
                    (EVENT2ICON[@core.event] || @core.event)
                  end
      @line_no  = frame_line
      filename  = source_container[1]
      canonic_filename = 
        if (0 == filename.index('(eval')) && frame.prev &&
            (eval_str = Debugger::Frame.eval_string(frame.prev))
          'eval ' + safe_repr(eval_str, 15)
        else
          canonic_file(filename)
        end
      loc = "#{canonic_filename}:#{line_no}"

      if source_container[0] != 'file'
        frame = @frame
        via = loc
        while source_container[0] != 'file' && frame.prev do
          frame            = frame.prev
          source_container = frame_container(frame, false)
        end
        if source_container[0] == 'file'
          @line_no  = frame.source_location[0]
          filename  = source_container[1]
          loc      += " via #{canonic_file(filename)}:#{@line_no}"
          text      = line_at(filename, @line_no)
        end
      else
        container = source_container[1]
        map_file, map_line = LineCache::map_file_line(container, @line_no)
        if [container, @line_no] != [map_file, map_line]
          loc += " remapped #{canonic_file(map_file)}:#{map_line}"
        end
        
        text  = line_at(container, @line_no)
      end
      msg "#{ev} (#{loc})"
      if %w(return c-return).member?(@core.event)
        retval = Debugger::Frame.value_returned(@frame, @core.event)
        msg 'r=> %s' % retval.inspect 
      end
      
      if text && !text.strip.empty?
        msg text
        @line_no -= 1
      end
    end
  end
end
