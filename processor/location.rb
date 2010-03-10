require_relative 'msg'
class Debugger
  class CmdProcessor
    # Get line +line_number+ from file named +filename+. Return "\n"
    # there was a problem. Leaking blanks are stripped off.
    def line_at(filename, line_number) # :nodoc:
      @reload_on_change=nil unless defined?(@reload_on_change)
      line = LineCache::getline(filename, line_number, @reload_on_change)
      return "\n" unless line
      return line.gsub(/^\s+/, '').chomp
    end

    def print_location
      text      = nil
      source_container = frame_container(@frame, false)
      ev        = if @core.event.nil? || @frame_index != 0 
                    '  ' 
                  else
                    (EVENT2ICON[@core.event] || @core.event)
                  end
      @line_no  = frame_line
      filename  = source_container[1]
      loc       = "#{canonic_file(filename)}:#{line_no}"

      if source_container[0] != 'file'
        frame = @frame
        via = loc
        while source_container[0] != 'file' and frame.prev do
          source_container = frame_container(frame, false)
          frame     = frame.prev
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
      message = "#{ev} (#{loc})"
      if text && !text.strip.empty?
        message += "\n#{text}" 
        @line_no -= 1
      end
      msg message
    end
  end
end
