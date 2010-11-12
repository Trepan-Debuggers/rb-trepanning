# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require 'linecache'
require_relative 'msg'
require_relative '../app/frame'
class Trepan
  class CmdProcessor
    attr_accessor :reload_on_change
    include Frame

    def location_initialize
      @reload_on_change = nil
    end

    def resolve_file_with_dir(path_suffix)
      settings[:directory].split(/:/).each do |dir|
        dir = 
          if '$cwd' == dir
            Dir.pwd
          elsif '$cdir' == dir
            RubyVM::OS_STARTUP_DIR
          else
            dir
          end
        next unless dir && File.directory?(dir)
        try_file = File.join(dir, path_suffix)
        return try_file if File.readable?(try_file)
      end
      nil
    end

    # Get line +line_number+ from file named +filename+. Return "\n"
    # there was a problem. Leading blanks are stripped off.
    def line_at(filename, line_number) # :nodoc:
      line = LineCache::getline(filename, line_number, @reload_on_change)
      unless line
        # Try using search directories (set with command "directory")
        if filename[0..0] != File::SEPARATOR
          try_filename = resolve_file_with_dir(filename) 
          if try_filename && 
              line = LineCache::getline(try_filename, line_number, 
                                        @reload_on_change)
            LineCache::remap_file(filename, try_filename)
          end
        end
      end
      return "\n" unless line
      return line.lstrip.chomp
    end

    def loc_and_text(loc, frame, line_no, source_container)
      if source_container[0] != 'file'
        via = loc
        while source_container[0] != 'file' && frame.prev do
          frame            = frame.prev
          source_container = frame_container(frame, false)
        end
        if source_container[0] == 'file'
          line_no  = frame.source_location[0]
          filename  = source_container[1]
          loc      += " via #{canonic_file(filename)}:#{line_no}"
          text      = line_at(filename, line_no)
        end
      else
        container = source_container[1]
        map_file, map_line = LineCache::map_file_line(container, line_no)
        if [container, line_no] != [map_file, map_line]
          loc += " remapped #{canonic_file(map_file)}:#{map_line}"
        end
        
        text  = line_at(container, line_no)
      end
      [loc, line_no, text]
    end

    def print_location
      if %w(c-call call).member?(@event)
        # FIXME: Fix Ruby so we don't need this workaround? 
        # See also where.rb
        opts = {}
        opts[:class] = @core.hook_arg if 
          'CFUNC' == @frame.type && @core.hook_arg && 0 == @frame_index 
        msg format_stack_call(@frame, opts) 
      elsif 'raise' == @event
        msg @core.hook_arg.inspect if @core.hook_arg # Exception object
      end

      text      = nil
      source_container = frame_container(@frame, false)
      ev        = if @event.nil? || 0 != @frame_index
                    '  ' 
                  else
                    (EVENT2ICON[@event] || @event)
                  end
      @line_no  = frame_line

      loc = source_location_info(source_container, @line_no, @frame)
      loc, @line_no, text = loc_and_text(loc, @frame, @line_no, 
                                         source_container)
      msg "#{ev} (#{loc})"

      if %w(return c-return).member?(@event)
        retval = Trepan::Frame.value_returned(@frame, @event)
        msg 'R=> %s' % retval.inspect 
      end
      
      if text && !text.strip.empty?
        msg text
        @line_no -= 1
      end
    end

    def source_location_info(source_container, line_no, frame)
      filename  = source_container[1]
      canonic_filename = 
        if (0 == filename.index('(eval')) && frame.prev &&
            (eval_str = Trepan::Frame.eval_string(frame.prev))
          'eval ' + safe_repr(eval_str, 15)
        else
          canonic_file(filename)
        end
      loc = "#{canonic_filename}:#{line_no}"
      return loc
    end # source_location_info

  end
end
