# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Trepan command list validation routines.  A String type is
# usually passed in as the argument to validation routines.

require 'rubygems'

require_relative './validate'

class Trepan
  class CmdProcessor < VirtualCmdProcessor

    # If last is less than first, assume last is a count rather than an
    # end line number. If last is negative, range is [first+last..first].
    def adjust_last(first, last)
      last < first ? first + last - 1 : last
    end

    def frame_filename
      
      container = frame_container(frame, false)
      
      # FIXME: put into a helper routine
      # See also duplicate code in print_location
      if container[0] != 'file'
        try_container = container
        while try_container[0] != 'file' && frame.prev do
          frame            = frame.prev
          try_container = frame_container(frame, false)
        end
        container = try_container if try_container[0] == 'file'
      end
      
      return container[1]
    end


    # Parse a list command. On success return:
    #   - the line number - a Fixnum
    #   - file name
    def parse_list_cmd(position_str, listsize, center_correction=0)
      iseq = nil
      if position_str.empty?
        filename = frame_filename
        first = [1, frame_line - center_correction].max
      else
        list_cmd_parse = parse_list(position_str,
                                    :file_exists_proc => file_exists_proc)
        return [nil] * 4 unless list_cmd_parse
        last = list_cmd_parse.num
        position = list_cmd_parse.position

        if position.is_a?(String)
          if position == '-'
            return no_frame_msg_for_list unless frame_line
            first = [1, frame_line - 2*listsize - 1].max
          elsif position == '.'
            return no_frame_msg_for_list unless frame_line
            if (second = list_cmd_parse.num)
              first = frame_line 
              last = adjust_last(first, second)
            else
              first = [1, frame_line - center_correction].max
              last = first + listsize - 1
            end
          end
          filename = frame_filename
        else
          meth_or_frame, filename, offset, offset_type = 
            parse_position(position)
          return [nil] * 4 unless filename
          if offset_type == :line
            first = offset
          elsif meth_or_frame
            if iseq = meth_or_frame.iseq
              iseq, first, vm_offset = 
                position_to_line_and_offset(iseq, filename, position, offset_type)
              unless first
              errmsg("Unable to get location in #{meth_or_frame}")
                return [nil] * 4 
              end
            end
          elsif !offset 
            first = 1
          else
            errmsg("Unable to parse list position #{position_str}")
            return [nil] * 4
          end
        end
      end
      if last
        first, last = [first + last, first] if last < 0
        last = adjust_last(first, last)
      else
        first = [1, first - center_correction].max 
        last = first + listsize - 1 unless last
      end
      LineCache::cache(filename) unless LineCache::cached?(filename)
      return [iseq, filename, first, last]
    end
    
    def no_frame_msg_for_list
      errmsg("No Ruby program loaded.")
      return nil, nil, nil, nil
    end
    
  end
end

if __FILE__ == $0
  # Demo it.
  if !(ARGV.size == 1 && ARGV[0] == 'noload')
    ARGV[0..-1]    = ['noload']
    load(__FILE__)
  else    
    require 'thread_frame'
    require_relative '../app/mock'
    require_relative './default'
    require_relative 'frame'

    # FIXME: Have to include before defining CmdProcessor!
    require_relative '../processor' 

    cmdproc = Trepan::CmdProcessor.new(Trepan::MockCore.new())
    cmdproc.frame_initialize
    cmdproc.instance_variable_set('@settings', 
                               Trepan::CmdProcessor::DEFAULT_SETTINGS)
    cmdproc.frame_setup(RubyVM::Frame.current)
    def foo; 5 end
    def cmdproc.errmsg(msg)
      puts msg
    end
    puts '-' * 20
    p cmdproc.parse_list_cmd('.', 10)
    p cmdproc.parse_list_cmd('-', 10)
    p cmdproc.parse_list_cmd('foo', 10)
    p cmdproc.parse_list_cmd('@0', 10)
    p cmdproc.parse_list_cmd("#{__LINE__}", 10)
    p cmdproc.parse_list_cmd("#{__FILE__}   @0", 10)
    p cmdproc.parse_list_cmd("#{__FILE__}:#{__LINE__}", 10)
    p cmdproc.parse_list_cmd("#{__FILE__} #{__LINE__}", 10)
    p cmdproc.parse_list_cmd("cmdproc.errmsg", 10)
    p cmdproc.parse_list_cmd("cmdproc.errmsg:@0", 10)
    p cmdproc.parse_list_cmd("cmdproc.errmsg:@0", -10)
  end
end
