# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require 'rubygems'
require 'linecache'
require 'pathname'  # For cleanpath
require_relative 'msg'
require_relative '../app/frame'
require_relative 'virtual'
class Trepan::CmdProcessor < Trepan::VirtualCmdProcessor
  include Trepan::Frame

  unless defined?(EVENT2ICON)
    # Event icons used in printing locations.
    EVENT2ICON = {
      'brkpt'          => 'xx',
      'tbrkpt'         => 'x1',
      'c-call'         => 'C>',
      'c-return'       => '<C',
      'call'           => '->',
      'send'           => '=>',
      'leave'          => '<=',
      'class'          => '::',
      'coverage'       => '[]',
      'debugger-call'  => ':o',
      'end'            => '-|',
      'line'           => '--',
      'post-mortem'    => ':/',
      'raise'          => '!!',
      'return'         => '<-',
      'switch'         => 'sw',
      'trace-var'      => '$V',
      'unknown'        => '?!',
      'vm'             => 'VM',
      'vm-insn'        => '..',
      'yield'          => '<>',
    } 
  end

  def canonic_container(container)
    [container[0], canonic_file(container[1])]
  end
  
  def canonic_file(filename, resolve=true)
    # For now we want resolved filenames 
    if @settings[:basename]
      File.basename(filename)
    elsif resolve
      filename = LineCache::map_file(filename)
      File.expand_path(Pathname.new(filename).cleanpath.to_s)
    else
      filename
    end
  end

  # Return the text to the current source line.
  # FIXME: loc_and_text should call this rather than the other
  # way around.
  def current_source_text
    opts = {:reload_on_change => @reload}
    junk1, junk2, text, found_line = 
      loc_and_text('', frame, frame.source_location[0], 
                   frame.source_container, opts)
    text
  end
  
  def resolve_file_with_dir(path_suffix)
    @settings[:directory].split(/:/).each do |dir|
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
  
  # Get line +line_number+ from file named +filename+. Return ''
  # if there was a problem. Leading blanks are stripped off.
  def line_at(filename, line_number,
              opts = {
                :reload_on_change => @settings[:reload],
                :output => @settings[:highlight]
              })
    line = LineCache::getline(filename, line_number, opts)
    
    unless line
      # Try using search directories (set with command "directory")
      if filename[0..0] != File::SEPARATOR
        try_filename = resolve_file_with_dir(filename) 
        if try_filename && 
            line = LineCache::getline(try_filename, line_number, opts)
          LineCache::remap_file(filename, try_filename)
        end
      end
    end
    line ? line.lstrip.chomp : line
  end

  def loc_and_text(loc, frame, line_no, source_container,
                   opts = {
                     :reload_on_change => @settings[:reload],
                     :output => @settings[:highlight]
                   })
    found_line = true
    ## FIXME: condition is too long.
    if source_container[0] == 'string' && frame.iseq && frame.iseq.eval_source
      file = LineCache::map_iseq(frame.iseq)
      text = LineCache::getline(frame.iseq, line_no, opts)
      loc += " remapped #{canonic_file(file)}:#{line_no}"
    elsif source_container[0] != 'file'
      via = loc
      while source_container[0] != 'file' && frame.prev do
        frame            = frame.prev
        source_container = frame_container(frame, false)
      end
      if source_container[0] == 'file'
        line_no      = frame.source_location[0]
        filename     = source_container[1]
        loc         += " via #{canonic_file(filename)}:#{line_no}"
        text         = line_at(filename, line_no, opts)
        found_line   = false
      end
    else
      container = source_container[1]
      map_file, map_line = LineCache::map_file_line(container, line_no)
      if [container, line_no] != [map_file, map_line]
        loc += " remapped #{canonic_file(map_file)}:#{map_line}"
      end
      
      text  = line_at(container, line_no, opts)
    end
    [loc, line_no, text, found_line]
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
    loc, @line_no, text, found_line = 
      loc_and_text(loc, @frame, @line_no, source_container)
    
    ip_str = @frame.iseq ? " @#{frame.pc_offset}" : ''
    msg "#{ev} (#{loc}#{ip_str})"
    
    if %w(return c-return).member?(@event)
      retval = Trepan::Frame.value_returned(@frame, @event)
      msg 'R=> %s' % retval.inspect 
    end
    
    if text && !text.strip.empty?
      msg text
      @line_no -= 1
    end
    unless found_line
      # Can't find source line, so give assembly as consolation.
      # This great idea comes from the Rubinius reference debugger.
      run_command('disassemble')
    end
  end
  
  def source_location_info(source_container, line_no, frame)
    filename  = source_container[1]
    ## FIXME: condition is too long.
    canonic_filename = 
      if 'string' == source_container[0] && frame.iseq && 
          frame.iseq.eval_source
        eval_str = frame.iseq.eval_source
        'eval "' + safe_repr(eval_str.gsub(/\n/,';'), 15) + '"'
      else
        canonic_file(filename, false)
      end
    loc = "#{canonic_filename}:#{line_no}"
    return loc
  end # source_location_info
end

if __FILE__ == $0 && caller.size == 0
  # Demo it.
  require 'thread_frame'
  require_relative 'frame'
  require_relative '../app/mock'
  class Trepan::CmdProcessor
    def errmsg(msg)
      puts msg
    end
    def print_location
      puts "#{@frame.source_container} #{frame.source_location[0]}"
    end
  end

  proc = Trepan::CmdProcessor.new(Trepan::MockCore.new())
  proc.instance_variable_set('@settings', {})
  proc.frame_initialize
  proc.frame_setup(RubyVM::ThreadFrame.current)
  proc.frame_initialize

  puts proc.canonic_file(__FILE__)
  proc.settings[:basename] = true
  puts proc.canonic_file(__FILE__)
  puts proc.current_source_text
  xx = eval <<-END
     proc.frame_initialize
     proc.frame_setup(RubyVM::ThreadFrame.current)
     puts proc.current_source_text
  END
end
