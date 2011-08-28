# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
# The main "driver" class for a command processor. Other parts of the 
# command class and debugger command objects are pulled in from here.

%w(default breakpoint display eventbuf eval load_cmds location frame hook msg 
   running validate).each do
  |mod_str|
  require_relative "processor/#{mod_str}"
end
require_relative 'app/brkptmgr'

class Trepan
  class CmdProcessor < VirtualCmdProcessor

    # SEE ALSO attr's in require_relative's of loop above.

    attr_reader   :cmd_argstr      # Current command args, a String.
                                   # This is current_command with the command
                                   # name removed from the beginning.
    attr_reader   :cmd_name        # command name before alias or macro resolution
    attr_reader   :cmd_queue       # queue of commands to run
    attr_reader   :core            # Trepan core object
    attr_reader   :current_command # Current command getting run, a String.
    attr_reader   :dbgr            # Trepan instance (via
                                   # Trepan::Core instance)
    attr_accessor :debug_nest      # Number of nested debugs. Used in showing
                                   # prompt.
    attr_accessor :different_pos   # Same type as settings[:different] 
                                   # this is the temporary value for the
                                   # next stop while settings is the default
                                   # value to use.
    attr_accessor :event           # Stop event. Same as @core.event
    attr_reader   :intf            # Current interface
                                   # Trepan::Core instance)
    attr_reader   :interfaces      # Array of all interfaces
    attr_accessor :leave_cmd_loop  # Commands set this to signal to leave
                                   # the command loop (which often continues to 
                                   # run the debugged program). 
    attr_accessor :line_no         # Last line shown in "list" command
    attr_accessor :next_level      # Fixnum. frame.stack_size has to
                                   # be <= than this.  If next'ing,
                                   # this will be > 0.
    attr_accessor :next_thread     # Thread. If non-nil then in
                                   # stepping the thread has to be
                                   # this thread.
    attr_accessor :pass_exception  # Pass an exception back 
    attr_accessor :prompt          # String print before requesting input
    attr_reader   :settings        # Hash[:symbol] of command
                                   # processor settings
    
    attr_accessor :traced_vars     # list of traced global variables

    # The following are used in to force stopping at a different line
    # number. FIXME: could generalize to a position object.
    attr_accessor :last_pos       # Last position. 6-Tuple: of
                                  # [location, container, stack_size, 
                                  #  current_thread, pc_offset]


    def initialize(core, settings={})
      @cmd_queue       = []
      @core            = core
      @debug_nest      = 1
      @dbgr            = core.dbgr
      @hidelevels      = {}
      @interfaces      = @dbgr.intf
      @last_command    = nil
      @last_pos        = [nil, nil, nil, nil, nil, nil]
      @next_level      = 32000
      @next_thread     = nil

      start_cmds       = settings.delete(:start_cmds)
      start_file       = settings.delete(:start_file)

      @settings        = DEFAULT_SETTINGS.merge(settings)
      @traced_vars     = {}
      @different_pos   = @settings[:different]

      # FIXME: Rework using a general "set substitute file" command and
      # a global default profile which gets read.
      file = File.expand_path(File.join(File.dirname(__FILE__), 
                                        %w(.. data prelude.rb)))
      LineCache::cache(file)
      LineCache::remap_file('<internal:prelude>', file)
      file = File.expand_path(File.join(File.dirname(__FILE__), 
                                        %w(.. data custom_require.rb)))
      LineCache::cache(file)
      LineCache::remap_file('<internal:lib/rubygems/custom_require>', 
                            file)

      # Start with empty thread and frame info.
      frame_teardown 

      # Run initialization routines for each of the "submodule"s.
      # load_cmds has to come first.
      %w(load_cmds breakpoint display eventbuf frame running validate
         ).each do |submod|
        self.send("#{submod}_initialize")
      end
      hook_initialize(commands)

      # FIXME: run start file and start commands.
    end

    def compute_prompt
      thread_str = 
        if 1 == Thread.list.size
          ''
        elsif Thread.current == Thread.main
          '@main'
        else
          "@#{Thread.current.object_id}"
        end
      "%s#{settings[:prompt]}%s%s: " % 
        ['(' * @debug_nest, thread_str, ')' * @debug_nest]
      
    end

    def finalize
      breakpoint_finalize
    end

    # Check that we meed the criteria that cmd specifies it needs
    def ok_for_running(cmd, name, nargs)
      # TODO check execution_set against execution status.
      # Check we have frame is not null
      min_args = cmd.class.const_get(:MIN_ARGS)
      if nargs < min_args
        errmsg(("Command '%s' needs at least %d argument(s); " + 
                "got %d.") % [name, min_args, nargs])
        return false
      end
      max_args = cmd.class.const_get(:MAX_ARGS)
      if max_args and nargs > max_args
        errmsg(("Command '%s' needs at most %d argument(s); " + 
                "got %d.") % [name, max_args, nargs])
        return false
      end
      # if cmd.class.const_get(:NEED_RUNNING) && !...
      #   errmsg "Command '%s' requires a running program." % name
      #   return false
      # end

      if cmd.class.const_get(:NEED_STACK) && !@frame
        errmsg "Command '%s' requires a running stack frame." % name
        return false
      end

      return true
    end

    # Run one debugger command. True is returned if we want to quit.
    def process_command_and_quit?()
      intf_size = @dbgr.intf.size
      @intf  = @dbgr.intf[-1]
      return true if @intf.input_eof? && intf_size == 1
      while intf_size > 1 || !@intf.input_eof?
        begin
          @current_command = 
            if @cmd_queue.empty?
              # Leave trailing blanks on for the "complete" command
              read_command.chomp 
            else
              @cmd_queue.shift
            end
          if @current_command.empty? 
            next unless @last_command && intf.interactive?;
          end
          next if @current_command[0..0] == '#' # Skip comment lines
          break
        rescue IOError, Errno::EPIPE => e
          if intf_size > 1
            @dbgr.intf.pop
            intf_size = @dbgr.intf.size
            @intf = @dbgr.intf[-1]
            @last_command = nil
            print_location
          else
            ## FIXME: think of something better.
            quit('quit!')
            return true
          end
        end
      end
      run_command(@current_command)

      # Save it to the history.
      @intf.history_io.puts @last_command if @last_command && @intf.history_io
    end

    # This is the main entry point.
    def process_commands(frame)

      frame_setup(frame)
      @event = @core.event

      @unconditional_prehooks.run
      
      if 'trace-var' == @event 
        variable_name, value = @core.hook_arg
        action = @traced_vars[variable_name]
        msg "trace: #{variable_name} = #{value}"
        case action
        when nil
          errmsg "No action recorded for variable. Using 'stop'."
        when 'stop'
          msg "Note: we are stopped *after* the above location."
        when 'nostop'
          print_location
          return
        else
          errmsg "Internal error: unknown trace_var action #{action}"
        end
      end

      if breakpoint?
        @last_pos = [@frame.source_container, frame_line,
                     @stack_size, @current_thread, @event, 
                     @frame.pc_offset]
      else
        return if stepping_skip? || @stack_size <= @hide_level
      end

      @prompt = compute_prompt

      @leave_cmd_loop = false

      print_location unless @settings[:traceprint]
      @eventbuf.add_mark if @settings[:tracebuffer]
 
      @cmdloop_prehooks.run
      while not @leave_cmd_loop do
        begin
          break if process_command_and_quit?()
        rescue SystemExit
          @dbgr.stop
          raise
        rescue Exception => exc
          # If we are inside the script interface errmsg may fail.
          begin
            errmsg("Internal debugger error: #{exc.inspect}")
          rescue IOError
            $stderr.puts "Internal debugger error: #{exc.inspect}"
          end
          exception_dump(exc, @settings[:debugexcept], $!.backtrace)
        end
      end
      @cmdloop_posthooks.run
    end

    # Run current_command, a String. @last_command is set after the
    # command is run if it is a command.
    def run_command(current_command)
      eval_command = 
        if current_command[0..0] == '!'
          current_command[0] = ''
        else
          false
        end

      unless eval_command
        commands = current_command.split(';;')
        if commands.size > 1
          current_command = commands.shift
          @cmd_queue.unshift(*commands)
        end
        args = current_command.split
        # Expand macros. FIXME: put in a procedure
        while true do
          macro_cmd_name = args[0]
          return false if args.size == 0
          break unless @macros.member?(macro_cmd_name)
          current_command = @macros[macro_cmd_name][0].call(*args[1..-1])
          msg current_command.inspect if settings[:debugmacro]
          if current_command.is_a?(Array) && 
              current_command.all? {|val| val.is_a?(String)}
            args = (first=current_command.shift).split
            @cmd_queue += current_command
            current_command = first
          elsif current_command.is_a?(String)
            args = current_command.split
          else
            errmsg("macro #{macro_cmd_name} should return an Array " +
                   "of Strings or a String. Got #{current_command.inspect}")
            return false
          end
        end

        @cmd_name = args[0]
        run_cmd_name = 
          if @aliases.member?(@cmd_name)
            @aliases[@cmd_name] 
          else
            @cmd_name
          end
        
        run_cmd_name = uniq_abbrev(@commands.keys, run_cmd_name) if
          !@commands.member?(run_cmd_name) && @settings[:abbrev]
          
        if @commands.member?(run_cmd_name)
          cmd = @commands[run_cmd_name]
          if ok_for_running(cmd, run_cmd_name, args.size-1)
            @cmd_argstr = current_command[@cmd_name.size..-1].lstrip
            cmd.run(args) 
            @last_command = current_command
          end
          return false
        end
      end

      # Eval anything that's not a command or has been
      # requested to be eval'd
      if settings[:autoeval] || eval_command
        # eval_code(current_command, @settings[:maxstring])
        begin
          msg 'D=> ' + debug_eval_with_exception(current_command).inspect
          return false
        rescue NameError
        end
      end
      undefined_command(cmd_name)
      return false
    end

    # Error message when a command doesn't exist
    def undefined_command(cmd_name)
      begin 
        errmsg('Undefined command: "%s". Try "help".' % cmd_name)
      rescue
        $stderr.puts 'Undefined command: "%s". Try "help".' % cmd_name
      end
    end

    # FIXME: Allow access to both Trepan::CmdProcessor and Trepan
    # for index [] and []=.
    # If there is a Trepan::CmdProcessor setting that would take precidence.
    # def settings
    #   @settings.merge(@dbgr.settings) # wrong because this doesn't allow []=
    # end
  end
end

if __FILE__ == $0
  $0 = 'foo' # So we don't get here again
  require_relative 'lib/trepanning'
  dbg =  Trepan.new(:nx => true)
  dbg.core.processor.msg('I am main')
  cmdproc = dbg.core.processor
  cmdproc.errmsg('Whoa!')
  cmds = cmdproc.commands
  p cmdproc.aliases
  p cmdproc.commands.keys.sort
  cmd_name, cmd_obj = cmds.first
  puts cmd_obj.class.const_get(:HELP)
  puts cmd_obj.class.const_get(:SHORT_HELP)

  puts cmdproc.compute_prompt
  Thread.new{ puts cmdproc.compute_prompt }.join

  th = Thread.new{ Thread.pass; x = 1 }
  puts cmdproc.compute_prompt
  th.join
  cmdproc.debug_nest += 1
  puts cmdproc.compute_prompt


  if ARGV.size > 0
    dbg.core.processor.msg('Enter "q" to quit')
    dbg.proc_process_commands
  else
    $input = []
    class << dbg.core.processor
      def read_command
        $input.shift
      end
    end
    $input = ['1+2']
    dbg.core.processor.process_command_and_quit?
    $input = ['!s = 5']  # ! means eval line 
    dbg.core.processor.process_command_and_quit?
  end
end
