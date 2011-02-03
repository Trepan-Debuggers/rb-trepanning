# -*- coding: utf-8 -*-
# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
# Trepan::CmdProcess that loads up debugger commands from builtin and
# user directories.
# Sets @commands, @aliases, @macros
require_relative '../app/util'
class Trepan
  class CmdProcessor

    attr_reader   :aliases         # Hash[String] of command names
                                   # indexed by alias name
    attr_reader   :commands        # Hash[String] of command objects
                                   # indexed by name
    attr_reader   :macros          # Hash[String] of Proc objects 
                                   # indexed by macro name.
    
    # "initialize" for multi-file class. Called from main.rb's "initialize".
    def load_cmds_initialize
      @commands = {}
      @aliases = {}
      @macros = {}

      cmd_dirs = [ File.join(File.dirname(__FILE__), 'command') ]
      cmd_dirs <<  @settings[:user_cmd_dir] if @settings[:user_cmd_dir]
      cmd_dirs.each do |cmd_dir| 
          load_debugger_commands(cmd_dir) if File.directory?(cmd_dir)
        end
    end

    # Loads in debugger commands by require'ing each ruby file in the
    # 'command' directory. Then a new instance of each class of the 
    # form Trepan::xxCommand is added to @commands and that array
    # is returned.
    def load_debugger_commands(file_or_dir)
      if File.directory?(file_or_dir)
        Dir.glob(File.join(file_or_dir, '*.rb')).each do |rb| 
          # We use require so that multiple calls have no effect.
          require rb
        end
      elsif File.readable?(file_or_dir)
        # We use load in case we are reloading. 
        # 'require' would not be effective here
        load file_or_dir
      else
        return false
      end
      Trepan::Command.constants.grep(/.Command$/).each do |command|
        setup_command(command)
      end
      return true
    end

    def load_debugger_command(command_file)
      return unless File.readable?(command_file)
      load command_file
      Trepan::Command.constants.grep(/.Command$/).each do |command|
        setup_command(command)
      end
    end

    # Instantiate a Trepan::Command and extract info: the NAME, ALIASES
    # and store the command in @commands.
    def setup_command(command)
      # Note: there is probably a non-eval way to instantiate the
      # command, but I don't know it. And eval works.
      klass = self.instance_eval("Trepan::Command::#{command}")
      cmd = klass.send(:new, self)
      
      # Add to list of commands and aliases.
      cmd_name = klass.const_get(:NAME)
      if klass.constants.member?(:ALIASES)
        aliases= klass.const_get(:ALIASES)  
        aliases.each {|a| @aliases[a] = cmd_name}
      end
      @commands[cmd_name] = cmd
    end

    # Looks up cmd_array[0] in @commands and runs that. We do lots of 
    # validity testing on cmd_array.
    def run_cmd(cmd_array)
      unless cmd_array.is_a?(Array)
        errmsg "run_cmd argument should be an Array, got: #{cmd_array.class}"
        return
      end
      if cmd_array.detect{|item| !item.is_a?(String)}
        errmsg "run_cmd argument Array should only contain strings. " + 
          "Got #{cmd_array.inspect}"
        return
      end
      if cmd_array.empty?
        errmsg "run_cmd Array should have at least one item. " + 
          "Got: #{cmd_array.inspect}"
        return
      end
      cmd_name = cmd_array[0]
      if @commands.member?(cmd_name)
        @commands[cmd_name].run(cmd_array)
      end
    end

    def complete(arg, replace_leading=true)
      if arg.kind_of?(String)
        args = arg.split
      elsif arg.kind_of?(Array)
        args = arg
      else
        return []
      end
      return args if args.empty?
      if args.size == 1
        cmd_matches = Trepan::Util.complete_token(@commands.keys, args[0])
        (@aliases.keys.select do |cmd|
          cmd.start_with?(args[0]) && !cmd_matches.member?(@aliases[cmd])
         end) + cmd_matches.sort
      else 
        first_ary = complete(args[0])
        return first_ary unless 1 == first_ary.size 
        first_arg = first_ary[0]
        cmd = @commands[first_arg]
        if cmd.respond_to?(:complete)
          second_ary = cmd.complete(args[1])
          if second_ary.empty?
            return [args.join(' ')] 
          end
          return second_ary.map do |second_arg|
            # FIXME: break out args[2..-1] if that exists to 
            # handle more complex completions including subsubcmds.
            if replace_leading
              "#{first_arg} #{second_arg.to_s + args[2..-1].join(' ')}"
            else
              "#{args[0]} #{second_arg.to_s + args[2..-1].join(' ')}"
            end
          end
        end
        return ["#{first_arg} #{args[1..-1].join(' ')}"]
      end
    end
  end
end
if __FILE__ == $0
  cmdproc = Trepan::CmdProcessor.new
  cmddir = File.join(File.dirname(__FILE__), 'command')
  cmdproc.instance_variable_set('@settings', {})
  cmdproc.load_cmds_initialize
  require 'columnize'
  puts Columnize.columnize(cmdproc.commands.keys.sort)
  puts '=' * 20
  puts Columnize.columnize(cmdproc.aliases.keys.sort)
  puts '=' * 20

  def cmdproc.errmsg(mess)
    puts "** #{mess}"
  end

  def cmdproc.msg(mess)
    puts mess
  end

  cmdproc.run_cmd('foo')  # Invalid - not an Array
  cmdproc.run_cmd([])     # Invalid - empty Array
  cmdproc.run_cmd(['list', 5])  # Invalid - nonstring arg
  p cmdproc.complete("d")
  p cmdproc.complete("sho d")
end
