class Debugger
  class CmdProcessor

    attr_reader   :aliases         # Hash[String] of command names
                                   # indexed by alias name
    attr_reader   :commands        # Hash[String] of command objects
                                   # indexed by name
    attr_reader   :macros          # Hash[String] of Proc objects 
                                   # indexed by macro name.
    
    # Load up debugger commands from builtin and user directories
    # Sets @commands, @aliases
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
    # form Debugger::xxCommand is added to @commands and that array
    # is returned.

    def load_debugger_commands(cmd_dir)
      Dir.glob(File.join(cmd_dir, '*.rb')).each do |rb| 
        require rb
      end if File.directory?(cmd_dir)
      # Instantiate each Command class found by the above require(s).
      Debugger::Command.constants.grep(/.Command$/).each do |command|
        # Note: there is probably a non-eval way to instantiate the
        # command, but I don't know it. And eval works.
        new_cmd = "Debugger::Command::#{command}.new(self)"
        cmd = self.instance_eval(new_cmd)

        # Add to list of commands and aliases.
        cc = cmd.class
        cmd_name = cc.const_get(:NAME)
        if cc.constants.member?(:ALIASES)
          aliases= cc.const_get(:ALIASES)  
          aliases.each {|a| @aliases[a] = cmd_name}
        end
        @commands[cmd_name] = cmd
      end
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
  end
end
if __FILE__ == $0
  cmdproc = Debugger::CmdProcessor.new
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
end
