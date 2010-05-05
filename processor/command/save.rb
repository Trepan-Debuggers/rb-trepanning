require_relative 'base/cmd'
class Debugger::Command::SaveCommand < Debugger::Command

  unless defined?(HELP)
    HELP = 
"Save settings to a file"

    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    NAME         = File.basename(__FILE__, '.rb')
    SHORT_HELP  = 'Send debugger state to a file'
  end
    
  # This method runs the command
  def run(args) # :nodoc
    if args.size > 1
    end
    @proc.commands.each do |cmd_name, cmd_obj|
      cmd_obj.restore_command if cmd_obj.respond_to?(:restore_command)
      next unless cmd_obj.is_a?(Debugger::SubcommandMgr)
      cmd_obj.subcmds.subcmds.each do |subcmd_name, subcmd_obj|
        puts subcmd_obj.restore_command if 
          subcmd_obj.respond_to?(:restore_command)
        next unless subcmd_obj.is_a?(Debugger::SubSubcommandMgr)
        subcmd_obj.subcmds.subcmds.each do |subsubcmd_name, subsubcmd_obj|
        puts subsubcmd_obj.restore_command if 
            subsubcmd_obj.respond_to?(:restore_command)
        end
      end
    end
  end
end

if __FILE__ == $0
  require_relative '../mock'
  name = File.basename(__FILE__, '.rb')
  dbgr, cmd = MockDebugger::setup(name)
end
