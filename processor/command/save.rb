# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
require 'tmpdir'
require_relative 'base/cmd'
class Trepan::Command::SaveCommand < Trepan::Command

  unless defined?(HELP)
    NAME = File.basename(__FILE__, '.rb')
    HELP = <<-HELP
#{NAME} [--[no-]erase] [filename ]

Save settings to file FILENAME. If FILENAME not given one will be made
selected.
    HELP

    CATEGORY     = 'running'
    MAX_ARGS     = 1  # Need at most this many
    SHORT_HELP  = 'Send debugger state to a file'

    DEFAULT_OPTIONS = { :erase => true, }
  end
    
  def parse_options(options, args) # :nodoc
    parser = OptionParser.new do |opts|
      opts.on("-e", "--[no-]erase", 
              "Add line to erase after reading") do
        |v| 
        options[:erase] = v
      end
    end
    parser.parse(args)
    return options
  end

  # This method runs the command
  def run(args)
    options = parse_options(DEFAULT_OPTIONS.dup, args[1..-2])
    save_filename = 
      if args.size > 1 
        args[1]
      else
        @proc.settings[:save_cmdfile] ||
          File.join(Dir.tmpdir, 
                    Dir::Tmpname.make_tmpname(['trepanning-save', '.txt'], nil))
      end
    begin
      save_file = File.open(save_filename, 'w')
    rescue => exc
      errmsg("Can't open #{save_filename} for writing.")
      errmsg("System reports: #{exc.inspect}")
      return
    end
    save_file.puts "#\n# Commands to restore trepanning environment\n#\n"
    @proc.commands.each do |cmd_name, cmd_obj|
      cmd_obj.save_command if cmd_obj.respond_to?(:save_command)
      next unless cmd_obj.is_a?(Trepan::SubcommandMgr)
      cmd_obj.subcmds.subcmds.each do |subcmd_name, subcmd_obj|
        save_file.puts subcmd_obj.save_command if 
          subcmd_obj.respond_to?(:save_command)
        next unless subcmd_obj.is_a?(Trepan::SubSubcommandMgr)
        subcmd_obj.subcmds.subcmds.each do |subsubcmd_name, subsubcmd_obj|
        save_file.puts subsubcmd_obj.save_command if 
            subsubcmd_obj.respond_to?(:save_command)
        end
      end
    end
    save_file.puts "!FileUtils.rm #{save_file.to_path.inspect}" if 
      options[:erase]
    save_file.close
    msg "Debugger commands written to file: #{save_file.to_path}"
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  require 'tmpdir'
  cmd.run([cmd.name, Dir.tmpdir])
  cmd.run([cmd.name])
  cmd.run([cmd.name, File.join(Dir.tmpdir, 'save_file.txt')])
end
