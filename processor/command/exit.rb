require_relative 'base_cmd'
class Debugger

  class ExitCommand < Command
    # A little harsh, but for now let's go with this.
    def name_aliases
      %w(exit)
    end
    def run(args)
      p 'calling it quits'
      exit
    end
  end
end
puts "quit loaded"
if __FILE__ == $0
  cmd = Debugger::ExitCommand.new
  p cmd.name_aliases
  cmd.run
end
