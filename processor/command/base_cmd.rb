# Base class of all commands. Code common to all commans is here.

class Debugger
  class Command
    class << self
      # name_aliases is an Array of String. The first entry is the
      # name of the command, remaining entries are aliases.
      attr_accessor :help, :short_help, :name_aliases
    end
    def run(args)
      raise RuntimeError, 'You need to define this method elsehwere'
    end
  end
end
