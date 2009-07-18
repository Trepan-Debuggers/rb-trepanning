# Base class of all commands. Code common to all commans is here.

class Debugger
  class Command
    def run(args)
      raise RuntimeError, 'You need to define this method elsehwere'
    end
  end
end
