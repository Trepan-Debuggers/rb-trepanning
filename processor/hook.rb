class Debugger
  class CmdProcessor
    # Command processor hooks.
    class Hook
      def initialize
        @list = []
      end

      def <<(name, hook, args)
        @list << [name, hook, args]
      end
      def delete(delete_name)
        @list.delete_if {|hook_name, hook, args| hook_name == delete_name}
      end

      # Run each function in `hooks' with args
      def run
        @list.each { |name, hook, args| hook.call(*args) }
      end
    end
  end
end
