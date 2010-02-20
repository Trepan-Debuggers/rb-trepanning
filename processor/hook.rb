class Debugger
  class CmdProcessor
    # Command processor hooks.
    attr_reader :autoirb_hook
    attr_reader :autolist_hook
    attr_reader :display_hook
    attr_reader :trace_hook
    attr_reader :unconditional_prehooks
    attr_reader :cmdloop_prehooks

    class Hook
      attr_accessor :list

      def initialize(list=[])
        @list = list
      end

      def delete_by_name(delete_name)
        @list.delete_if {|hook_name, priority, hook| hook_name == delete_name}
      end

      def empty?
        @list.empty?
      end

      def insert(priority, name, hook)
        insert_loc = @list.size  # at end
        @list.each_with_index do |n, p, h, index|
          if priority > p 
            insert_loc = index
            break
          end
        end
        @list.insert(insert_loc, [name, priority, hook])
      end

      def insert_if_new(priority, name, hook)
        insert(priority, name, hook) unless
          @list.find {|try_name, priority, hook| try_name == name}
      end

      # Run each function in `hooks' with args
      def run(*args)
        @list.each do |name, priority, hook| 
          hook.call(name, *args) 
        end
      end

      # Could add delete_at and delete if necessary.
    end
    
    def hook_initialize(commands)
      @cmdloop_prehooks = Hook.new
      @unconditional_prehooks = Hook.new

      irb_cmd = commands['irb']
      @autoirb_hook = ['autoirb', 
                       Proc.new{|*args| irb_cmd.run(['irb']) if irb_cmd}]

      display_cmd = commands['display']
      @display_hook = ['display', 
                       Proc.new{|*args| display_cmd.run(['display']) if display_cmd}]

      list_cmd = commands['list']
      @autolist_hook = ['autolist', 
                       Proc.new{|*args| list_cmd.run(['list']) if list_cmd}]

      @trace_hook   = ['trace', 
                       Proc.new{|*args| print_location}]
    end

  end
end
if __FILE__ == $0
  # Demo it.
  hooks = Debugger::CmdProcessor::Hook.new
  hooks.run(5)
  hook1 = Proc.new {|name, a| puts "#{name} called with #{a}"}
  hooks = Debugger::CmdProcessor::Hook.new()
  hooks.insert(-1, 'hook1', hook1)
  p hooks.list
  hooks.insert_if_new(-1, 'hook1', hook1)
  puts '-' * 30
  p hooks.list
  hooks.run(10)
  puts '-' * 30
  hooks.insert(-1, 'hook2', hook1)
  hooks.run(20)
  puts '-' * 30
  hooks.delete_by_name('hook2')
  hooks.run(30)
end
