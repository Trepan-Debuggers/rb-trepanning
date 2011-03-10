# use_grammar.rb
require 'rubygems'
require_relative 'cmd_parser'

class Trepan
  module CmdParser

    # Given a KPeg parse object, return the method of that parse or raise a
    # Name error if we can't find a method. parent_class is the parent class of
    # the object we've found so far and "binding" is used if we need
    # to use eval to find the method.
    def resolve_method(m, bind, parent_class = nil)
      name = m.name
      # DEBUG p  name
      errmsg = nil
      if m.type == :constant
        begin
          if parent_class
            klass = parent_class.const_get(m.chain[0].name)
          else
            errmsg = "Constant #{m} is not a class or module"
            raise NameError, errmsg unless m.chain[0]
            klass = eval(m.chain[0].name, bind)
          end
          errmsg = "Constant #{klass} is not a class or module" unless
          raise NameError, errmsg unless
            klass.kind_of?(Class) or klass.kind_of?(Module)
          m = m.chain[1]
          if klass.instance_methods.member?(:binding)
            bind = klass.bind
          elsif klass.private_instance_methods.member?(:binding)
            bind = klass.send(:binding)
          else
            bind = nil
          end
          resolve_method(m, bind, klass)
        rescue NameError 
          errmsg ||= "Can't resolve constant #{name}"
          raise NameError, errmsg
        end
      else
        is_class = 
          begin
            m.chain && m.chain[0] && 
              Class == eval("#{m.chain[0].name}.class", bind) 
          rescue 
            false
          end
        if is_class
          # Handles stuff like:
          #    x = File
          #    x.basename
          # Above, we tested we get a class back when we evalate m.chain[0]
          # below. So it is safe to run the eval.
          klass = eval("#{m.chain[0].name}", bind)
          resolve_method(m.chain[1], klass.send(:binding), klass)
        else
          begin
            errmsg = "Can't get method for #{name.inspect}"
            if m.chain && m.chain[0]
              parent_obj = eval("#{m.chain[0].name}", bind) if !parent_class && bind
            end
            parent = parent_class || parent_obj
            meth = 
              if parent
                errmsg << "in #{parent}"
                lookup_name = m.chain && m.chain[1] ? m.chain[1].name : name
                if parent.respond_to?('instance_methods') && 
                    parent.instance_methods.member?(lookup_name.to_sym)
                  parent.instance_method(lookup_name.to_sym)
                elsif parent.respond_to?('methods')
                  parent.method(lookup_name.to_sym)
                end
              elsif m.chain && m.chain[1]
                eval("#{m.chain[0].name}.method(#{lookup_name.name.inspect})", bind)
              else
                eval("self.method(#{name.inspect})", bind)
              end
            return meth
          rescue
            raise NameError, errmsg
          end
        end
      end
    end

    # Return the method by evaluating parse_struct.
    # nil is returned if we can't parse str
    def meth_for_parse_struct(parse_struct, start_binding)
      resolve_method(parse_struct, start_binding)
    end

    # Parse str and return the method associated with that.
    # nil is returned if we can't parse str
    def meth_for_string(str, start_binding)
      @cp ? @cp.setup_parser(str) : @cp = CmdParse.new(str)
      begin 
        if @cp._class_module_chain
          # Did we match all of it?
          if @cp.result.name == str.strip
            meth_for_parse_struct(@cp.result, start_binding)
          else
            nil
          end
        else
          # FIXME: change to raise ParseError? 
          nil
        end
      rescue NameError
        return nil
      end
    end

    def parse_terminal(terminal_name, loc_str)
      @cp ? @cp.setup_parser(loc_str) : @cp = CmdParse.new(loc_str)
      @cp.send(terminal_name) ? @cp : nil
    end

    def parse_location(loc_str)
      parse = parse_terminal(:_location, loc_str)
      parse ? parse.result : nil
    end

    def parse_breakpoint(str)
      parse = parse_terminal(:_breakpoint_stmt, str)
      parse ? parse.result : nil
    end

    def parse_breakpoint_no_condition(str)
      parse = parse_terminal(:_breakpoint_stmt_no_condition, str)
      parse ? parse.result : nil
    end

    def parse_list(str)
      parse = parse_terminal(:_list_stmt, str)
      parse ? parse.result : nil
    end
  end
end

if __FILE__ == $0
  # Demo it.
  %w(a a1 $global __FILE__ Constant 0 1e10 a.b).each do |name|
    cp = CmdParse.new(name)
    if cp._identifier && cp.result.name == name
      p [cp.string, cp.result, 'succeeded']
    else
      puts "#{name} failed"
    end
  end
  
  %w(Object  A::B  A::B::C  A::B::C::D  A::B.c  A.b.c.d  A(5)
     RubyVM::InstructionSequence.new
     ).each do |name|
    cp = CmdParse.new(name)
    if cp._class_module_chain && cp.result.name == name
      p [cp.string, cp.result, 'succeeded']
    else
      puts "#{name} failed"
    end
  end

  def five; 5 end
  include Trepan::CmdParser
  p meth_for_string('Array.map', binding)
  p meth_for_string('Rubinius::VM.backtrace', binding)
  %w(five
     Array.map
     RubyVM::InstructionSequence.new
     Kernel.eval
     Kernel::eval).each do |str|
    meth = meth_for_string(str, binding)
    p meth
  end
  module Testing
    def testing; 5 end
    module_function :testing
  end
  p meth_for_string('Testing.testing', binding)  
  p meth_for_string('File.basename', binding)  
  x = File
  p meth_for_string('x.basename', binding)  
  def x.five; 5; end
  p  meth_for_string('x.five', binding)  
  p x.five

  p parse_terminal(:_line_number, '5').result
  p parse_terminal(:_vm_offset, '@5').result

  # Location stuff
  ['fn', 'fn 5', 'fn @5', '@5', '5'].each do |location|
    p parse_location(location)
  end

  # Location stuff
  ['fn if a > b', 'fn 5 unless c > d', 'fn:5 if x', '@5', '5'].each do |str|
    p parse_breakpoint(str)
  end

end

