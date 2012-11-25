# Copyright (C) 2010-2012 Rocky Bernstein <rockyb@rubyforge.net>
require 'rubygems'
require_relative 'virtual'
class Trepan
  class CmdProcessor < VirtualCmdProcessor

    def debug_eval(str, max_fake_filename=15, 
                   ruby_193_hack=false) #FIXME: remove ruby_193_hack
      begin
        debug_eval_with_exception(str, max_fake_filename, ruby_193_hack)
      rescue SyntaxError, StandardError, ScriptError => e
        exception_dump(e, @settings[:stack_trace_on_error], $!.backtrace)
        nil
      end
    end

    def debug_eval_with_exception(str, max_fake_filename=15, 
                                  ruby_193_hack=false) # FIXME: remove ruby_193_hack
      RubyVM::Frame.current.trace_off = true if ruby_193_hack
      filename, b = get_binding_and_filename(str, max_fake_filename)
      RubyVM::Frame.current.trace_off = false if ruby_193_hack
      eval(str, b, filename)
    end

    def debug_eval_no_errmsg(str, max_fake_filename=15)
      begin
        debug_eval_with_exception(str, max_fake_filename)
      rescue SyntaxError, StandardError, ScriptError => e
        nil
      end
    end

    def eval_code(str, max_fake_filename)
      obj = debug_eval(str, max_fake_filename)
      
      # idx = @user_variables
      # @user_variables += 1
      
      # str = "$d#{idx}"
      # Rubinius::Globals[str.to_sym] = obj
      # msg "#{str} = #{obj.inspect}"
      msg "#{obj.inspect}"
      obj
    end

    def exception_dump(e, stack_trace, backtrace)
      str = "#{e.class} Exception:\n\t#{e.message}"
      if stack_trace
        str += "\n" + backtrace.map{|l| "\t#{l}"}.join("\n") rescue nil
      end
      msg str
      # throw :debug_error
    end

    def fake_eval_filename(str, maxlen = 15)
      fake_filename = 
        if maxlen < str.size
          # FIXME: Guard against \" in positions 13..15?
          str.inspect[0..maxlen-1] + '"...'
        else
          str.inspect
        end
      "(eval #{fake_filename})"
    end
    
    def get_binding_and_filename(str, maxlen)
      b = 
        begin
          @frame.binding
        rescue
          binding
        end
      filename = fake_eval_filename(str, maxlen)
      return [filename, b]
    end

  end
end

if __FILE__ == $0
  # Demo it.
  cmdp = Trepan::CmdProcessor.new([])
  puts cmdp.fake_eval_filename('x = 1; y = 2')
  puts cmdp.fake_eval_filename('x = 1; y = 2', 7)

  def cmdp.errmsg(msg)
    puts "** #{msg}"
  end
  def cmdp.msg(msg)
    puts "** #{msg}"
  end
  begin 
    1/0
  rescue Exception => exc
    cmdp.exception_dump(exc, true, $!.backtrace)
    puts '=' * 40
  end

  x = 10
  require 'thread_frame'
  cmdp.instance_variable_set('@frame', RubyVM::Frame.current)
  cmdp.instance_variable_set('@settings', {:stack_trace_on_error => true})
  def cmdp.msg(mess) ; puts mess end
  puts cmdp.debug_eval('x = "#{x}"')
  puts '=' * 40
  puts cmdp.debug_eval('x+')
  puts cmdp.debug_eval_no_errmsg('y+')
  puts '=' * 40
  puts cmdp.debug_eval('y = 1; x+', 4)
  puts '=' * 40
end
