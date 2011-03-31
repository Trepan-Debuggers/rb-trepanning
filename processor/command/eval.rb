# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative './base/cmd'

class Trepan::Command::EvalCommand < Trepan::Command

  old_verbose = $VERBOSE  
  $VERBOSE    = nil
  NAME        = File.basename(__FILE__, '.rb')
  HELP    = <<-HELP
#{NAME} [STRING]

Run code in the context of the current frame.

The value of the expression is stored into a global variable so it
may be used again easily. The name of the global variable is printed
next to the inspect output of the value.

If no string is given, we run the string from the current source code
about to be run. If the command ends ? (via an alias) and no string is
given we will the following translations occur:

   {if|elsif|unless} expr [then]  => expr
   {until|while} expr [do]        => expr
   return expr                    => expr
   case expr                      => expr
   def fn(params)                 => [params]
   var = expr                     => expr

The above is done via regular expression. No fancy parsing is done, say,
to look to see if expr is split across a line or whether var an assigment
might have multiple variables on the left-hand side.

Examples:

#{NAME} 1+2  # 3
#{NAME} @v
#{NAME}      # Run current source-code line
#{NAME}?     # but strips off leading 'if', 'while', ..
             # from command 

See also 'set autoeval'. The command helps one predict future execution.
See 'set buffer trace' for showing what may have already been run.
      HELP

  ALIASES       = %w(eval? ev? ev)
  CATEGORY      = 'data'
  NEED_STACK    = true
  SHORT_HELP    = 'Run code in the current context'
  $VERBOSE      = old_verbose 

  def run(args)
    if args.size == 1
      text  = @proc.current_source_text
      if  '?' == args[0][-1..-1] 
        if text =~ /^\s*(?:if|elsif|unless)\s+/
          text.gsub!(/^\s*(?:if|elsif|unless)\s+/,'') 
          text.gsub!(/\s+then\s*$/, '')
        elsif text =~ /^\s*(?:until|while)\s+/
          text.gsub!(/^\s*(?:until|while)\s+/,'') 
          text.gsub!(/\s+do\s*$/, '')
        elsif text =~ /^\s*return\s+/
          text.gsub!(/^\s*return\s+/,'')
        elsif text =~ /^\s*case\s+/
          text.gsub!(/^\s*case\s*/,'')
        elsif text =~ /^\s*def\s*.*\(.+\)/
          text.gsub!(/^\s*def\s*.*\((.*)\)/,'[\1]')
        elsif text =~ /^\s*[A-Za-z_][A-Za-z0-9_\[\]]*\s*=[^=>]/
          text.gsub!(/^\s*[A-Za-z_][A-Za-z0-9_\[\]]*\s*=/,'')
        end
        msg "eval: #{text}"
      end
    else
      text = @proc.cmd_argstr
    end
    @proc.eval_code(text, settings[:maxstring])
  end
end

if __FILE__ == $0
  require_relative '../mock'
  dbgr, cmd = MockDebugger::setup
  arg_str = '1 + 2'
  cmd.proc.instance_variable_set('@cmd_argstr', arg_str)
  puts "eval #{arg_str} is: #{cmd.run([cmd.name, arg_str])}"
end
