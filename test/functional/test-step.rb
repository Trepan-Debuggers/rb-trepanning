#!/usr/bin/env ruby
require 'test/unit'
require 'trace'
require_relative 'fn_helper'

class TestStep < Test::Unit::TestCase

  include FnTestHelper
  include Trace

  def test_step_same_level

    # See that we can step with parameter which is the same as 'step 1'
    cmds = ['step', 'continue']
    d = strarray_setup(cmds)
    d.core.step_events = TEST_STEP_EVENT_MASK

    d.start
    ########### t1 ###############
    x = 5
    y = 6
    ##############################
    d.stop
    out = ['-- x = 5',
           '-- y = 6']
    compare_output(out, d, cmds)

    # See that we can step with a computed count value
    cmds = ['step 5-3', 'continue']
    d = strarray_setup(cmds)
    d.start
    ########### t2 ###############
    x = 5
    y = 6
    z = 7
    ##############################
    d.stop # ({:remove => true})
    out = ['-- x = 5',
           '-- z = 7']
    compare_output(out, d, cmds)
    
    # Test step>
    cmds = ['step>', 'continue']
    d = strarray_setup(cmds)
    d.start
    ########### t3 ###############
    x = 5
    def foo()
    end
    y = 6
    foo
    ##############################
    d.stop  # {:remove => true})
    out = ['-- x = 5',
           '-> def foo()']
    compare_output(out, d, cmds)
    
    # Test step!
    cmds = ['step!', 'continue']
    d = strarray_setup(cmds)
    d.start()
    ########### t4 ###############
    x = 5
    begin
      y = 2
      z = 1/0
    rescue
    end
    ##############################
    d.stop # ({:remove => true})
    out = ['-- x = 5',
           '!! z = 1/0']
    compare_output(out, d, cmds)
    
    # Test "step" with sets of events. Part 1
    cmds = ['set events call raise',
            'step', 's!']
    d = strarray_setup(cmds)
    d.start()
    ########### t5 ###############
    x = 5
    def foo1
      y = 2
      raise Exception
    rescue Exception
    end
    foo1()
    z = 1
    ##############################
    d.stop # ({:remove => true})
    out = ["-- x = 5",
           "Trace events we may stop on:",
           "\tcall, raise",
           "-> def foo1",
           "!! raise Exception",
           "!! raise Exception"]

    got = filter_line_cmd(d.intf[-1].output.output)
    out.pop if got.size+1 == out.size
    compare_output(out, d, cmds)
    
    # Test "step" will sets of events. Part 2
    cmds = ['step> 1+0',
            'step! 1', 'continue']
    d = strarray_setup(cmds)
    d.start()
    ########### t6 ###############
    x = 5
    begin
      def foo2()
        y = 2
        raise Exception
      end
      foo2()
    rescue Exception
    end
    z = 1
    ##############################
    d.stop({:remove => true})
    out = ['-- x = 5',
           '-> def foo2()',
           '!! raise Exception']
    compare_output(out, d, cmds)
    
  end

  def test_step_between_fn

    # Step into and out of a function
    def sqr(x)
      y = x * x
    end
    cmds = %w(step) * 4 + %w(continue)
    out =  ['-- x = sqr(4)',
            '-> def sqr(x)',
            '-- y = x * x',
            '<- end',
            '-- y = 5']
    d = strarray_setup(cmds)
    d.start
    ########### t7 ###############
    x = sqr(4)
    y = 5
    ##############################
    d.stop # ({:remove => true})
    compare_output(out, d, cmds)

    cmds = ['set events call return',
            'step', 'step', 'continue']
    out =  ['-- x = sqr(4)',
            'Trace events we may stop on:',
            '	call, return',
            '-> def sqr(x)',
            '<- end']
    d = strarray_setup(cmds)
    d.start
    ########### t8 ###############
    x = sqr(4)
    y = 5
    ##############################
    d.stop # ({:remove => true})
    compare_output(out, d, cmds)
  end

  def test_step_in_exception
    def boom(x)
      y = 0/x
    end
    def bad(x)
      boom(x)
      y = x * x
    end
    cmds = %w(step! continue)
    d = strarray_setup(cmds)
    begin 
      d.start()
      x = bad(0)
      assert_equal(false, true, 'should have raised an exception')
    rescue ZeroDivisionError
      assert true, 'Got the exception'
    ensure
      d.stop({:remove => true})
    end
    
    out = ['-- x = bad(0)',  # line event
           '!! y = 0/x',     # exception event
           ]
    compare_output(out, d, cmds)
  end

  def test_step_event

    def fact(x)
      return 1 if x <= 1
      x = x * fact(x-1)
      return x
    end
    cmds = ['step<', '1 == x', 'continue'] 
    d = strarray_setup(cmds)
    d.start
    ########### t9 ###############
    x = fact(4)
    y = 5
    ##############################
    d.stop # ({:remove => true})
    out = ['-- x = fact(4)',
           '<- return 1 if x <= 1',
           'true']
    compare_output(out, d, cmds)
  end

  def test_step_into_fun

    # Bug was that we were stopping at a VM instruction before the fn
    # call proper ('bar' below), and not getting a line number for it.
    # So a subsequent 'step' when 'set different' in effect was to stay
    # at the same place at the function call.
    cmds = ['set different', 'set events insn call, class, line, return',
            'step', 'step', 'step', 'step', 'continue'] 
    d = strarray_setup(cmds)
    d.start
    ########### t10 ###############
    def bar
      return 1
    end

    def foo
      bar
      return 5
    end
    foo
    ##############################
    d.stop # ({:remove => true})
    out = ['-- def bar',
           "different is on.",
           'Trace events we may stop on:',
           "\tcall, class, insn, line, return",
           '.. def foo',
           '.. foo',
           '-> def foo',
           '-- bar']
    compare_output(out, d, cmds)
  end
end
