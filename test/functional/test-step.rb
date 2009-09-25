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
    ##############################
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
    ##############################
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
    ##############################
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
    ##############################
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
    
#     # Test "step" with sets of events. Part 1
#     cmds = ['step call exception',
#             'step call exception', 'continue']
#     d = strarray_setup(cmds)
#     d.start()
#     ##############################
#     x = 5
#     begin
#       def foo1()
#         y = 2
#         raise Exception
#       end
#       foo1()
#     rescue
#     end
#     z = 1
#     ##############################
#     d.stop({:remove => true})
#     out = ['-- x = 5',
#            '-> def foo1():',
#            '!! raise Exception']
#     compare_output(out, d, cmds)
    
#     # Test "step" will sets of events. Part 2
#     cmds = ['step call exception 1+0',
#             'step call exception 1', 'continue']
#     d = strarray_setup(cmds)
#     d.start()
#     ##############################
#     x = 5
#     begin
#       def foo2()
#         y = 2
#         raise Exception
#       end
#       foo2()
#     rescue
#     end
#     z = 1
#     ##############################
#     d.stop({:remove => true})
#     out = ['-- x = 5',
#            '-> def foo2():',
#            '!! raise Exception']
#     compare_output(out, d, cmds)
    
#   end

#   def test_step_between_fn

#     # Step into and out of a function
#     def sqr(x)
#       return x * x
#     end
#     [
#      [['step', 'step', 'continue'],
#       ['-- x = sqr(4)',
#        '-- return x * x',
#        '-- y = 5'],
#       frozenset(['line'])],
#      [['step', 'step', 'step', 'step', 'continue'],
#       ['-- x = sqr(4)',
#        '-> def sqr(x):',
#        '-- return x * x',
#        '<- return x * x',
#        '-- y = 5'],
#       tracer.ALL_EVENTS]
#     ].each do |cmds, out, eventset|
#       d = strarray_setup(cmds)
#       d.settings['traceset'] = eventset
#       d.start()
#       ##############################
#       x = sqr(4)
#       y = 5
#       ##############################
#       d.stop({:remove => true})
#       compare_output(out, d, cmds)
#       pass
#     end

#     def test_step_in_exception
#       def boom(x)
#         y = 0/x
#       end
#       def bad(x)
#         boom(x)
#         return x * x
#       end
#       cmds = ['step', 'step', 'step', 'step', 'step', 'step',
#               'step', 'step', 'step', 'step', 'continue']
#       d = strarray_setup(cmds)
#       begin 
#         d.start()
#         x = bad(0)
#         assert_equal(false, true, 'should have raised an exception')
#       rescue ZeroDivisionError
#         self.assertTrue(true, 'Got the exception')
#       ensure
#         d.stop({:remove => true})
#       end
      
#       out = ['-- x = bad(0)',  # line event
#              '-> def bad(x):', # call event
#              '-- boom(x)',     # line event
#              '-> def boom(x):',# call event
#              '-- y = 0/x',     # line event
#              '!! y = 0/x',     # exception event
#              '<- y = 0/x',     # return event
#              '!! boom(x)',     # exception event
#              '<- boom(x)',     # return event
#              '!! x = bad(0)',  # return event
#              '-- except ZeroDivisionError:']
#       compare_output(out, d, cmds)
#       return
#     end
  end
end
