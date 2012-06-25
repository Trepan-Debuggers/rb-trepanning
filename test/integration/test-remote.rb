#!/usr/bin/env ruby
require 'test/unit'
require_relative 'helper'

class TestQuit < Test::Unit::TestCase
  @@NAME = File.basename(__FILE__, '.rb')[5..-1]

  def test_trepan_call
    skip('need a usable fork to run this') unless Process.respond_to?(:fork) 
    skip('Not ready for prime time yet')
    cmdfile = File.join(File.dirname(__FILE__), '../data/quit.cmd')
    child = fork
    if child
      ## FIXME: client doesn't work with cmdfile yet.
      serveropts = {:dbgr => '--client', :cmdfile => cmdfile}
      run_debugger(@@NAME, 'gcd.rb', serveropts)
      Process.wait
    else
      clientopts = {:dbgr => '--server', :nocommand => true}
      run_debugger(@@NAME, 'gcd.rb', clientopts)
    end
  end
end
