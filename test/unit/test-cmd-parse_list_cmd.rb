#!/usr/bin/env ruby
require_relative 'cmd-helper'
require_relative '../../processor/command/list'

class TestCommandParseListCmd < Test::Unit::TestCase
  include UnitHelper
  def setup
    common_setup
    @cmd = @cmds['list']
  end
  def test_parse_list_cmd
    tf = RubyVM::Frame.current
    @cmdproc.frame_setup(tf)
    short_file = File.basename(__FILE__)
    listsize = 10
    line = __LINE__ - 8
    load 'tmpdir.rb'
    [['', [short_file, line, line+listsize-1]],
     ["#{__FILE__}:10", [short_file, 5, 14]],
     ["#{__FILE__} 10", [short_file, 5, 14]],
     ['tmpdir.rb', ['tmpdir.rb', 1, listsize]],
     ['tmpdir.rb 10', ['tmpdir.rb', 5, 5+listsize-1]],
     ['Columnize.columnize 15', ['columnize.rb', 10, 10+listsize -1]],
     ['Columnize.columnize 30 3', ['columnize.rb', 30, 32]],
     ['Columnize.columnize 40 50', ['columnize.rb', 40, 50]],
    ].each do |arg_str, expect|
      got = @cmdproc.parse_list_cmd(arg_str, listsize, listsize/2)[1..-1]
      got[0] = File.basename(got[0])
      assert_equal expect, got
    end
  end

end
