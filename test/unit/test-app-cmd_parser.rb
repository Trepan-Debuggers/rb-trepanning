#!/usr/bin/env ruby
require 'test/unit'
require_relative '../../app/cmd_parser'

class TestAppCmdParser < Test::Unit::TestCase
  def setup
    @cp = CmdParse.new('', true)
  end

  def test_parse_filename
    [
     ['filename', 'filename'],
     ['"this is a filename"', 'this is a filename'],
     ['this\ is\ another\ filename', 'this is another filename'],
     ['C\:filename', 'C:filename']
    ].each do |name, expect|
      @cp.setup_parser(name, true)
      res = @cp._filename
      assert_equal(expect, @cp.result)
    end
  end
end
