# -*- coding: utf-8 -*-
# Things related to file/module status
require 'thread_frame'

module Rbdbgr
  def file_match_pat(filename)
    prefix = 
      if filename[0..0] == File::SEPARATOR
        # An absolute filename has to match at the beginning and
        # the end.
        '^'
      else
        # An nonabsolute filename has to match either at the
        # beginning of the file name or have a path separator before
        # the supplied part, e.g. "file.rb" does not match "myfile.rb"
        # but matches "my/file.rb"
        '(?:^|[/])'
      end
    "#{prefix}#{Regexp.escape(filename)}$"
  end

  def filter_scripts(dirname)
    match_block = Proc.new{|filename, iseq| filename =~ /^#{dirname}/}
    scripts = SCRIPT_ISEQS__.select &match_block
    SCRIPT_ISEQS__.delete_if &match_block
    match_block = Proc.new{|iseq| 
      iseq.source_container[1] =~ /^#{dirname}/
    }
    rejected = {}
    ISEQS__.each do |name, iseqs|
      ary = iseqs.select &match_block
      rejected[name] = ary unless ary.empty?
      iseqs.delete_if &match_block
    end
    return [scripts, rejected]
  end

  def find_scripts(filename)
    filename_pat = file_match_pat(filename)
    return SCRIPT_ISEQS__.keys.grep(/#{filename_pat}/)
  end

  def find_iseqs(iseqs_hash, name)
    iseq_name, filename = name.split(/@/)
    return [] unless iseqs_hash.member?(iseq_name)
    iseqs = iseqs_hash[iseq_name]
    # FIXME: filter out debugger iseqs
    if filename
      filename_pat = file_match_pat(filename)
      iseqs.select{|iseq| iseq.source_container[1] =~ /#{filename_pat}/}
    else
      return iseqs 
    end
  end

  # parse_position(errmsg, arg)->(fn, name, lineno)
  #    
  #  Parse arg as [filename|module:]lineno
  #  Make sure it works for C:\foo\bar.rb:12
  def parse_position(errmsg, arg)
    colon = arg.rindex(':') 
    if colon
      # FIXME: Handle double colons, e.g. File::open
      filename = arg[0..colon-1].rstrip
      m, f = lookupmodule(filename)
      if not f
        errmsg.call("'%s' not found using sys.path" % filename)
        return nil, nil, nil
      else
        filename = f
        arg = arg[colon+1..-1].lstrip
      end
      begin
        lineno = Integer(arg)
      rescue 
        errmsg.call("Bad line number: %s", arg)
        return nil, filename, nil
      end
      return nil, filename, lineno
    end
    return nil, nil, nil
  end
end
# Demo it
if __FILE__ == $0
  include Rbdbgr
  if  not (ARGV.size == 1 && ARGV[0] == 'noload')
    ISEQS__        = {}
    SCRIPT_ISEQS__ = {}
    ARGV[0..-1]    = ['noload']
    load(__FILE__)
  else    
    load 'tmpdir.rb'
    tmpdir_dir = File.dirname(find_scripts('tmpdir.rb')[0])
    p tmpdir_dir
    %w(tmpdir.rb /tmpdir.rb sometmpdir.rb).each do |filename|
      p find_scripts(filename)
    end
    p find_scripts(__FILE__)
    def tmpdir
      'to conflict with the other tmpdir'
    end
    p find_iseqs(ISEQS__, "tmpdir@#{__FILE__}")
    puts '-' * 20
    p SCRIPT_ISEQS__.keys
    puts '-' * 20
    scripts, rejected = filter_scripts(tmpdir_dir)
    p scripts.keys
    p rejected.keys
    puts '-' * 20
    p SCRIPT_ISEQS__.keys
  end
end
