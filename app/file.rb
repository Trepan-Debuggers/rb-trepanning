# -*- coding: utf-8 -*-
# Things related to file/module status
require 'thread_frame'

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

def find_scripts(filename)
  filename_pat = file_match_pat(filename)
  return SCRIPT_ISEQS__.keys.grep(/#{filename_pat}/)
end

def find_iseqs(name)
  iseq_name, filename = name.split(/@/)
  return [] unless ISEQS__.member?(iseq_name)
  iseqs = ISEQS__[iseq_name]
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

# Demo it
if __FILE__ == $0
  if  not (ARGV.size == 1 && ARGV[0] == 'noload')
    ISEQS__        = {}
    SCRIPT_ISEQS__ = {}
    ARGV[0..-1]    = ['noload']
    load(__FILE__)
  else    
    load 'tmpdir.rb'
    %w(tmpdir.rb /tmpdir.rb sometmpdir.rb).each do |filename|
      p find_scripts(filename)
    end
    p find_scripts(__FILE__)
    def tmpdir
      'to conflict with the other tmpdir'
    end
    p find_iseqs("tmpdir@#{__FILE__}")
  end
end
