# -*- coding: utf-8 -*-
# Things related to file/module status

# lookupmodule()->(module, file) translates a possibly incomplete
# file or module name into an absolute file name. nil can be
# returned for either of the values positions of module or file when
#  no or module or file is found.
def lookupmodule(name)
  if sys.modules.get(name)
    return sys.modules[name], sys.modules[name].__FILE__
  end
  return nil, name if os.path.isabs(name) && File.readable?(name)
  f = File.join(sys.path[0], name)
  return nil, f if File.readable?(f)
  root, ext = os.path.splitext(name)
  if ext == ''
    name += '.rb'
  end
  return nil, name if os.path.isabs(name)
  sys.path.each do |dirname| 
    while os.path.islink(dirname)
      dirname = os.readlink(dirname)
    end
    fullname = File.join(dirname, name)
    return nil, fullname if File.readable?(fullname)
  end
  return nil, nil
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
  # print "lookupmodule('os.path'): ", lookupmodule('os.path')
  # print "lookupmodule(__FILE__): ", lookupmodule(__FILE__)
  # print "lookupmodule('fafdsadsa'): ", lookupmodule('fafdsafdsa')
end
