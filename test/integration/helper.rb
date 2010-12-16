require 'diff/lcs'
require 'fileutils'
require_relative '../../app/run' # for ruby_path

DEFAULT_DEBUGGER_OPTS = {
  :args        => '', 
  :dbgr        => '', 
  :outfile     => nil,
  :short_cmd   => nil,
  :short_right => nil, 
  :do_diff     => true,
}

def run_debugger(testname, ruby_file, opts={})
  opts       = DEFAULT_DEBUGGER_OPTS.merge(opts)
  srcdir     = File.dirname(__FILE__)
  datadir    = File.join(srcdir, %w(.. data))
  progdir    = File.join(srcdir, %w(.. example)) 
  
  dbgr_dir   = File.join(srcdir, %w(.. ..))
  dbgr_short = File.join(%w(bin trepan))
  dbgr_path  = File.join(dbgr_dir, dbgr_short)

  short_right = "#{opts[:short_right] || testname}.right"
  rightfile  = File.join(datadir, short_right)

  short_cmd   = "#{opts[:short_cmd] || testname}.cmd"
  cmdfile     = File.join(datadir, short_cmd)
  outfile     = opts[:outfile] ||
    File.join(srcdir,  "#{testname}.out")
  programfile = ruby_file ? File.join(progdir, ruby_file) : ''

  FileUtils.rm(outfile) if File.exist?(outfile)

  cmd = "%s %s --nx --command %s %s '%s' %s >%s" %
    [Trepanning::ruby_path, dbgr_path, cmdfile, opts[:dbgr], 
     programfile, opts[:args], outfile]
  
  system(cmd)
  return false unless 0 == $?.exitstatus 
  if opts[:do_diff]
    from_file  = rightfile
    # fromdate  = time.ctime(os.stat(fromfile).st_mtime)
    from_lines = File.open(from_file).readlines()
    to_file    = outfile
    # todate    = time.ctime(os.stat(tofile).st_mtime)
    to_lines   = File.open(to_file).readlines()
    
    # Seems to be a bug in LCS in that it will return a diff even if two
    # files are the same.
    return true if from_lines == to_lines
    
    sdiffs = Diff::LCS.sdiff(from_lines, to_lines)
    
    if sdiffs.empty?
      FileUtils.rm(outfile)
    else 
      puts cmd
      sdiffs.each do |diff|
        p diff
      end
    end
    return sdiffs.empty?
  else
    return true  # We already tested for false above
  end
end
    
if __FILE__ == $0
  run_debugger('testing', 'gcd1.rb')
end
