require 'diff/lcs'
require 'fileutils'

def run_debugger(testname, ruby_file, dbgr_opts='', args='', outfile=nil)

  srcdir     = File.dirname(__FILE__)
  datadir    = File.join(srcdir, %w(.. data))
  progdir    = File.join(srcdir, %w(.. example)) 
  
  dbgr_dir   = File.join(srcdir, %w(.. ..))
  dbgr_short = File.join(%w(bin trepan))
  dbgr_path  = File.join(dbgr_dir, dbgr_short)

  rightfile  = File.join(datadir, "#{testname}.right")

  cmdfile     = File.join(datadir, "#{testname}.cmd" % testname)
  outfile     = File.join(srcdir,  "#{testname}.out" % testname)
  programfile = ruby_file ? File.join(progdir, ruby_file) : ''

  FileUtils.rm(outfile) if File.exist?(outfile)

  cmd = "%s --nx --command %s %s '%s' %s >%s" %
    [dbgr_path, cmdfile, dbgr_opts, programfile, args, outfile]
  
  system(cmd)
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
end
    
if __FILE__ == $0
  run_debugger('testing', 'gcd1.rb')
end
