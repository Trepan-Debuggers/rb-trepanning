require 'diff/lcs'

def run_debugger(testname, ruby_file, dbgr_opts='', args='', outfile=nil)

  srcdir     = File.dirname(__FILE__)
  datadir    = File.join(srcdir, %w(.. data))
  progdir    = File.join(srcdir, %w(.. example)) 
  
  dbgr_dir   = File.join(srcdir, %w(.. ..))
  dbgr_short = File.join(srcdir, %w(bin rbdbgr))
  dbgr_path  = File.join(dbgr_dir, dbgr_short)

  rightfile  = File.join(datadir, "#{testname}.right")

  cmdfile     = File.join(datadir, "#{testname}.cmd" % testname)
  outfile     = File.join(srcdir,  "#{testname}.out" % testname)
  programfile = ruby_file ? File.join(progdir, ruby_file) : ''

  outfile_opt = "--output=#{outfile}"
  
  FileUtils.rm(outfile) if File.exist?(outfile)

  cmd = "%s --command %s %s %s %s %s" %
    [dbgr_path, cmdfile, outfile_opt, dbgr_opts, programfile, args]
  
  # print cmd
  system(cmd)
  fromfile  = rightfile
  # fromdate  = time.ctime(os.stat(fromfile).st_mtime)
  fromlines = File.open(fromfile).readlines()
  tofile    = outfile
  # todate    = time.ctime(os.stat(tofile).st_mtime)
  tolines   = File.open(tofile).readlines()

  sdiffs = Diff::LCS.sdiff(lines1, lines2)

  if sdiffs.empty?
    FileUtils.rm(outfile)
  else 
    sdiffs.each do |diff|
      p diff
    end
  end
  return sdiffs.empty?
end
    
if __FILE__ == $0
  run_debugger('testing', 'gcd1.rb')
end
