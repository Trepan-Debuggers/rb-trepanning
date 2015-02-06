require 'diff/lcs'
require 'fileutils'

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
  cmdfile     = opts[:cmdfile] || File.join(datadir, short_cmd)
  outfile     = opts[:outfile] ||
    File.join(srcdir,  "#{testname}.out")
  programfile = ruby_file ? File.join(progdir, ruby_file) : ''

  FileUtils.rm(outfile) if File.exist?(outfile)

  cmd = opts[:feed_input] ? "#{opts[:feed_input]} |" : ''
  cmd +=
    if opts[:standalone]
      "%s %s %s >%s 2>&1" %
        [RbConfig.ruby, programfile, opts[:args], outfile]
    elsif opts[:nocommand]
      "%s %s --nx --basename --no-highlight %s '%s' %s >%s 2>&1" %
        [RbConfig.ruby, dbgr_path, opts[:dbgr],
         programfile, opts[:args], outfile]
    else
      "%s %s --nx --basename --no-highlight --command %s %s '%s' %s >%s 2>&1" %
        [RbConfig.ruby, dbgr_path, cmdfile, opts[:dbgr],
         programfile, opts[:args], outfile]
    end
  puts cmd if opts[:verbose]
  system(cmd)
  return false unless 0 == $?.exitstatus
  if opts[:do_diff]
    expected_lines = File.open(rightfile).readlines()
    got_lines      = File.open(outfile).readlines()
    opts[:filter].call(got_lines, expected_lines) if opts[:filter]
    # puts "=" * 80
    # got_lines.map{|line| puts line}

    # Seems to be a bug in LCS in that it will return a diff even if two
    # files are the same.
    return true if expected_lines == got_lines

    sdiffs = Diff::LCS.sdiff(expected_lines, got_lines)

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
