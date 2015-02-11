#!/usr/bin/env rake
# -*- Ruby -*-
require 'rubygems'
require 'rbconfig'
raise RuntimeError,
'This package is for MRI Ruby 2.1.5 with debugger runtime support!' unless
  defined? RubyVM::Frame and  RbConfig::CONFIG.member?('rb-threadframe')

ROOT_DIR = File.dirname(__FILE__)
GEM_PROG = ENV['GEM_PROG'] || 'gem'
Gemspec_filename='trepanning.gemspec'
require_relative './app/options'

def gemspec
  @gemspec ||= eval(File.read(Gemspec_filename), binding, Gemspec_filename)
end

require 'rubygems/package_task'
desc "Build the gem"
task :package=>:gem
task :gem=>:gemspec do
  Dir.chdir(ROOT_DIR) do
    sh "#{GEM_PROG} build #{Gemspec_filename}"
    FileUtils.mkdir_p 'pkg'
    FileUtils.mv gemspec.file_name, 'pkg'
  end
end

desc 'Install the gem locally'
task :install => :gem do
  Dir.chdir(ROOT_DIR) do
    sh %{#{GEM_PROG} install --local pkg/#{gemspec.file_name}}
  end
end

require 'rake/testtask'
desc "Test everything."
Rake::TestTask.new(:test) do |t|
  t.libs << './lib'
  t.pattern = 'test/test-*.rb'
  t.verbose = true
end
task :test => :lib

desc "same as test"
task :check => :test

require 'rbconfig'

def run_standalone_ruby_files(list, opts={})
  puts '*' * 40
  list.each do |ruby_file|
    system(RbConfig.ruby, ruby_file)
    p $?.exitstatus
    break if $?.exitstatus != 0 && !opts[:continue]
  end
end

def run_standalone_ruby_file(directory, opts={})
  puts(('*' * 10) + ' ' + directory + ' ' + ('*' * 10))
  Dir.chdir(directory) do
    Dir.glob('*.rb').each do |ruby_file|
      puts(('-' * 20) + ' ' + ruby_file + ' ' + ('-' * 20))
      system(RbConfig.ruby, ruby_file)
      break if $?.exitstatus != 0 && !opts[:continue]
    end
  end
end

desc 'Create a GNU-style ChangeLog via git2cl'
task :ChangeLog do
  system('git log --pretty --numstat --summary | git2cl > ChangeLog')
end

desc 'Test units - the smaller tests'
Rake::TestTask.new(:'test:unit') do |t|
  t.test_files = FileList['test/unit/**/test-*.rb']
  # t.pattern = 'test/**/*test-*.rb' # instead of above
  t.options = '--verbose' if $VERBOSE
end

desc 'Test functional - the medium-sized tests'
Rake::TestTask.new(:'test:functional') do |t|
  t.test_files = FileList['test/functional/**/test-*.rb']
  t.options = '--verbose' if $VERBOSE
end

desc 'Test integration - end-to-end blackbox tests'
Rake::TestTask.new(:'test:integration') do |t|
  t.test_files = FileList['test/integration/**/test-*.rb']
  t.options = '--verbose' if $VERBOSE
end

desc 'Test everything - unit, functional, and integration tests.'
task :test do
  exceptions = %w(test:unit test:functional test:integration).collect do |task|
    begin
      Rake::Task[task].invoke
      nil
    rescue => e
      e
    end
  end.compact

  exceptions.each {|e| puts e;puts e.backtrace }
  raise 'Test failures' unless exceptions.empty?
end

desc 'Run each Ruby app file in standalone mode.'
task :'check:app' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} app)))
end

desc 'Run each command in standalone mode.'
task :'check:commands' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} processor command)))
end

desc 'Run each of the sub-sub commands in standalone mode.'
task :'check:sub:commands' do
  p "#{ROOT_DIR}/processor/command/*_subcmd/*_subcmd/*.rb"
  Dir.glob("#{ROOT_DIR}/processor/command/*_subcmd").each do |sub_dir|
    run_standalone_ruby_file(sub_dir)
  end
end

desc 'Run each of the sub-sub commands in standalone mode.'
task :'check:subsub:commands' do
  subsub_files = FileList["#{ROOT_DIR}/processor/command/*_subcmd/*_subcmd/*.rb"]
  run_standalone_ruby_files(subsub_files)
end

desc 'Run each processor Ruby file in standalone mode.'
task :'check:lib' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} lib)))
end

desc 'Run each processor Ruby file in standalone mode.'
task :'check:processor' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} processor)))
end

desc 'Run each processor Ruby file in standalone mode.'
task :'check:unit' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} test unit)))
end

desc 'Run functional tests in standalone mode.'
task :'check:functional' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} test functional)))
end

desc 'Run integration tests in standalone mode.'
task :'check:functional' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} test ingtegration)))
end

desc 'Run command parser grammar.'
task :'check:cmd_parse' do
  sh "kpeg --test --debug #{File.join(ROOT_DIR, %w(app cmd_parse.kpeg))}"
end

desc 'Generate command parser.'
task :'cmd_parse' do
  require 'tmpdir'
  temp_file =
    File.join(Dir.tmpdir,
              Dir::Tmpname.make_tmpname(['cmd_parser_', '.rb'], nil))

  sh("kpeg --name CmdParse --verbose --stand-alone  " +
     "#{File.join(ROOT_DIR, %w(app cmd_parse.kpeg))} " +
     "--output #{temp_file}")
end

task :'check:integration' do
  run_standalone_ruby_files(FileList['test/integration/**/test-*.rb'])
end

task :check => %w(check:lib check:processor check:commands).map{|c| c.to_sym}

desc "Default action is same as 'test'."
task :default => :test

desc 'Generate the gemspec'
task :generate do
  puts gemspec.to_ruby
end

desc 'Validate the gemspec'
task :gemspec do
  gemspec.validate
end

# ---------  RDoc Documentation ------
require 'rdoc/task'
desc "Generate rdoc documentation"
Rake::RDocTask.new("rdoc") do |rdoc|
  rdoc.rdoc_dir = 'doc'
  rdoc.title    = "Trepanning #{Trepan::VERSION} Documentation"

  rdoc.rdoc_files.include(%w(lib/*.rb
                          app/*.rb intf/*.rb io/*.rb
                          bin/trepan
                         ))
end

desc 'Same as rdoc'
task :doc => :rdoc

task :clobber_package do
  FileUtils.rm_rf File.join(ROOT_DIR, 'pkg')
end

task :clobber_rdoc do
  FileUtils.rm_rf File.join(ROOT_DIR, 'doc')
end

desc 'Remove residue from running patch'
task :rm_patch_residue do
  FileUtils.rm_rf FileList['**/*.{rej,orig}'].to_a
end

desc 'Remove ~ backup files'
task :rm_tilde_backups do
  FileUtils.rm_rf Dir.glob('**/*~'), :verbose => true
end

desc 'Remove built files'
task :clean => [:clobber_package, :clobber_rdoc, :rm_patch_residue,
                :rm_tilde_backups]
