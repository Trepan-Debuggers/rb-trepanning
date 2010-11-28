#!/usr/bin/env rake
# -*- Ruby -*-
require 'rubygems'

ROOT_DIR = File.dirname(__FILE__)
require File.join %W(#{ROOT_DIR} app options)

def gemspec
  @gemspec ||= eval(File.read('.gemspec'), binding, '.gemspec')
end

require 'rake/gempackagetask'
desc "Build the gem"
task :package=>:gem
task :gem=>:gemspec do
  Dir.chdir(ROOT_DIR) do
    sh "gem build .gemspec"
    FileUtils.mkdir_p 'pkg'
    FileUtils.mv "#{gemspec.name}-#{gemspec.version}.gem", 'pkg'
  end
end

desc "Install the gem locally"
task :install => :gem do
  Dir.chdir(ROOT_DIR) do
    sh %{gem install --local pkg/#{gemspec.name}-#{gemspec.version}}
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
RUBY_PATH = File.join(RbConfig::CONFIG['bindir'],  
                      RbConfig::CONFIG['RUBY_INSTALL_NAME'])

def run_standalone_ruby_files(list)
  puts '*' * 40
  list.each do |ruby_file|
    system(RUBY_PATH, ruby_file)
  end
end

def run_standalone_ruby_file(directory)
  puts ('*' * 10) + ' ' + directory + ' ' + ('*' * 10)
  Dir.chdir(directory) do
    Dir.glob('*.rb').each do |ruby_file|
      puts ('-' * 20) + ' ' + ruby_file + ' ' + ('-' * 20)
      system(RUBY_PATH, ruby_file)
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
  t.verbose = true
end

desc 'Test functional - the medium-sized tests'
Rake::TestTask.new(:'test:functional') do |t|
  t.test_files = FileList['test/functional/**/test-*.rb']
  t.verbose = true
end

desc 'Test integration - end-to-end blackbox tests'
Rake::TestTask.new(:'test:integration') do |t|
  t.test_files = FileList['test/integration/**/test-*.rb']
  t.verbose = true
end

desc 'Test everything - unit tests for now.'
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
  raise "Test failures" unless exceptions.empty?
end

desc "Run each library Ruby file in standalone mode."
task :'check:app' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} app)))
end

desc "Run each command in standalone mode."
task :'check:commands' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} processor command)))
end

desc "Run each of the sub-sub commands in standalone mode."
task :'check:subsub:commands' do
  subsub_files = FileList["#{ROOT_DIR}/processor/command/*_subcmd/*_subcmd/*.rb"]
  run_standalone_ruby_files(subsub_files)
end

desc "Run each processor Ruby file in standalone mode."
task :'check:lib' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} lib)))
end

desc "Run each processor Ruby file in standalone mode."
task :'check:processor' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} processor)))
end

desc "Run each processor Ruby file in standalone mode."
task :'check:unit' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} test unit)))
end

task :'check:functional' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} test functional)))
end

task :'check:integration' do
  run_standalone_ruby_file(File.join(%W(#{ROOT_DIR} test integration)))
end

task :check => %w(check:lib check:processor check:commands).map{|c| c.to_sym}

desc "Test everything - same as test."
task :default => :test

desc "Generate the gemspec"
task :generate do
  puts gemspec.to_ruby
end

desc "Validate the gemspec"
task :gemspec do
  gemspec.validate
end

# ---------  RDoc Documentation ------
require 'rake/rdoctask'
desc "Generate rdoc documentation"
Rake::RDocTask.new("rdoc") do |rdoc|
  rdoc.rdoc_dir = 'doc'
  rdoc.title    = "Trepanning #{Trepanning::VERSION} Documentation"

  rdoc.rdoc_files.include('lib/*.rb', 'app/*.rb', 'bin/trepan')
end
desc "Same as rdoc"
task :doc => :rdoc

desc "Remove built files"
task :clean => [:clobber_package, :clobber_rdoc]

task :clobber_package do
  FileUtils.rm_rf File.join(ROOT_DIR, 'pkg')
end

task :clobber_rdoc do
  FileUtils.rm_rf File.join(ROOT_DIR, 'doc')
end

