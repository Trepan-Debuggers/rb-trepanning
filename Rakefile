#!/usr/bin/env rake
# -*- Ruby -*-
require 'rubygems'
require 'rake/gempackagetask'
require 'rake/rdoctask'
require 'rake/testtask'

rake_dir = File.dirname(__FILE__)

require 'rbconfig'
RUBY_PATH = File.join(Config::CONFIG['bindir'],  
                      Config::CONFIG['RUBY_INSTALL_NAME'])

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
task :'test:unit' do |t|
  Rake::TestTask.new(:'test:unit') do |t|
    t.test_files = FileList['test/unit/**/*.rb']
    # t.pattern = 'test/**/*test-*.rb' # instead of above
    t.verbose = true
  end
end

desc 'Test functional - the medium-sized tests'
task :'test:functional' do |t|
  Rake::TestTask.new(:'test:functional') do |t|
    t.test_files = FileList['test/functional/**/test-*.rb']
    t.verbose = true
  end
end

desc 'Test everything - unit tests for now.'
task :test do
  exceptions = %w(test:unit test:functional).collect do |task|
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
  run_standalone_ruby_file(File.join(%W(#{rake_dir} app)))
end

desc "Run each command in standalone mode."
task :'check:commands' do
  run_standalone_ruby_file(File.join(%W(#{rake_dir} processor command)))
end

desc "Run each processor Ruby file in standalone mode."
task :'check:processor' do
  run_standalone_ruby_file(File.join(%W(#{rake_dir} processor)))
end

desc "Run each processor Ruby file in standalone mode."
task :'check:unit' do
  run_standalone_ruby_file(File.join(%W(#{rake_dir} test unit)))
end

task :'check:functional' do
  run_standalone_ruby_file(File.join(%W(#{rake_dir} test functional)))
end

task :check => %w(check:lib check:processor check:commands).map{|c| c.to_sym}

desc "Test everything - same as test."
task :default => :test

FILES = FileList[
  'README.textile',
  'Rakefile',
  'app/*',
  'bin/*',
  'data/*',
  'interface/*',
  'io/*',
  'lib/*',
  'processor/**/*.rb',
  # 'test/**/*.rb',
]                        

spec = Gem::Specification.new do |spec|
  spec.name = 'rbdbgr'
  spec.homepage = 'http://wiki.github.com/rocky/rbdbgr'
  spec.summary = 'Modular Ruby 1.9 Debugger'

  spec.description = "A modular, testable, Ruby debugger using some of the best ideas from ruby-debug, other debuggers and Ruby Rails. 
Some of the core debugger concepts have been rethought. As a result perhaps there are some of this may be experimental."

  spec.version = '0.0.1'
  spec.author = "R. Bernstein"
  spec.email = "rockyb@rubyforge.org"
  spec.platform = Gem::Platform::RUBY
  spec.bindir = "bin"
  spec.executables = ["rbdbgr"]
  spec.files = FILES.to_a

  spec.date = Time.now
  spec.add_dependency('rb-threadframe', '>= 0.2')
  spec.add_dependency('rb-trace', '>= 0.1')
  spec.add_dependency('linecache')
  
  spec.has_rdoc = true
  spec.extra_rdoc_files = ['README.textile']
end

# Rake task to build the default package
Rake::GemPackageTask.new(spec) do |pkg|
  pkg.need_tar = true
end
