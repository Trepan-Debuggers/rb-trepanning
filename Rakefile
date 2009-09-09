#!/usr/bin/env rake
# -*- Ruby -*-
require 'rubygems'
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

desc 'Test units - the smaller tests'
task :'test:unit' do |t|
  Rake::TestTask.new(:'test:unit') do |t|
    t.test_files = FileList['test/unit/**/*.rb']
    # t.pattern = 'test/**/*test-*.rb' # instead of above
    t.verbose = true
  end
end

desc 'Test everything - unit tests for now.'
task :test do
  exceptions = ['test:unit'].collect do |task|
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

desc "Run each command in standalone mode."
task :'check:commands' do
  run_standalone_ruby_file(File.join(%W(#{rake_dir} processor command)))
end

desc "Run each library Ruby file in standalone mode."
task :'check:lib' do
  run_standalone_ruby_file(File.join(%W(#{rake_dir} lib)))
end

desc "Run each processor Ruby file in standalone mode."
task :'check:processor' do
  run_standalone_ruby_file(File.join(%W(#{rake_dir} processor)))
end

desc "Test everything - same as test."
task :check => :test
task :default => :test
