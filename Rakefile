#!/usr/bin/env rake
# -*- Ruby -*-
require 'rubygems'
require 'rake/testtask'

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

desc "Test everything - same as test."
task :check => :test

