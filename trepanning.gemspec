# -*- Ruby -*-
# -*- encoding: utf-8 -*-
require 'rake'
require 'rubygems' unless
  Object.const_defined?(:Gem)
require File.dirname(__FILE__) + "/app/options" unless
  Object.const_defined?(:'Trepan')

Gem::Specification.new do |spec|
  spec.authors      = ['R. Bernstein']
  spec.date         = Time.now
  spec.description = <<-EOF
== Description

A gdb-like Ruby debugger with both high and low-level debugging support.

See https://github.com/rocky/rb-trepanning/wiki/Features for a list of features.

To provide the advanced features this version works only with a
patched MRI Ruby 1.9.3. See http://ruby-debugger-runtime.sourceforge.net/

This version works only with a patched version of Ruby 1.9.3.
For Ruby 2.1.5, look for a version that starts with 2.15
EOF
  spec.add_dependency('rb-trace', '~> 0.5')
  spec.add_dependency('rb-threadframe', '~> 0.41')
  spec.add_dependency('columnize', '~> 0.9')
  spec.add_dependency('redcarpet', '~> 3.2')
  spec.add_dependency('coderay', '~> 1.1')
  spec.add_dependency('term-ansicolor', '~> 1.3')
  spec.add_development_dependency('diff-lcs', '~> 0') # For testing only
  spec.author       = 'R. Bernstein'
  spec.bindir       = 'bin'
  spec.email        = 'rockyb@rubyforge.net'
  spec.executables = ['trepan']
  spec.files        = `git ls-files`.split("\n")
  spec.has_rdoc     = true
  spec.homepage     = 'http://wiki.github.com/rocky/rb-trepanning'
  spec.name         = 'trepanning'
  spec.license      = 'MIT'
  spec.platform     = Gem::Platform::RUBY
  spec.required_ruby_version = '~> 1.9.3'
  spec.require_path = 'lib'
  spec.summary      = 'Enhanced Ruby 1.9.3 Debugger'
  spec.version      = Trepan::VERSION

  # Make the readme file the start page for the generated html
  spec.rdoc_options += %w(--main README)
  spec.rdoc_options += ['--title', "Trepan #{Trepan::VERSION} Documentation"]

end
