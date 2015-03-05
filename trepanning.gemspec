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
A Ruby debugger with both high and low-level debugging support.

To provide the advanced features this version works only with a
patched MRI Ruby 1.9.3 runtime.

Versions also exist for Ruby 2.1.5 and 1.9.2.
EOF
  spec.add_dependency('rb-threadframe', '~> 0.39', '>= 0.39.9')
  spec.add_dependency('rb-trace', '~> 0.5')
  spec.add_dependency('linecache-tf', '~> 1.0')
  spec.add_dependency('columnize', '~> 0.8')
  spec.add_dependency('redcarpet', '~> 3.2')
  spec.add_development_dependency('diff-lcs', '~> 0') # For testing only
  spec.author       = 'R. Bernstein'
  spec.bindir       = 'bin'
  spec.email        = 'rockyb@rubyforge.net'
  spec.executables = ['trepan']
  spec.files        = `git ls-files`.split("\n")
  spec.has_rdoc     = true
  spec.homepage     = 'http://wiki.github.com/rocky/rb-trepanning'
  spec.name         = 'trepanning'
  spec.license      = 'GPL'
  spec.platform     = Gem::Platform::RUBY
  spec.require_path = 'lib'
  spec.summary      = 'Enhanced Ruby 1.9 Debugger'
  spec.version      = Trepan::VERSION

  # Make the readme file the start page for the generated html
  spec.rdoc_options += %w(--main README)
  spec.rdoc_options += ['--title', "Trepan #{Trepan::VERSION} Documentation"]

end
