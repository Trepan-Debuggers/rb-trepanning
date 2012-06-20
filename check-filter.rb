#!/usr/bin/env ruby
# Use this to cut out the crud from make check.
# Use like this:
#   make check 2>&1  | ruby ../make-check-filter.rb
# See Makefile.am
pats = ["^(?:Loaded",
        'Started',
        "Making check in",
        'Test run options',
        "^trepan: That",
       ].join('|') + ')'
# puts pats
skip_re = /#{pats}/

while gets()
  next if $_ =~ skip_re
  puts $_
end
