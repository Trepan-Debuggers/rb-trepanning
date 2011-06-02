# The class serves as the parent for Trepan::CmdProcessor which is
# quite large and spans over several files. By declaring "initialize"
# below, we have a consistent initialization routine and many of the
# others don't need to define "initialize".

# Also, simple versions of the I/O routines make it possible
# to do testing without having to bring in the whole "Interface"
# and I/O routines that trepanning (and "main.rb") use.

# Note that via this file we can change 'class Trepan' to 'module
# Trepan' or vice versa. So this takes effect on all of the places
# which subclass this. They use class Trepan::CmdProcessor <
# Trepan::VirtualCmdProcessor rather than breaking this down into to
# parts as below.
class Trepan
  class VirtualCmdProcessor
    attr_reader :settings
    def initialize(interfaces, settings={})
    end
  end
end
