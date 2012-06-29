# -*- coding: utf-8 -*-
# Copyright (C) 2011, 2012 Rocky Bernstein <rockyb@rubyforge.net>

# Communication status codes
module Trepanning
  # Most of these go from debugged process to front-end
  # client interface. COMMAND goes the other way.
  module RemoteCommunication
    unless defined?(PRINT)
      PRINT         = '.'   # Server wants to print
      COMMAND       = 'C'   # read a command
      CONFIRM_TRUE  = 'Y'   # Confirm read, default true
      CONFIRM_FALSE = 'N'   # Confirm read, default false
      CONFIRM_REPLY = '?'
      QUIT          = 'q'   # End debug session
      PROMPT        = 'p'   # Write prompt
      SYNC          = 's'   # Resynchronize communication
      RESTART       = 'r'   # Restart
    end
  end
end
