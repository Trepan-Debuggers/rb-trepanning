# -*- coding: utf-8 -*-
# Copyright (C) 2011 Rocky Bernstein <rockyb@rubyforge.net>

# Communication status codes
module Trepanning
  # Most of these go from debugged process to front-end
  # client interface. COMMAND goes the other way.
  module RemoteCommunication
    unless defined?(PRINT)
      PRINT         = '.'
      COMMAND       = 'C'
      CONFIRM_TRUE  = 'Y'
      CONFIRM_FALSE = 'N'
      CONFIRM_REPLY = '?'
      QUIT          = 'q'
      PROMPT        = 'p'
      RESTART       = 'r'
    end
  end
end
