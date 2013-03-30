# -*- coding: utf-8 -*-
# Copyright (C) 2013 Rocky Bernstein <rockyb@rubyforge.net>

# Part of Trepan::CmdProcess to handle command completion
require_relative '../app/complete'
require_relative 'virtual'

class Trepan::CmdProcessor < Trepan::VirtualCmdProcessor

  attr_reader   :leading_str     # leading part of string. Used in
                                 # command completion

  # Handle initial completion. We draw from the commands, aliases,
  # and macros for completion. However we won't include aliases which
  # are prefixes of other commands.
  def complete(str, last_token)
    @leading_str = str
    next_blank_pos, token = Trepan::Complete.next_token(str, 0)
    return [''] if token.empty? && !last_token.empty?
    match_pairs = Trepan::Complete.complete_token_with_next(@commands,
                                                            token)
    match_hash = {}
    match_pairs.each do |pair|
      match_hash[pair[0]] = pair[1]
    end
    alias_pairs = Trepan::Complete.
      complete_token_filtered_with_next(@aliases, token, match_hash,
                                        @commands)
    match_pairs += alias_pairs

    macro_pairs = Trepan::Complete.
      complete_token_filtered_with_next(@macros, token, match_hash,
                                        @commands)
    match_pairs += macro_pairs

    if str[next_blank_pos..-1].empty?
      return match_pairs.map{|pair| pair[0]}.sort
    else
      alias_pairs.each do |pair|
        match_hash[pair[0]] = pair[1]
      end
    end
    if match_pairs.size > 1
      # FIXME: figure out what to do here.
      # Matched multiple items in the middle of the string
      # We can't handle this so do nothing.
      return []
      # return match_pairs.map do |name, cmd|
      #   ["#{name} #{args[1..-1].join(' ')}"]
      # end
    end
    # match_pairs.size == 1
    next_complete(str, next_blank_pos, match_pairs[0][1], last_token)
  end

  def next_complete(str, next_blank_pos, cmd, last_token)
    next_blank_pos, token = Trepan::Complete.next_token(str, next_blank_pos)
    return [] if token.empty? && !last_token.empty?

    if cmd.respond_to?(:complete_token_with_next)
      match_pairs = cmd.complete_token_with_next(token)
      return [] if match_pairs.empty?
      if str[next_blank_pos..-1].rstrip.empty? &&
          (token.empty? || token == last_token)
        return match_pairs.map { |completion, junk| completion }
      else
        if match_pairs.size == 1
          return next_complete(str, next_blank_pos, match_pairs[0][1],
                               last_token)
        else
          # FIXME: figure out what to do here.
          # Matched multiple items in the middle of the string
          # We can't handle this so do nothing.
          return []
        end
      end
    elsif cmd.respond_to?(:complete)
      matches = cmd.complete(token)
      return [] if matches.empty?
      if str[next_blank_pos..-1].rstrip.empty? &&
          (token.empty? || token == last_token)
        return matches
      else
        # FIXME: figure out what to do here.
        # Matched multiple items in the middle of the string
        # We can't handle this so do nothing.
        return []
      end
    else
      return []
    end
  end
end

if __FILE__ == $0

  require_relative 'load_cmds'
  class Trepan::CmdProcessor
    def initialize(core, settings={})
    end
  end


  cmdproc = Trepan::CmdProcessor.new(nil)

  def cmdproc.errmsg(mess)
    puts "** #{mess}"
  end

  def cmdproc.msg(mess)
    puts mess
  end

  cmddir = File.join(File.dirname(__FILE__), 'command')
  cmdproc.instance_variable_set('@settings', {})
  cmdproc.load_cmds_initialize
  p cmdproc.complete("d", 'd')
  p cmdproc.complete("sho d", 'd')
  p cmdproc.complete('', '')
end
