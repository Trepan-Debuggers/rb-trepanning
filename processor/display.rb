# Copyright (C) 2010, 2011 Rocky Bernstein <rockyb@rubyforge.net>
require_relative '../app/display'
require_relative 'virtual'
class Trepan
  class CmdProcessor < VirtualCmdProcessor
    attr_reader   :displays

    def display_initialize
      @displays = DisplayMgr.new
    end

    def display_find(num, show_errmsg = true)
      if 0 == @displays.size 
        errmsg('No display expressions set.') if show_errmsg
        return nil
      elsif num > @displays.max || num < 1
        errmsg('Display number %d is out of range 1..%d' %
               [num, @displays.max]) if show_errmsg
        return nil
      end
      disp = @displays[num]
      if disp
        return disp
      else
        errmsg('Display number %d previously deleted.' %
               num) if show_errmsg
        return nil
      end
    end

    # Enable or disable a breakpoint given its breakpoint number.
    def en_disable_display_by_number(num, do_enable=true)
      disp = display_find(num)
      return false unless disp
          
      enable_disable = do_enable ? 'en' : 'dis'
      if disp.enabled? == do_enable
        errmsg('Display %d previously %sabled.' % 
               [num, enable_disable])
        return false
      end
      disp.enabled = do_enable
      return true
    end

    def run_eval_display(args={})
      for line in @displays.display(@frame) do 
        msg(line)
      end
    end
  end
end
