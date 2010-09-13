# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
module Registers
  def register_array_index(name, args, max_value=nil)
    if args.size == 0
      # Form is: "info xx" which means "info xx 0"
      lookup_pos = position = 0
    else
      position_str = args[0]
      opts = {
        :msg_on_error => 
        "The 'info registers %s' command argument must eval to an integer. Got: %s" % [name, position_str],
        # :min_value => 0,
        :max_value => max_value
      }
      position = @proc.get_an_int(position_str, opts)
      return nil unless position
    end
    lookup_pos = 
      if 'lfp' == name
        max_value + 1 - position 
      elsif 'sp' == name && 'CFUNC' == @proc.frame.type 
        # && @proc.frame.next.type == 'IFUNC" # ? 

        # FIXME: In C frames there seems to be some vm_push_frame's
        # via perhaps vm_yield_with_cfunc along with sp adjustments.
        # I am not sure if this is under what conditions this
        # *doesn't* happen so until I can figure out the better thing
        # to do, possibly in the Ruby 1.9 interpeter, we'll handle
        # this here.  It is also conceivable to handle this in
        # thread_frame's sp handling.
        position + 2
      else
        position
      end
    msg("VM %s(%d) = %s" % [name, position, 
                            @proc.frame.send(name, lookup_pos).inspect])
    return position
  end

end
