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
      if name == 'lfp' 
        max_value + 1 - position 
      else
        position
      end
    msg("VM %s(%d) = %s" % [name, position, 
                            @proc.frame.send(name, lookup_pos).inspect])
    return position
  end

end
