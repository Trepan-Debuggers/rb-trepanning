module Registers
    def register_array_index(name, args)
    if args.size == 0
      # Form is: "info xx" which means "info xx 0"
      position = 0
    else
      position_str = args[0]
      opts = {
        :msg_on_error => 
        "The 'info registers %s' command argument must eval to an integer. Got: %s" % [name, position_str],
        # :min_value => 1,
        # :max_value => ??
      }
      position = @proc.get_an_int(position_str, opts)
      return unless position
    end
    msg("VM %s(%d) = %s" % [name, position, 
                            @proc.frame.send(name, position).inspect])
  end

end
