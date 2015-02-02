# Copyright (C) 2010-2011, 2015 Rocky Bernstein <rockyb@rubyforge.net>
module Registers
    def register_array_index(name, arg, max_value=nil)
        if !arg or arg.empty?
            # Form is: "info xx" which means "info xx 0"
            lookup_pos = position = 0
        else
            opts = {
                :msg_on_error =>
                "The 'info registers %s' command argument must eval to an integer. Got: %s" % [name, arg],
                :min_value => -10,
                :max_value => max_value + 10
            }
            position = @proc.get_an_int(arg, opts)
            return nil unless position
        end
        ### FIXME: not sure if this is correct
        lookup_pos =
            if 'ep' == name
                max_value + 1 - position
            elsif 'sp' == name && 'CFUNC' == @proc.frame.type
                # In C frames, the "local_size" parameter is always 1.
                # I guess this is for the return parameter.
                # There is a magic number 2 that we also always have
                # to add.
                position + 3
            else
                position
            end

        val = @proc.frame.send(name, lookup_pos).inspect
        msg("VM %s(%d) = %s (%s)" % [name, position, val, val.class])
        return position
    end

end
