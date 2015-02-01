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
                # && @proc.frame.next.type == 'IFUNC" # ?

                # FIXME: In C frames there seems to be some vm_push_frame's
                # via perhaps vm_yield_with_cfunc along with sp adjustments.
                # I am not sure if this is under what conditions this
                # *doesn't* happen so until I can figure out the better thing
                # to do, possibly in the Ruby 1.9 interpeter, we'll handle
                # this here.  It is also conceivable to handle this in
                # thread_frame's sp handling.
                position + 3
            else
                position
            end

        val = @proc.frame.send(name, lookup_pos).inspect
        klass = @proc.frame.send(name, lookup_pos).class
        msg("VM %s(%d) = %s (%s)" % [name, position, val, klass])
        return position
    end

end
