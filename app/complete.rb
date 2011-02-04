class Trepan
  module Complete

    module_function

    # Return an Array of String found from Array of String
    # +complete_ary+ which start out with String +prefix+.
    def complete_token(complete_ary, prefix)
      complete_ary.select { |cmd| cmd.to_s.start_with?(prefix) }.sort
    end
    
    def complete_token_with_next(complete_hash, prefix)
      result = []
      complete_hash.each do |cmd_name, cmd_obj| 
        result << [cmd_name, cmd_obj] if cmd_name.to_s.start_with?(prefix)
      end
      result.sort{|a, b| a[0] <=> b[0]}
    end
    
    # Find all starting matches in Hash +aliases+ that start with +prefix+,
    # but filter out any matches already in +expanded+.
    def complete_token_filtered(aliases, prefix, expanded)
      complete_ary = aliases.keys
      complete_ary.select { |cmd| 
        cmd.to_s.start_with?(prefix) && ! expanded.member?(aliases[cmd])}.sort
    end

    # Find all starting matches in Hash +aliases+ that start with +prefix+,
    # but filter out any matches already in +expanded+.
    def complete_token_filtered_with_next(aliases, prefix, expanded,
                                          commands)
      complete_ary = aliases.keys
      expanded_ary = expanded.keys
      result = []
      complete_ary.each do |cmd| 
        if cmd.to_s.start_with?(prefix) && 
            !expanded_ary.member?(aliases[cmd])
          result << [cmd, commands[aliases[cmd]]] 
        end
      end
      result
    end
  end
end

if __FILE__ == $0
  include Trepan::Complete
  hash = {'ab' => 1, 'aac' => 2, 'aa' => 3, 'b' => 4}
  p complete_token(hash.keys, 'a')
  p complete_token_with_next(hash, 'a')
end
