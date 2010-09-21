# Copyright (C) 2010 Rocky Bernstein <rockyb@rubyforge.net>
class Trepan
  module ThreadHelper
    # Return the thread at position num or object_id num.
    def get_thread(num)
      Thread.list.at(num) || 
        Thread.list.detect {|t| t.object_id == num}
    end
    module_function :get_thread
  end
end

# Demo it.
if __FILE__ == $0
  include Trepan::ThreadHelper
  Object::Thread.new do 
    [2, -2, 0, 1, -1,
     Thread.main.object_id, 
     Thread.current.object_id].each do |th_num|
      puts "get_thread(#{th_num}) = #{get_thread(th_num).inspect}"
    end
  end.join
    
end
