# We use this as the default startup file for irb inside rbdbgr
# Down the line we will have a way for folks to add/override this
# with their own file.
IRB.conf[:PROMPT_MODE] = :SIMPLE
IRB.conf[:PROMPT][:SIMPLE] = 
    {:PROMPT_C=>"rbdbgr ?> ",
     :PROMPT_I=>"rbdbgr >> ",
     :PROMPT_N=>"rbdbgr >> ",
     :PROMPT_S=>nil,
     :RETURN=>"=> %s\n"}
puts "You are in a rbdbr session. You should have access to program scope."
puts "'step', 'n', 'cont' commands have been added."
if defined?($rbdbgr) && $rbdbgr
   puts 'You should have access to debugger state via global variable $rbdbgr'
end

