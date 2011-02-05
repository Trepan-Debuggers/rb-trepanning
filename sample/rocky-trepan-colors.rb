# Terminal color settings I use. I doubt terminal colors are
# standardized let alone what colors you want for the various
# syntax elements. So you should adjust as desired.

# Values are either a number string, e.g. '36' or a pair of number
# semicolon and number, e.g. '3;36'.
#
# The first number before a semicolon is a font modifier from the
# following table:
#
#    1 - bold 
#    3 - italic
#    4 - underline 

require 'rubygems'
require 'coderay'
require 'coderay/encoders/term'
TERM_TOKEN_COLORS = {
  :comment => '3;37',        # sienna #8b4726
  :constant => '1;34',       # Bold Midnight Blue #191970
  :class => '1;2',           # 
  :doctype => '1;2',         # 
  :global_variable => '36',  # yellow brownish 
  :integer => '29',          # black #00000
  :label => '4',             # black underline
  :method => '34',           # blue #0000FF 
  :pre_constant => '3;33',   # goldenrod4 #8b6914
  :regexp => {
    :content => '36',        # dark cyan #008b8b
    :delimiter => '1;29',    # bold black
    :modifier => '35',
    :function => '1;29'
  },
  :string => {
    :content => '1;37',      # ivory4 (grey) #8b8b83
    :delimiter => '1;29',    # bold black
  },
  :reserved => '1;32',       # bold dark olive green #556b2f RGB: 85, 107, 47
  :symbol   => '35',         # purple4 #551A8B RGB: 85, 26, 139
}
module CodeRay::Encoders
  class Term < Encoder
    TERM_TOKEN_COLORS.each_pair do |key, value|
      TOKEN_COLORS[key] = value
    end
  end
end
