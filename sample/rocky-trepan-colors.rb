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
  :comment => ';36',    # italic yellow brownish
  :global_variable => '36',  # yellow brownish 
  :integer => '34',
  :label => '4',       # black underline
  :regexp => {
    :content => '33',  # light turquoise (from red)
    :delimiter => '1;29',
    :modifier => '35',
    :function => '1;29'
  },
  :reserved => '1;32',  # green (from red)
  :symbol   => '35',    # purple
}
module CodeRay::Encoders
  class Term < Encoder
    TERM_TOKEN_COLORS.each_pair do |key, value|
      TOKEN_COLORS[key] = value
    end
  end
end
