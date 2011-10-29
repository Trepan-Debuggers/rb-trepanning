def factorial(n)
  if n > 0
    return n * factorial(n-1)
  else
    return 1
  end
end
n = ARGV[0] || 5
puts "#{n}! is #{factorial(5)}"

