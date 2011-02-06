# use_grammar.rb
require 'rubygems'
require 'citrus'
Citrus.require '/src/external-vcs/rb-trepanning/data/method_name.citrus'

%w(a a1 $global __FILE__ Constant 0 1e10 a.b).each do |name|
  begin
    match = MethodName.parse(name, :root => :identifier)
    p [name, match.value.type, 'succeeded']
  rescue Citrus::ParseError
    p [name, 'failed']
  end
end

%w(Object
   A::B
   A::B::C
   A::B::C::D
   A::B.c
   A.b.c.d
  ).each do |name|
  begin
    match = MethodName.parse(name, :root => :class_module_chain)
    p [name, match.value.type, match.value.name, match.value.chain, 'succeeded']
    m = match.value.chain[1]
    while m
      p [m.value.name, m.value.type]
      m = m.value.chain[1]
    end
  rescue Citrus::ParseError
    p [name, 'failed']
  end
end

# Shows if we can resolve a name to a method given match_data
def resolve_method(match_data, binding)
  m = match_data
  name = m.value.name
  if m.value.type == :constant
    begin
      klass = eval(name, binding)
      m = m.value.chain[1]
    rescue NameError
      puts "Can't resolve constant #{name}"
      return nil
    end
  else
    begin
      meth = eval("self.method(#{name.inspect})", binding)
      puts "method #{meth}"
      return meth
    rescue
      puts "Can't get method for #{name.inspect}"
      return nil
    end
  end
end

def five; 5 end

%w(five Object.dup).each do |name|
  match = MethodName.parse(name, :root => :class_module_chain)
  resolve_method(match, binding)
end


