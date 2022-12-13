#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze
lines = input.readlines.each(&:freeze).freeze

def parents(path)
  return [] if path.parent == path
  [path.parent, *parents(path.parent)]
end

cwd = Pathname.new('/')
total_size = 0
sizes = Hash.new(0)
lines.each do |line|
  if arg = line.match(/^\$ cd (.*)$/)&.[](1)
    if arg == '/' then cwd = Pathname.new('/')
    elsif arg == '..' then cwd = cwd.parent
    else cwd = cwd.join(arg)
    end
  elsif line.match(/^dir (.*)$/)
  elsif line.match(/^\$ ls/)
  elsif match = line.match(/^(\d+) (.*)$/)
    _name, size = match[2], Integer(match[1])
    total_size += size
    [cwd, *parents(cwd)].each do |path|
      sizes[path] += size
    end
  else raise "unexpected line: #{line}"
  end
end
part1 = sizes.values.select { |x| x <= 100_000 }.sum

total_space = 70_000_000
wanted_space = 30_000_000
needed_space = wanted_space - (total_space - total_size)
part2 = sizes.values.sort.detect { |size| size >= needed_space }

puts "part1:#{part1} part2:#{part2}"
