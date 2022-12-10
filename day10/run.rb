#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze

cycle = 1
val = 1
final_val = 0
wanted_cycles = [20, 60, 100, 140, 180, 220]

display = Array.new(6) { Array.new(40) { ?. } }

define_method :render do
  y, x = ((cycle-1) % 240).divmod(40)
  if (val-1..val+1).cover?(x)
    display[y][x] = ?#
  end
end

define_method :run do
  if wanted_cycles.include?(cycle)
    final_val += val * cycle
  end
  render
end

input.each_line do |line|
  if line =~ /^noop$/
    run
    cycle += 1
  else
    x = Integer(line.match(/^addx (-?\d+)$/)[1])
    2.times do
      run
      cycle += 1
    end
    val += x
  end
end

part1 = final_val
puts "part1=#{part1} part2="
display.each do |row|
  puts row.join
end
