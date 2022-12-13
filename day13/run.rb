#!/usr/bin/env ruby

require 'json'
require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze
packets = input.readlines.grep(/./).map { |s| JSON.parse(s) }

def check_order(a, b)
  case [a, b]
  in [Integer, Integer] then a != b ? a < b : nil
  in [Array, Array]
    padding = Array.new([a,b].map(&:size).max, nil)
    padding.zip(a, b).reduce(nil) do |_, (_, a, b)|
      x = check_order(a, b)
      break x unless x.nil?
      nil
    end
  in [nil, _] then true
  in [_, nil] then false
  in [Array, Integer] then check_order(a, [b])
  in [Integer, Array] then check_order([a], b)
  end
end

part1 = packets.each_slice(2).map.with_index do |(a, b), idx|
  check_order(a, b) ? idx+1 : nil
end.compact.sum

dividers = [[[2]], [[6]]]
sorted = (packets + dividers).sort { |a, b| check_order(a, b) ? -1 : 1 }
part2 = dividers.map { |divider| sorted.index(divider) + 1 }.reduce(:*)

puts "part1:#{part1} part2:#{part2}"
