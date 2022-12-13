#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze
packets = input.read.split(//).each(&:freeze).freeze

def find_marker(packets, n)
  packets.each_cons(n).with_index do |xs, idx|
    break idx + n if xs.uniq.length == xs.length
  end
end

part1 = find_marker(packets, 4)
part2 = find_marker(packets, 14)

puts "part1:#{part1} part2:#{part2}"
