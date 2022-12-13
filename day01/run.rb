#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt')
top3 = input.readlines
  .slice_when { |line| line == "\n" }
  .map { |vals| vals.map(&:to_i).sum }
  .sort.reverse_each.take(3)
puts "part1:#{top3.first} part2:#{top3.sum}"
