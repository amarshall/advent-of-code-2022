#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze
lines = input.readlines.each(&:freeze).freeze
rucksacks = lines.map { |line| line.strip.split(//) }.each(&:freeze).freeze

class Array
  def split_at(idx)
    [slice(0, idx), slice(idx, length - idx)]
  end
end

priorities = Hash[(('a'..'z').to_a + ('A'..'Z').to_a).zip(1..Float::INFINITY)]

part1 = rucksacks.flat_map do |rucksack_items|
  compartment_1, compartment_2 = rucksack_items.split_at(rucksack_items.length / 2)
  (compartment_1 & compartment_2).map { |item| priorities.fetch(item) }
end.sum

part2 = rucksacks.each_slice(3).flat_map do |group|
  group.reduce(:&).map { |item| priorities.fetch(item) }
end.sum

puts "part1:#{part1} part2:#{part2}"
