#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze
input_stacks, input_actions = input.read.split("\n\n").each(&:freeze)

stacks = input_stacks.split("\n").reverse_each.drop(1).map do |line|
  line.split(//).each_slice(4).map do |item|
    item.join.match(/\[(.)\]/)&.[](1)
  end
end.transpose.map(&:compact).each(&:freeze).freeze

actions = input_actions.split("\n").map do |line|
  count, from, to = line.match(/move (\d+) from (\d+) to (\d+)/).captures.map { |s| Integer(s) }
  {count:, from:, to:}
end.each(&:freeze).freeze

def apply_action(stacks:, count:, from:, to:, reverse:)
  items = stacks[from-1].pop(count)
  items.reverse! if reverse
  stacks[to-1].concat(items)
end

part1 = begin
  stacks1 = stacks.dup.map(&:dup)
  actions.each { |action| apply_action(stacks: stacks1, reverse: true, **action) }
  stacks1.map(&:last).join
end

part2 = begin
  stacks2 = stacks.dup.map(&:dup)
  actions.each { |action| apply_action(stacks: stacks2, reverse: false, **action) }
  stacks2.map(&:last).join
end

puts "part1:#{part1} part2:#{part2}"
