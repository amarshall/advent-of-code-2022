#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze

class Monkey < Struct.new(:items, :op, :test_div, :true_monkey, :false_monkey, :inspections)
  def initialize
    self.inspections = 0
  end

  def dup
    super.tap { |m| m.items = m.items.dup }
  end
end

monkeys = input.read.split("\n\n").map do |paragraph|
  monkey = Monkey.new
  paragraph.split("\n").each do |line|
    case line
    when /^Monkey / then nil
    when /^  Starting items: ([0-9, ]+)$/ then monkey.items = $1.split(', ').map { |s| Integer(s) }
    when /^  Operation: new = old ([+*]) (\d+|old)$/
      op, val = $1, $2
      monkey.op = ->(old) do
        x = val == 'old' ? old : Integer(val)
        old.public_send(op, x)
      end
    when /^  Test: divisible by (\d+)$/ then monkey.test_div = Integer($1)
    when /^    If true: throw to monkey (\d+)$/ then monkey.true_monkey = Integer($1)
    when /^    If false: throw to monkey (\d+)$/ then monkey.false_monkey = Integer($1)
    else raise "unexpected line: #{line}"
    end
  end
  monkey
end

def run(monkeys, rounds, worry_div:)
  lcm = monkeys.map(&:test_div).reduce(:lcm)
  rounds.times do |round|
    monkeys.each.with_index do |monkey, idx|
      while item = monkey.items.shift
        item = monkey.op.(item)
        if worry_div
          item = (item / 3.0).floor
        else
          item %= lcm
        end
        throw_to = item % monkey.test_div == 0 ? monkey.true_monkey : monkey.false_monkey
        monkeys[throw_to].items << item
        monkey.inspections += 1
      end
    end
  end
  monkeys.map(&:inspections).sort.reverse_each.take(2).reduce(:*)
end

part1 = run(monkeys.dup.map(&:dup), 20, worry_div: true)
part2 = run(monkeys.dup.map(&:dup), 10_000, worry_div: false)

puts "part1:#{part1} part2:#{part2}"
