#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze
lines = input.readlines.each(&:freeze).freeze
assignments = lines.flat_map do |line|
  line.strip.split(',').map do |range|
    start, stop = range.split('-').map { |s| Integer(s) }
    start..stop
  end
end.freeze

class Range
  def fully_overlaps?(other)
    self.begin <= other.begin && self.end >= other.end
  end

  def overlaps?(other)
    self.cover?(other.begin) || self.cover?(other.end) ||
      other.cover?(self.begin) || other.cover?(self.end)
  end
end

part1 = assignments.each_slice(2).select do |a, b|
  a.fully_overlaps?(b) || b.fully_overlaps?(a)
end.size

part2 = assignments.each_slice(2).select do |a, b|
  a.overlaps?(b)
end.size

puts "part1=#{part1} part2=#{part2}"
