#!/usr/bin/env ruby

require 'pathname'
require 'set'

input = Pathname.new(__dir__).join('./input.txt').freeze

class Coord < Struct.new(:x, :y); end

class Numeric
  def sign; negative? ? -1 : 1 end
end

head = Coord.new(0, 0)
tail = head.dup

tail_positions = Set.new([tail.dup])

input.each_line do |line|
  direction, dist = line.match(/^([LRUD]) (\d+)$/).captures
  dist = Integer(dist)
  Integer(dist).times do
    case direction
    when ?L then head.x -= 1
    when ?R then head.x += 1
    when ?U then head.y -= 1
    when ?D then head.y += 1
    end

    xΔ = head.x - tail.x
    yΔ = head.y - tail.y

    if [xΔ.abs, yΔ.abs].max > 1 && [xΔ.abs, yΔ.abs].min > 0
      tail.x += xΔ.sign
      tail.y += yΔ.sign
    elsif xΔ.abs > 1
      tail.x += xΔ.sign
    elsif yΔ.abs > 1
      tail.y += yΔ.sign
    end

    tail_positions << tail.dup
  end
end

part1 = tail_positions.count

part2 = nil

puts "part1=#{part1} part2=#{part2}"
