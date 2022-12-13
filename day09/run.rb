#!/usr/bin/env ruby

require 'pathname'
require 'set'

input = Pathname.new(__dir__).join('./input.txt').freeze

class Coord < Struct.new(:x, :y); end

class Numeric
  def sign; negative? ? -1 : 1 end
end

def run(actions, rope_length)
  head, *, tail = rope = Array.new(rope_length) { Coord.new(0, 0) }
  tail_positions = Set.new([tail.dup])

  actions.each do |direction, dist|
    Integer(dist).times do
      case direction
      when ?L then head.x -= 1
      when ?R then head.x += 1
      when ?U then head.y -= 1
      when ?D then head.y += 1
      end

      rope.each_cons(2) do |prev, this|
        xΔ = prev.x - this.x
        yΔ = prev.y - this.y

        if [xΔ.abs, yΔ.abs].max > 1 && [xΔ.abs, yΔ.abs].min > 0
          this.x += xΔ.sign
          this.y += yΔ.sign
        elsif xΔ.abs > 1
          this.x += xΔ.sign
        elsif yΔ.abs > 1
          this.y += yΔ.sign
        end
      end

      tail_positions << tail.dup
    end
  end

  tail_positions
end

actions = input.each_line.map do |line|
  direction, dist = line.match(/^([LRUD]) (\d+)$/).captures
  [direction, Integer(dist)].each(&:freeze).freeze
end.freeze

part1 = run(actions, 2).count
part2 = run(actions, 10).count

puts "part1:#{part1} part2:#{part2}"
