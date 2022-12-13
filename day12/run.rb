#!/usr/bin/env ruby

require 'pathname'
require 'set'

input = Pathname.new(__dir__).join('./input.txt').freeze

class Point < Struct.new(:x, :y, :raw, :height, :dist, :target?, :prev)
  def hash; x.hash ^ y.hash end
end
OFFSETS = [-1,0,1].permutation(2).reject { |p| p.none?(&:zero?) }.freeze

grid = input.each_line.map.with_index do |line, y|
  line.chomp.split(//).map.with_index do |height, x|
    Point.new(x, y, height,
      {?S => ?a, ?E => ?z}.fetch(height, height).codepoints.first, height == ?E ? 0 : Float::INFINITY,
      height == ?S,
    )
  end.freeze
end.freeze

def neighbors(grid, point)
  OFFSETS.map do |ox, oy|
    [point.x+ox, point.y+oy]
  end.select { |p| p.none?(&:negative?) }.map do |x, y|
    grid[y]&.[](x)
  end.compact.reject do |neighbor|
    point.height - neighbor.height > 1
  end
end

q = grid.flatten.to_set
until q.empty?
  u = q.sort_by(&:dist).first
  q.delete(u)
  (q & neighbors(grid, u)).each do |v|
    alt = u.dist + 1
    if alt < v.dist
      v.dist = alt
      v.prev = u
    end
  end
end

part1 = grid.flatten.detect(&:target?).dist
part2 = grid.flatten.select { |p| p.raw == ?a }.map(&:dist).min

puts "part1:#{part1} part2:#{part2}"
