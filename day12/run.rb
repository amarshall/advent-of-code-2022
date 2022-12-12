#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze

class Point < Struct.new(:x, :y, :raw, :height, :dist, :target?, :prev); end
OFFSETS = [-1,0,1].permutation(2).reject { |p| p.none?(&:zero?) }.freeze

grid = input.each_line.map.with_index do |line, y|
  line.chomp.split(//).map.with_index do |height, x|
    Point.new(x, y, height,
      {?S => ?a, ?E => ?z}.fetch(height, height).codepoints.first, height == ?S ? 0 : Float::INFINITY,
      height == ?E,
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

q = grid.flatten
until q.empty?
  u = q.sort_by(&:dist).first
  break if u.target?
  q.delete(u)
  (neighbors(grid, u) & q).each do |v|
    alt = u.dist + 1
    if alt < v.dist
      v.dist = alt
      v.prev = u
    end
  end
end

part1 = grid.flatten.detect(&:target?).dist
part2 = nil

puts "part1=#{part1} part2=#{part2}"
