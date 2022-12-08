#!/usr/bin/env ruby

require 'matrix'
require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze

grid = input.readlines.map do |line|
  line.chomp.split(//).map { |s| Integer(s) }.freeze
end.freeze

width = grid.first.length
height = grid.length
visible_grid = Array.new(height) { Array.new(width, false) }

def walk(grid, visible_grid)
  width = grid.first.length
  grid.each_with_index.reduce(Array.new(width, -1)) do |current_maxes, (row, i)|
    row.zip(current_maxes).map.with_index do |(cur_tree, max), j|
      visible_grid[i][j] ||= max < cur_tree
      [cur_tree, max].max
    end
  end
end

walk(grid, visible_grid)
3.times do
  grid = Matrix[*grid].rotate_entries.to_a
  visible_grid = Matrix[*visible_grid].rotate_entries.to_a
  walk(grid, visible_grid)
end

part1 = visible_grid.flatten.select { |x| x }.count

part2 = nil

puts "part1=#{part1} part2=#{part2}"
