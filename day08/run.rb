#!/usr/bin/env ruby

require 'matrix'
require 'pathname'

input = Pathname.new(__dir__).join('./input.txt').freeze

class Hash
  def transform!; each_key { |k| self[k] = yield k, self[k] }; end
end

grid = input.readlines.map do |line|
  line.chomp.split(//).map { |s| Integer(s) }.freeze
end.freeze

TREE_HEIGHTS = (0..9).to_a
width = grid.first.length
height = grid.length
visible_grid = Array.new(height) { Array.new(width, false) }
viewing_distance_grid = Array.new(height) { Array.new(width) { [] } }

def walk(grid, visible_grid, viewing_distance_grid)
  width = grid.first.length
  current_viewing_distances = Array.new(width) { Hash[TREE_HEIGHTS.zip(Array.new(TREE_HEIGHTS.size, 0))] }
  grid.each_with_index.reduce(Array.new(width, -1)) do |current_maxes, (row, i)|
    row.zip(current_maxes).map.with_index do |(cur_tree, max), j|
      viewing_distance_grid[i][j] << current_viewing_distances[j].fetch(cur_tree)
      current_viewing_distances[j].transform! { |tree_height, dist| tree_height <= cur_tree ? 1 : dist + 1 }
      visible_grid[i][j] ||= max < cur_tree
      [cur_tree, max].max
    end
  end
end

walk(grid, visible_grid, viewing_distance_grid)
3.times do
  grid = Matrix[*grid].rotate_entries.to_a
  visible_grid = Matrix[*visible_grid].rotate_entries.to_a
  viewing_distance_grid = Matrix[*viewing_distance_grid].rotate_entries.to_a
  walk(grid, visible_grid, viewing_distance_grid)
end

part1 = visible_grid.flatten.select { |x| x }.count

part2 = viewing_distance_grid.flatten(1).map { |distances| distances.reduce(:*) }.max

puts "part1=#{part1} part2=#{part2}"
