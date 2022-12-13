#!/usr/bin/env ruby

require 'pathname'

input = Pathname.new(__dir__).join('./input.txt')

name_map = {
  'A' => 'rock',
  'B' => 'paper',
  'C' => 'scissors',
  'X' => 'rock',
  'Y' => 'paper',
  'Z' => 'scissors',
}

losing_games = [
  ['rock', 'scissors'],
  ['scissors', 'paper'],
  ['paper', 'rock'],
]
beats_map = Hash[losing_games]
looses_map = beats_map.invert

shape_points = {
  'rock' => 1,
  'paper' => 2,
  'scissors' => 3,
}

games = input.readlines.map { |line| line.split(' ') }.each(&:freeze).freeze

part1 = games.sum do |them, us|
  them = name_map.fetch(them)
  us = name_map.fetch(us)
  win_points = if us == them
    3
  elsif losing_games.include?([them, us])
    0
  else
    6
  end
  win_points + shape_points.fetch(us)
end

part2 = games.sum do |them, decision|
  them = name_map.fetch(them)
  us = if decision == 'X' # loss
    win_points = 0
    beats_map.fetch(them)
  elsif decision == 'Y' # draw
    win_points = 3
    them
  elsif decision == 'Z' # win
    win_points = 6
    looses_map.fetch(them)
  end
  win_points + shape_points.fetch(us)
end

puts "part1:#{part1} part2:#{part2}"
