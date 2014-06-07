# University of Washington, Programming Languages, Homework 6, hw6runner.rb

require_relative './hw6provided'

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces + [rotations([[0, 0], [-1, 0], [1, 0], [0, -1], [-1, -1]]),
                                  [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]],
                                   [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                                  rotations([[0, 0], [1, 0], [0, -1]])]

  # your enhancements here
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
    if @game.is_cheating?
      @current_block = MyPiece.new([[[0, 0]]], self)
      @game.reset_cheating
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.size - 1)).each{|index|
      current = locations[index]
      @grid[current[1] + displacement[1]][current[0] + displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def minus_score(deltas)
    @score -= deltas
  end
end

class MyTetris < Tetris
  # your enhancements here
  def initialize
    @root = TetrisRoot.new
    @timer = TetrisTimer.new
    set_board
    @running = true
    @cheating = false
    key_bindings
    buttons
    run_game
  end

  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('c', lambda {self.cheat})
    @root.bind('u', lambda {@board.rotate_clockwise;@board.rotate_clockwise})
  end

  def cheat
    if !@cheating and @board.score >= 100
      @board.minus_score(100)
      @cheating = true
    end
  end

  def is_cheating?
    @cheating
  end

  def reset_cheating
    @cheating = false
  end
end

#============================================================================================
#I add a shadow to help users figure out where the piece will drop when they hit 'space' key.
#============================================================================================
class MyPieceChallenge < MyPiece
  def initialize (point_array, board)
    super(point_array, board)
    set_shadow
  end

  def set_shadow
    @current_shadow = current_rotation
    isTouch = false
    deltay = 0
    while !isTouch
      deltay += 1
      @current_shadow.each{|p|
        if !(@board.empty_at([p[0] + @base_position[0],
                              p[1] + deltay + @base_position[1]]))
          isTouch = true
        end
      }
    end
    @current_shadow = @current_shadow.map{|x, y| [x + @base_position[0], y + deltay - 1 + @base_position[1]]}
  end

  def move (delta_x, delta_y, delta_rotation)
    moved = true
    potential = @all_rotations[(@rotation_index + delta_rotation) % @all_rotations.size]
    potential.each{|posns| 
      if !(@board.empty_at([posns[0] + delta_x + @base_position[0],
                            posns[1] + delta_y + @base_position[1]]));
        moved = false;  
      end
    }
    if moved
      @base_position[0] += delta_x
      @base_position[1] += delta_y
      @rotation_index = (@rotation_index + delta_rotation) % @all_rotations.size
      set_shadow
    end
    moved
  end

  def self.next_piece (board)
    MyPieceChallenge.new(All_My_Pieces.sample, board)
  end

  def current_shadow
    @current_shadow
  end
end

class MyBoardChallenge < MyBoard
  def initialize (game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPieceChallenge.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def next_piece
    if @current_shadow_pos != nil
      @current_shadow_pos.each{|block| block.remove}
    end
    if @game.is_cheating?
      @current_block = MyPieceChallenge.new([[[0, 0]]], self)
      @game.reset_cheating
    else
      @current_block = MyPieceChallenge.next_piece(self)
    end
    @current_pos = nil
    @current_shadow_pos = nil
  end

  def draw
    @current_shadow_pos = @game.draw_shadow(@current_block, @current_shadow_pos)
    super
  end
end

class MyTetrisChallenge < MyTetris
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoardChallenge.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def draw_shadow (piece, old=nil)
    if old != nil
      old.each{|block| block.remove}
    end
    size = @board.block_size
    shadowblocks = piece.current_shadow
    shadowblocks.map{|block| 
    TetrisRect.new(@canvas, block[0]*size + 3, 
                       block[1]*size,
                       size + block[0]*size + 3, 
                       size + block[1]*size, 
                       'white')}
  end
end
