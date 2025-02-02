# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.
class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],
    rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
  [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
  [[0, 0], [0, -1], [0, 1], [0, 2]]],
  rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
  rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
  rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
  rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # z
  rotations([[0, 0], [1, 0], [0, 1]]),
  rotations([[0, 0], [-1, 0], [1,0], [-2, 0], [2,0]]),
  rotations([[0, 0], [-1, 0], [1, 0], [0, 1], [-1, 1]])]  # square (only needs one)
  # your enhancements here

end
class MyBoard < Board
  def initialize(game)
    super
    @cheat_piece_next = false
  end
    def rotate_180
      if !game_over? and @game.is_running?
        @current_block.move(0, 0, 1)  # First 90-degree rotation
        @current_block.move(0, 0, 1)  # Second 90-degree rotation
      end
      draw
    end
    def next_piece
      if @cheat_piece_next
        @current_block = cheat_piece
        @cheat_piece_next = false
      else
        @current_block = MyPiece.next_piece(self)
      end
      @current_pos = nil
    end
  
    def cheat
      if score >= 100
        @score -= 100
        @game.update_score
        @cheat_piece_next = true
        true
      else
        false
      end
    end
    private
    def cheat_piece
      MyPiece.new([[[0, 0]]], self)
      end
  
    end
  



class MyTetris < Tetris

    # Override this method to use MyBoard instead of Board
    def set_board
      @canvas = TetrisCanvas.new
      @board = MyBoard.new(self)
      @canvas.place(@board.block_size * @board.num_rows + 3,
                    @board.block_size * @board.num_columns + 6, 24, 80)
      @board.draw
    end

  def key_bindings
    @root.bind('c', proc {@board.cheat})
 @root.bind('u', proc {@board.rotate_180})
  

  @root.bind('n', proc {self.new_game}) 

  @root.bind('p', proc {self.pause}) 

  @root.bind('q', proc {exitProgram})
  
  @root.bind('a', proc {@board.move_left})
  @root.bind('Left', proc {@board.move_left}) 
  
  @root.bind('d', proc {@board.move_right})
  @root.bind('Right', proc {@board.move_right}) 

  @root.bind('s', proc {@board.rotate_clockwise})
  @root.bind('Down', proc {@board.rotate_clockwise})

  @root.bind('w', proc {@board.rotate_counter_clockwise})
  @root.bind('Up', proc {@board.rotate_counter_clockwise}) 
  
  @root.bind('space' , proc {@board.drop_all_the_way}) 
  end
end




