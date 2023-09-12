#' Generate Maze Object
#'
#' @param w_maze width of maze
#' @param l_maze length of maze
#' @param h_maze height (number of layers) of maze
#' @param exit coordinates of exit
#' @param i_weight a positive weight given to random directions in the i direction
#' @param j_weight a positive weight given to random directions in the j direction
#' @param k_weight a positive weight given to random directions in the k direction
#' @param curl a real on the interval `[-1, 1]` with negative values favoring clockwise
#' turns and positive values favoring counterclockwise turns. a curl of 0 favors continuing
#' in a straight line.
#' @param curl_weight a positive weight given to curl related
#' @param uncurl a logical which causes the direction of curl to alternate each time it resets
#' @param progress a logical which triggers an update to print each time 1000 cells have been visited
#' @param seed a real value which can be passed as a seed to reproduce a maze
#' @param mask_dir a .bmp file to use as a mask
#' @param ... other parameters
#' @examples
#' generate_maze()
#' @export
#' @importFrom stats rbinom
#' @import utils
#' @import tidyr
#' @importFrom magrittr equals
#' @import bmp
#' @returns a Maze object

generate_maze <-
  function(w_maze = 10,
           l_maze = 10,
           h_maze = 1,
           exit = NULL,
           i_weight = 1,
           j_weight = 1,
           k_weight = 0.1,
           curl = 0,
           curl_weight = 0,
           uncurl = FALSE,
           progress = FALSE,
           mask_dir = NULL,
           seed = NULL, 
           ... = NULL){
    
    # function to check whether bmp mask allows move
    move_allowed <-
      function(bw_mask, i, j, direction){
        if(direction == 1){return((bw_mask[i, j] == 1 && bw_mask[i + 1, j] == 1 && any(try(bw_mask[i + 1, j + 1] == 0), j == 1)) | bw_mask[i + 1, j] == 0)}
        if(direction == 2){return((bw_mask[i, j] == 1 && bw_mask[i, j + 1] == 1 && any(try(bw_mask[i + 1, j + 1] == 0), i == w_maze)) | bw_mask[i, j + 1] == 0)}
        if(direction == 6){return(bw_mask[i, j] == 0 | (bw_mask[i, j] == 1 && bw_mask[i - 1, j] == 1 && any(try(bw_mask[i, j + 1] == 0), j == l_maze)))}
        if(direction == 5){return(bw_mask[i, j] == 0 | (bw_mask[i, j] == 1 && bw_mask[i, j - 1] == 1 && any(try(bw_mask[i + 1, j] == 0), i == 1)))}
      }
    
    # load mask
    if(!is.null(mask_dir)){
      tmp_bmp <-
        read.bmp(mask_dir)
      
      bw_mask <-
        round(0^sqrt(
          ((tmp_bmp[, , 1] / 255)^2 +
             (tmp_bmp[, , 2] / 255)^2 +
             (tmp_bmp[, , 3] / 255)^2) / 
            3), 0) %>%
        .[nrow(.):1, ]
      
      w_maze <- nrow(bw_mask)
      l_maze <- ncol(bw_mask)
    }else{
      bw_mask <- matrix(rep(0, w_maze * l_maze), nrow = w_maze)
    }
    
    # set seed, if provided
    if(!is.null(seed)){set.seed(seed)}
    
    # define exit, if not provided
    if(is.null(exit)){exit <- c(w_maze, l_maze, h_maze)}
    if(length(exit) == 2){exit <- c(exit, 1)}
    
    # initialize variables
    n_cells <- w_maze * l_maze * h_maze
    maze_array <-
      array(c(rep(1L, n_cells * 3),
              rep(0L, n_cells)),
            dim = c(w_maze, l_maze, h_maze, 4))
    available_set <- c(1L, 2L, 3L, 4L, 5L, 6L)
    apply_curl <- !equals(curl_weight, 0)
    last_dir <- 0L
    path <- list()
    i <- 1
    j <- 1
    k <- 1
    
    cell_counter <- 0
    
    # find path through all cells
    while(cell_counter < n_cells){
      
      # print update status every 10000 cells if `progress` is set
      if(cell_counter %% 1000 == 0 & progress){print(cell_counter)}
      
      # list of coordinates from start to current position
      path <- lappend(path, c(i, j, k))
      
      # save solution if you've reached the end
      if(all(c(i, j, k) == exit)){solution <- path}
      
      cell_counter <- cell_counter + 1
      
      # mark current cell as "visited"
      maze_array[i, j, k, 4] <- 1L
      
      # stop if all cells have been visited
      if(cell_counter == n_cells){break}
      
      ###########################
      ###    Direction map    ###
      ###########################
      #   1     <->    +i       #
      # 5 4 2 3 <-> -j -k +j +k #
      #   6     <->    -i       #
      ###########################
      
      # initialize vector containing probabilities of moving in each direction
      move_probs <- rep(0L, 6)
      
      # update probabilities
      while(sum(move_probs) == 0L){
        
        if(i < w_maze){if(maze_array[i + 1, j, k, 4] != 1L && move_allowed(bw_mask, i, j, 1)){move_probs[1] <- i_weight}}
        if(j < l_maze){if(maze_array[i, j + 1, k, 4] != 1L && move_allowed(bw_mask, i, j, 2)){move_probs[2] <- j_weight}}
        if(k < h_maze){if(maze_array[i, j, k + 1, 4] != 1L){move_probs[3] <- k_weight}}
        if(i > 1L)    {if(maze_array[i - 1, j, k, 4] != 1L && move_allowed(bw_mask, i, j, 6)){move_probs[6] <- i_weight}}
        if(j > 1L)    {if(maze_array[i, j - 1, k, 4] != 1L && move_allowed(bw_mask, i, j, 5)){move_probs[5] <- j_weight}}
        if(k > 1L)    {if(maze_array[i, j, k - 1, 4] != 1L){move_probs[4] <- k_weight}}
        
        
        if(apply_curl){
          if(last_dir == 1L){
            move_probs[c(5, 1, 2)] <- curler2(move_probs[c(5, 1, 2)], curl, curl_weight)
          }
          if(last_dir == 2L){
            move_probs[c(1, 2, 6)] <- curler2(move_probs[c(1, 2, 6)], curl, curl_weight)
          }
          if(last_dir == 6L){
            move_probs[c(2, 6, 5)] <- curler2(move_probs[c(2, 6, 5)], curl, curl_weight)
          }
          if(last_dir == 5L){
            move_probs[c(6, 5, 1)] <- curler2(move_probs[c(6, 5, 1)], curl, curl_weight)
          }
        }
        
        # backtrack if no paths forward are available
        if(sum(move_probs) == 0L){
          path <- head(path, -1)
          tmp <- tail(path, 1)[[1]]
          last_dir <- 0L
          i <- tmp[1]
          j <- tmp[2]
          k <- tmp[3]
          if(uncurl){curl <- -curl}
        }
        
      }
      
      # choose direction of next move
      next_dir <- sample(available_set, 1, prob = move_probs)
      
      i2 <- i
      j2 <- j
      k2 <- k
      
      if(next_dir == 1L){i2 <- i + 1}
      if(next_dir == 2L){j2 <- j + 1}
      if(next_dir == 3L){k2 <- k + 1}
      if(next_dir == 4L){k2 <- k - 1}
      if(next_dir == 5L){j2 <- j - 1}
      if(next_dir == 6L){i2 <- i - 1}
      
      if(next_dir <= 3L){
        maze_array[i, j, k, next_dir] <- 0
      }else{
        maze_array[i2, j2, k2, 7 - next_dir] <- 0
      }
      
      i <- i2
      j <- j2
      k <- k2
      last_dir <- next_dir
      
    }
    
    # create Maze S4 object
    new("Maze",
        width = w_maze,
        length = l_maze,
        height = h_maze,
        array = maze_array,
        exit = exit,
        solution = solution,
        tile = "square",
        mask = bw_mask)
  }

#' Generate Maze Object on Triangular Grid
#'
#' @param w_maze width of maze
#' @param l_maze length of maze
#' @param exit `c(w, l)` coordinates of exit
#' @param i_weight a positive weight given to random directions in the i direction
#' @param j_weight a positive weight given to random directions in the j direction
#' @param curl a real on the interval `[-1, 1]` with negative values favoring clockwise
#' turns and positive values favoring counterclockwise turns. a curl of 0 favors continuing
#' in a straight line.
#' @param curl_weight a positive weight given to curl related
#' @param uncurl a logical which causes the direction of curl to alternate each time it resets
#' @param progress a logical which triggers an update to print each time 1000 cells have been visited
#' @param seed a real value which can be passed as a seed to reproduce a maze
#' @param ... other parameters
#' @examples
#' generate_maze_tri()
#' @export
#' @importFrom stats rbinom
#' @import utils
#' @import tidyr
#' @importFrom magrittr equals
#' @import bmp
#' @returns a Maze object

generate_maze_tri <-
  function(w_maze = 5,
           l_maze = 10,
           exit = NULL,
           i_weight = 1,
           j_weight = 1,
           curl = 0,
           curl_weight = 0,
           uncurl = FALSE,
           progress = FALSE,
           seed = NULL, 
           ... = NULL){
    
    # set seed, if provided
    if(!is.null(seed)){set.seed(seed)}
    
    # define exit, if not provided
    if(is.null(exit)){exit <- c(w_maze, l_maze)}
    
    # initialize variables
    n_cells <- w_maze * l_maze
    maze_array <-
      array(c(rep(outer(1:w_maze, 1:l_maze - 1,"+") %% 2, 3),
              rep(0L, n_cells)),
            dim = c(w_maze, l_maze, 4))
    available_set <- c(1L, 2L, 3L, 4L, 5L, 6L)
    apply_curl <- !equals(curl_weight, 0)
    last_dir <- 0L
    path <- list()
    i <- 1
    j <- 1
    
    cell_counter <- 0
    
    # find path through all cells
    while(cell_counter < n_cells){
      
      # print update status every 10000 cells if `progress` is set
      if(cell_counter %% 1000 == 0 & progress){print(cell_counter)}
      
      # list of coordinates from start to current position
      path <- lappend(path, c(i, j))
      
      # save solution if you've reached the end
      if(all(c(i, j) == exit)){solution <- path}
      
      cell_counter <- cell_counter + 1
      
      # mark current cell as "visited"
      maze_array[i, j, 4] <- 1L
      
      # stop if all cells have been visited
      if(cell_counter == n_cells){break}
      
      ###########################
      ###    Direction map    ###
      ###########################
      
      # initialize vector containing probabilities of moving in each direction
      move_probs <- rep(0L, 6)
      
      # update probabilities
      while(sum(move_probs) == 0L){
        
        if(i < w_maze){if(maze_array[i + 1, j, 4] != 1L && is.odd(i + j)){move_probs[1] <- i_weight}}
        if(j < l_maze){if(maze_array[i, j + 1, 4] != 1L){move_probs[2] <- j_weight}}
        if(i > 1L)    {if(maze_array[i - 1, j, 4] != 1L && is.even(i + j)){move_probs[6] <- i_weight}}
        if(j > 1L)    {if(maze_array[i, j - 1, 4] != 1L){move_probs[5] <- j_weight}}
        
        
        if(apply_curl){
          if(last_dir == 1L){
            move_probs[c(5, 2, 6)] <- curler2(move_probs[c(5, 2, 6)], curl, curl_weight)
          }
          if(last_dir == 2L && is.odd(i + j)){
            move_probs[c(1, 2, 5)] <- curler2(move_probs[c(1, 2, 5)], curl, curl_weight)
          }
          if(last_dir == 2L && is.even(i + j)){
            move_probs[c(6, 2, 5)] <- curler2(move_probs[c(6, 2, 5)], curl, curl_weight)
          }
          if(last_dir == 6L){
            move_probs[c(2, 5, 1)] <- curler2(move_probs[c(2, 5, 1)], curl, curl_weight)
          }
          if(last_dir == 5L && is.odd(i + j)){
            move_probs[c(6, 5, 2)] <- curler2(move_probs[c(6, 5, 2)], curl, curl_weight)
          }
          if(last_dir == 5L && is.even(i + j)){
            move_probs[c(5, 1, 2)] <- curler2(move_probs[c(5, 1, 2)], curl, curl_weight)
          }
        }
        
        # backtrack if no paths forward are available
        if(sum(move_probs) == 0L){
          path <- head(path, -1)
          tmp <- tail(path, 1)[[1]]
          last_dir <- 0L
          i <- tmp[1]
          j <- tmp[2]
          if(uncurl){curl <- -curl}
        }
        
      }
      
      # choose direction of next move
      next_dir <- sample(available_set, 1, prob = move_probs)
      
      i2 <- i
      j2 <- j
      
      if(next_dir == 1L){i2 <- i + 1}
      if(next_dir == 2L){j2 <- j + 1}
      if(next_dir == 5L){j2 <- j - 1}
      if(next_dir == 6L){i2 <- i - 1}
      
      if(next_dir == 1L && is.even(i + j)){maze_array[i, j, 1] <- 0L}
      if(next_dir == 1L && is.odd(i + j)){maze_array[i + 1, j, 1] <- 0L}
      if(next_dir == 6L && is.even(i + j)){maze_array[i, j, 1] <- 0L}
      if(next_dir == 6L && is.odd(i + j)){maze_array[i - 1, j, 1] <- 0L}
      if(next_dir == 2L && is.even(i + j)){maze_array[i, j, 2] <- 0L}
      if(next_dir == 2L && is.odd(i + j)){maze_array[i, j + 1, 3] <- 0L}
      if(next_dir == 5L && is.even(i + j)){maze_array[i, j, 3] <- 0L}
      if(next_dir == 5L && is.odd(i + j)){maze_array[i, j - 1, 2] <- 0L}
      
      
      i <- i2
      j <- j2
      last_dir <- next_dir
      
    }
    
    # create Maze S4 object
    new("Maze",
        width = w_maze,
        length = l_maze,
        height = 1,
        array = maze_array,
        exit = exit,
        solution = solution,
        tile = "triangle")
  }

