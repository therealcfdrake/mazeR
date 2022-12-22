
setClass(
  "Maze",
  slots = c(
    width = "numeric",
    length = "numeric",
    height = "numeric",
    array = "array",
    exit = "vector",
    solution = "list"
  ),
  prototype = list(
    width = NA_real_,
    length = NA_real_,
    height = NA_real_,
    array = array(),
    exit = c(NA_real_, NA_real_, NA_real_),
    solution = NULL
  )
)


setMethod("show", signature(object = "Maze"), function(object){
  print(render_maze(object))
})


# Append List
lappend <-
  function (lst, ...){ # function borrowed from stack
    lst <- c(lst, list(...))
  return(lst)
}

#' Generate Maze Object
#'
#' @param w_maze width of maze
#' @param l_maze length of maze
#' @param h_maze height (number of layers) of maze
#' @param exit `c(w, l, h)` coordinates of exit
#' @param i_weight a positive weight given to random directions in the i direction
#' @param j_weight a positive weight given to random directions in the j direction
#' @param k_weight a positive weight given to random directions in the k direction
#' @param curl a real on the interval `[-1, 1]` with negative values favoring clockwise
#' turns and positive values favoring counterclockwise turns. a curl of 0 favors continuing
#' in a straight line.
#' @param curl_weight a positive weight given to curl related
#' @param curl_prob a numeric value describing the probability that curl is applied. Once applied, curl
#'  will influence subsequent direction choices until the algorithm reaches a dead end.
#' @param uncurl a logical which causes the direction of curl to alternate each time it resets
#' @param progress a logical which triggers an update to print each time 1000 cells have been visited
#' @param seed a real value which can be passed as a seed to reproduce a maze
#' @param mask_dir a .bmp file to use as a mask
#' @param ... other parameters
#' @examples
#' library(tidyverse)
#' generate_maze()
#' @export
#' @import tidyr
#' @import magrittr
#' @import bmp
#' @returns a Maze object

generate_maze <-
  function(w_maze = 10,
           l_maze = 10,
           h_maze = 1,
           exit = c(w_maze, l_maze, h_maze),
           i_weight = 1,
           j_weight = 1,
           k_weight = 0.1,
           curl = 0,
           curl_weight = 0,
           curl_prob = 0,
           uncurl = FALSE,
           progress = FALSE,
           mask_dir = NULL,
           seed = NULL, ...){

  # function to implement curl (rotational bias)
  curler <-
    function(base_weights, curl, curl_weight){
      base_weights +
        pmax(
          c(cos(curl * pi / 2) * curl_weight,
            sin(curl * pi / 2) * curl_weight,
            sin(-curl * pi / 2) * curl_weight
            ) * !equals(base_weights, 0L),
          0)
    }

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
      read.bmp(here::here(mask_dir))

    bw_mask <-
      round(0^sqrt(
        ((tmp_bmp[, , 1] / 255)^2 +
           (tmp_bmp[, , 2] / 255)^2 +
           (tmp_bmp[, , 3] / 255)^2) / 3), 0) %>%
      .[nrow(.):1, ]

    w_maze <- nrow(bw_mask)
    l_maze <- ncol(bw_mask)
  }else{
    bw_mask <- matrix(rep(0, w_maze * l_maze), nrow = w_maze)
  }

  # set seed, if provided
  if(!is.null(seed)){set.seed(seed)}

  # define exit, if not provided
  if(is.null(exit)){exit <- c(sample(1:w_maze, 1), sample(1:l_maze, 1), sample(1:h_maze, 1))}

  # initialize variables
  n_cells <- w_maze * l_maze * h_maze
  maze_array <-
    array(c(rep(1L, n_cells * 3),
            rep(0L, n_cells)),
          dim = c(w_maze, l_maze, h_maze, 4))
  available_set <- c(1L, 2L, 3L, 4L, 5L, 6L)
  apply_curl <- FALSE
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
    ###  RNG-direction map  ###
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

      if(!apply_curl){apply_curl <-  rbinom(1, 1, curl_prob) == 1}

      if(apply_curl){
        if(last_dir == 1L){
          move_probs[c(1, 2, 5)] <- curler(move_probs[c(1, 2, 5)], curl, curl_weight)
        }
        if(last_dir == 2L){
          move_probs[c(2, 6, 1)] <- curler(move_probs[c(2, 6, 1)], curl, curl_weight)
        }
        if(last_dir == 6L){
          move_probs[c(6, 5, 2)] <- curler(move_probs[c(6, 5, 2)], curl, curl_weight)
        }
        if(last_dir == 5L){
          move_probs[c(5, 1, 6)] <- curler(move_probs[c(5, 1, 6)], curl, curl_weight)
        }
      }

      # backtrack if no paths forward are available
      if(sum(move_probs) == 0L){
        path <- head(path, -1)
        tmp <- tail(path, 1)[[1]]
        apply_curl <- FALSE
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
      solution = solution)
}

#' Render a Maze object
#'
#' @param maze a Maze object
#' @param show_solution a logical describing whether to plot the solution to the maze
#' @examples
#' library(tidyverse)
#' generate_maze()
#' @export
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @importFrom lemon facet_rep_wrap
#' @returns ggplot of Maze object

render_maze <-
  function(maze, show_solution = FALSE){

  w_maze <- maze@width
  l_maze <- maze@length
  h_maze <- maze@height
  maze_array <- maze@array
  exit <- maze@exit
  solution <- maze@solution

  # initialize solution df
  plotdata_solution <-
    data.frame(y = c(1, 1), x = c(1, 1), z = c(1, 1), id = c(0, 0))

  # build solution df
  if(show_solution) {
    plotdata_solution <-
      solution %>%
      unlist %>%
      matrix(ncol = 3,
             byrow = TRUE,
             dimnames = list(NULL, c("y", "x", "z"))
             ) %>%
      data.frame() %>%
      # mutate(id = cumsum(c(0, as.numeric(diff(.$z)) != 0)))
      mutate(id = row_number())
  }

  # length of solution path
  n_sol_path <-
    pull(plotdata_solution) %>%
    unique %>%
    length

  # initialize maze walls list with borders
  wallslist <-
    list(data.frame(id = rep(rep("edge", 3), h_maze), # southern and western border
                    x = rep(c(0.5, 0.5, l_maze + 0.5), h_maze),
                    y = rep(c(w_maze + 0.5, 0.5, 0.5), h_maze),
                    z = sort(rep(1:h_maze, 3))))

  ##### extract coordinates for northern walls

  for(k in 1:h_maze){
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, k, 1] == 1L){
          wallslist <-
            lappend(wallslist,
                    data.frame(id = rep(paste0("i", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0"), str_pad(k, width = nchar(h_maze), side = "left", pad = "0")), 2),
                               x = c(j - 0.5, j + 0.5),
                               y = c(i + 0.5, i + 0.5),
                               z = c(k, k))
                    )
        }
      }
    }
  }

  ##### extract coordinates for eastern walls

  for(k in 1:h_maze){
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, k, 2] == 1L){
          wallslist <-
            lappend(wallslist,
                    data.frame(id = rep(paste0("j", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0"), str_pad(k, width = nchar(h_maze), side = "left", pad = "0")), 2),
                               x = c(j + 0.5, j + 0.5),
                               y = c(i - 0.5, i + 0.5),
                               z = c(k, k))
                    )
        }
      }
    }
  }

  ##### extract coordinate for ladders

  ladderlist <- list()

  for(k in 1:h_maze){
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, k, 3] == 0L){
          ladderlist <-
            lappend(ladderlist,
                    data.frame(id = rep(paste0("k", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0"), str_pad(k, width = nchar(h_maze), side = "left", pad = "0")), 2),
                               x = c(j, j),
                               y = c(i, i),
                               z = c(k, k + 1),
                               d = c("u", "d"))
                    )
        }
      }
    }
  }

  # define start- and end-points of path
  startenddata <-
    data.frame(id = c("start", "end"),
               x = c(1, exit[2]),
               y = c(1, exit[1]),
               z = c(1, exit[3]),
               d = c("none", "none"))

  plotdata_walls <-
    bind_rows(wallslist)

  plotdata_points <-
    bind_rows(ladderlist) %>%
    bind_rows(startenddata)

  # create ggplot of maze
  if(h_maze == 1){
    plot_out <-
      plotdata_walls %>%
      ggplot(aes(x = x, y = y)) +
      geom_path(aes(group = id), lineend = "round", size = 0) +
      geom_path(data = plotdata_solution, aes(color = id), size = 1, lineend = "round", linejoin = "round") +
      scale_color_gradient(low = "limegreen", high = "purple4") +
      geom_point(data = plotdata_points, aes(shape = d), color = "black") +
      scale_shape_manual(values = c("u" = 2, "d" = 6, "none" = 16)) +
      scale_x_continuous(breaks = 1:l_maze) +
      scale_y_continuous(breaks = 1:w_maze) +
      coord_equal() +
      theme(panel.background = element_rect(fill = NA),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            legend.position = "none",
            plot.margin = unit( c(0,0,0,0), "in" ),
            axis.ticks.length = unit(0, "cm")) +
      NULL} else { # alternate format for mazes with more than 1 floor
        plot_out <-
          plotdata_walls %>%
          ggplot(aes(x = x, y = y)) +
          geom_path(aes(group = id), lineend = "round", linejoin = "round") +
          geom_path(data = plotdata_solution, aes(group = id, color = factor(id, levels = 0:n_sol_path, ordered = TRUE)), size = 1, lineend = "round", linejoin = "round") +
          scale_color_manual(values = colorRampPalette(c("goldenrod1", "springgreen4", "purple4"))(n_sol_path)) +
          geom_point(data = plotdata_points, aes(shape = d, fill = d), color = "black", alpha = 0.5) +
          scale_shape_manual(values = c("u" = 24, "d" = 25, "none" = 21)) +
          scale_fill_manual(values = c("u" = "deepskyblue", "d" = "deeppink", "none" = "green")) +
          scale_x_continuous(breaks = 1:l_maze) +
          scale_y_continuous(breaks = 1:w_maze) +
          coord_equal() +
          facet_rep_wrap(paste("Level", str_pad(z, width = nchar(h_maze), side = "left", pad = "0")) ~ ., repeat.tick.labels = TRUE) +
          theme(panel.background = element_rect(fill = NA),
                axis.text = element_text(color = "gray50"),
                axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5),
                axis.ticks = element_blank(),
                axis.title = element_blank(),
                panel.grid.major = element_line(color = "gray95"),
                legend.position = "none") +
          NULL
      }

  plot_out

}

#' Export text coordinates of Maze object walls
#'
#' @param maze a Maze object
#' @param filename name of output .txt file
#' @export
#' @import dplyr
#' @import stringr

export_maze_3d <- function(maze, filename){

  cat("", file = filename, append = FALSE)

  w_maze <- maze@width
  l_maze <- maze@length
  h_maze <- maze@height
  maze_array <- maze@array

  wallslist <- list(data.frame(id = rep(rep("edge", 3), h_maze), # southern and western border
                               x = rep(c(0.5, 0.5, l_maze + 0.5), h_maze),
                               y = rep(c(w_maze + 0.5, 0.5, 0.5), h_maze),
                               z = sort(rep(1:h_maze, 3))))

  ##### coordinates for northern walls

  for(k in 1:h_maze){
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, k, 1] == 1L){
          wallslist <-
            lappend(wallslist, data.frame(id = rep(paste0("i", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0"), str_pad(k, width = nchar(h_maze), side = "left", pad = "0")), 2), x = c(j - 0.5, j + 0.5), y = c(i + 0.5, i + 0.5), z = c(k, k)))
        }
      }
    }
  }

  ##### coordinates for eastern walls

  for(k in 1:h_maze){
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, k, 2] == 1L){
          wallslist <- lappend(wallslist, data.frame(id = rep(paste0("j", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0"), str_pad(k, width = nchar(h_maze), side = "left", pad = "0")), 2), x = c(j + 0.5, j + 0.5), y = c(i - 0.5, i + 0.5), z = c(k, k)))
        }
      }
    }
  }

  ##### coordinate for ladders

  ladderlist <- list()

  for(k in 1:h_maze){
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, k, 3] == 0L){
          ladderlist <- lappend(ladderlist, data.frame(id = rep(paste0("k", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0"), str_pad(k, width = nchar(h_maze), side = "left", pad = "0")), 2), x = c(j, j), y = c(i, i), z = c(k, k + 1), d = c("u", "d")))
        }
      }
    }
  }

  plotdata_walls <-
    bind_rows(wallslist)
  plotdata_points <-
    bind_rows(ladderlist)

  for(r in 1:nrow(plotdata_walls)){
    if(r > 1){if(plotdata_walls$id[r] != plotdata_walls$id[r - 1]){cat("", file = filename, sep = "\n", append = TRUE)}}
    cat(paste(plotdata_walls$x[r],
              plotdata_walls$y[r],
              plotdata_walls$z[r],
              sep = ","),
        file = filename,
        sep = "\n",
        append = TRUE)

  }

}
