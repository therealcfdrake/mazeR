
#' @import utils

maze_plot <- function(plotdata_walls, plotdata_points, plotdata_solution, l_maze, w_maze, ratio){
  # data.frame(x = NA, y = NA) %>%
  plotdata_walls %>%
    ggplot(aes(x = x, y = y)) +
    # geom_path(data = filter(plotdata_walls, color == "red"), aes(group = id), lineend = "round", linejoin = "round", lwd = 1) +
    # geom_path(data = filter(plotdata_walls, color == "black"), aes(group = id), lineend = "round", linejoin = "round", lwd = 0) +
    geom_path(aes(group = id), lineend = "round", linejoin = "round", lwd = 0) +
    geom_path(data = plotdata_solution, aes(color = id), size = 1, lineend = "round", linejoin = "round") +
    scale_color_gradient(low = "limegreen", high = "purple4") +
    geom_point(data = plotdata_points, aes(shape = d), color = "black") +
    scale_shape_manual(values = c("u" = 2, "d" = 6, "none" = 16)) +
    scale_x_continuous(breaks = 1:l_maze) +
    scale_y_continuous(breaks = 1:w_maze) +
    coord_fixed(ratio = ratio) +
    theme(panel.background = element_rect(fill = NA),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          plot.margin = unit( c(0,0,0,0), "in" ),
          axis.ticks.length = unit(0, "cm"))
}


#' @import utils
#' @import dplyr
#' @importFrom grDevices colorRampPalette

maze_plot_3d <- function(plotdata_walls, plotdata_points, plotdata_solution, l_maze, w_maze, h_maze, ratio, n_sol_path){
  plotdata_walls %>%
    ggplot(aes(x = x, y = y)) +
    geom_path(aes(group = id), lineend = "round", size = 0) +
    geom_path(data = plotdata_solution, aes(group = layer_id, color = factor(id, levels = 0:n_sol_path, ordered = TRUE)), size = 1, lineend = "round", linejoin = "round") +###
    scale_color_manual(values = colorRampPalette(c("limegreen", "purple4"))(n_sol_path)) + ###
    geom_point(data = plotdata_points, aes(shape = d, fill = d), color = "black", alpha = 0.5) + ###
    scale_shape_manual(values = c("u" = 24, "d" = 25, "none" = 21)) + ###
    scale_fill_manual(values = c("u" = "deepskyblue", "d" = "deeppink", "none" = "green")) + ###
    scale_x_continuous(breaks = 1:l_maze) +
    scale_y_continuous(breaks = 1:w_maze) +
    coord_equal() +
    facet_rep_wrap(paste("Level", str_pad(z, width = nchar(h_maze), side = "left", pad = "0")) ~ ., repeat.tick.labels = TRUE) + ###
    theme(panel.background = element_rect(fill = NA),
          axis.text = element_text(color = "gray50"), ###
          axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5), ###
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_line(color = "gray95"), ###
          legend.position = "none") +
    NULL
}


#' Render a Maze object
#'
#' @param maze a Maze object
#' @param show_solution a logical describing whether to plot the solution to the maze
#' @examples
#' a <- generate_maze()
#' render_maze(a, show_solution = TRUE)
#' @export
#' @importFrom stats rbinom
#' @import utils
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
    bw_mask <- maze@mask
    
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
        mutate(layer_id = cumsum(c(0, as.numeric(diff(.$z)) != 0))) %>% 
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
                      z = sort(rep(1:h_maze, 3)),
                      color = rep(rep("black", 3), h_maze)))
    
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
                                 z = c(k, k),
                                 color = rep(
                                   ifelse(
                                     (bw_mask[i, j] == 1 && 
                                        try(bw_mask[i + 1, j], silent = TRUE) == 1 && 
                                        any(try(bw_mask[i + 1, j + 1], silent = TRUE) == 0)#, j == 1)
                                      ) | 
                                       try(bw_mask[i + 1, j], silent = TRUE) == 0, 
                                     "black", "red"), 2))
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
                                 z = c(k, k),
                                 color = rep(
                                   ifelse(
                                     (bw_mask[i, j] == 1 && 
                                        try(bw_mask[i, j + 1], silent = TRUE) == 1 && 
                                        any(try(bw_mask[i + 1, j + 1], silent = TRUE) == 0)#, i == w_maze)
                                      ) | 
                                       try(bw_mask[i, j + 1], silent = TRUE) == 0, 
                                     "black", "red"), 2))
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
      plot_out <- maze_plot(plotdata_walls, plotdata_points, plotdata_solution, l_maze, w_maze, 1)
    } else { # alternate format for mazes with more than 1 floor
      plot_out <- maze_plot_3d(plotdata_walls, plotdata_points, plotdata_solution, l_maze, w_maze, h_maze, 1, n_sol_path)
    }
    
    plot_out
    
  }

#' Render a Triangular Grid Maze object
#'
#' @param maze a Maze object
#' @param show_solution a logical describing whether to plot the solution to the maze
#' @examples
#' generate_maze_tri()
#' @export
#' @importFrom stats rbinom
#' @import utils
#' @import ggplot2
#' @import dplyr
#' @import stringr
#' @returns ggplot of Maze object


render_maze_tri <-
  function(maze, show_solution = FALSE){
    
    w_maze <- maze@width
    l_maze <- maze@length
    h_maze <- maze@height
    maze_array <- maze@array
    exit <- maze@exit
    solution <- maze@solution
    
    # initialize solution df
    plotdata_solution <-
      data.frame(y = c(1, 1), x = c(1, 1), id = c(0, 0))
    
    # build solution df
    if(show_solution) {
      plotdata_solution <-
        solution %>%
        unlist %>%
        matrix(ncol = 2,
               byrow = TRUE,
               dimnames = list(NULL, c("y", "x"))
        ) %>%
        data.frame() %>%
        mutate(across(c(x, y), ~(.x + lag(.x, default = 0)) / 2)) %>% 
        mutate(id = row_number())
    }
    
    # length of solution path
    n_sol_path <-
      pull(plotdata_solution) %>%
      unique %>%
      length
    
    # initialize maze walls list with borders
    wallslist <-
      list(data.frame(id = rep("edge", 2 * w_maze + 3),
                      x = c(l_maze - is.even(l_maze), # bottom right
                            0, # bottom left
                            as.integer(is.odd(1:w_maze)), # zigzag left
                            l_maze + as.integer(is.odd(l_maze + w_maze:0))), # zigzag right
                      y = c(0.5, 
                            0.5, 
                            0.5 + 1:w_maze, 
                            0.5 + w_maze:1,
                            0.5)))
    
    ##### extract coordinates for horizontal walls
    
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, 1] == 1L){
          wallslist <-
            lappend(wallslist,
                    data.frame(id = rep(paste0("i", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0")), 2),
                               x = c(j - 1, j + 1),
                               y = c(i - 0.5, i - 0.5))
            )
        }
      }
    }
    
    ##### extract coordinates for western sloped walls
    
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, 2] == 1L){
          wallslist <-
            lappend(wallslist,
                    data.frame(id = rep(paste0("j", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0")), 2),
                               x = c(j, j + 1),
                               y = c(i + 0.5, i - 0.5))
            )
        }
      }
    }
    
    ##### extract coordinate for eastern sloped walls
    
    ladderlist <- list()
    
    for(i in 1:w_maze){
      for(j in 1:l_maze){
        if(maze_array[i, j, 3] == 1L){
          wallslist <-
            lappend(wallslist,
                    data.frame(id = rep(paste0("k", str_pad(i, width = nchar(w_maze), side = "left", pad = "0"), str_pad(j, width = nchar(l_maze), side = "left", pad = "0")), 2),
                               x = c(j, j - 1),
                               y = c(i + 0.5, i - 0.5))
            )
        }
      }
    }
    
    # define start- and end-points of path
    startenddata <-
      data.frame(id = c("start", "end"),
                 x = c(1, exit[2]),
                 y = c(1, exit[1]),
                 d = c("none", "none"))
    
    plotdata_walls <-
      bind_rows(wallslist)
    
    plotdata_points <-
      bind_rows(ladderlist) %>%
      bind_rows(startenddata)
    
    # create ggplot of maze
    
    plot_out <- maze_plot(plotdata_walls, plotdata_points, plotdata_solution, l_maze, w_maze, ratio = sqrt(3))
    
    plot_out
    
  }



