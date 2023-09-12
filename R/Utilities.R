#' @import methods

setClass(
  "Maze",
  slots = c(
    width = "numeric",
    length = "numeric",
    height = "numeric",
    array = "array",
    exit = "vector",
    solution = "list",
    tile = "character",
    mask = "array"
  ),
  prototype = list(
    width = NA_real_,
    length = NA_real_,
    height = NA_real_,
    array = array(),
    exit = c(NA_real_, NA_real_, NA_real_),
    solution = NULL,
    tile = "square",
    mask = array()
  )
)

setMethod("show", signature(object = "Maze"), function(object){
  if(object@tile == "square"){
    print(render_maze(object))
  }else{
    print(render_maze_tri(object))
  }
})


# Append List
lappend <-
  function (lst, ...){ # function borrowed from stack
    lst <- c(lst, list(...))
    return(lst)
  }

is.odd <- function(x){
  x %% 2 == 1
}

is.even <- function(x){
  x %% 2 == 0
}

#' @importFrom magrittr equals

curler2 <-
  function(base_weights, curl, curl_weight){
    n_dir <- length(base_weights)
    pmax(base_weights + 
           cos(curl * pi - seq(-pi, pi, length.out = n_dir + 2)[-c(1, n_dir + 2)]) * 
           curl_weight * 
           !equals(base_weights, 0L),
         0 + 1e-6 * !equals(base_weights, 0L))
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