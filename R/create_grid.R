#' Creates tile layout for heatmaps
#'
#'
#' @export
#' @import ggplot2
#'


create_grid<-function(file= NULL, sq=5){


  unit.x<-(.85--.85)/sq
  unit.y<-(3.5-1.6)/sq

  max.x <- max(file$plate_x)
  max.y <- max(file$plate_z)

  min.x <- min(file$plate_x)
  min.y <- min(file$plate_z)

  num.x.max <- (max.x %/% unit.x)+1
  num.y.max <- (max.y %/% unit.y)+1

  num.x.min <- abs((min.x %/% unit.x)-1)
  num.y.min <- abs(((2.55-min.y) %/% unit.y)+1)

  x<- seq(from=-unit.x*num.x.min+(unit.x/2), to=unit.x*num.x.max+(unit.x/2), by=unit.x)
  y<- seq(from=2.55-unit.y*num.y.min+(unit.y/2), to=unit.y*num.y.max+(unit.y/2), by=unit.y)
  grid <-expand.grid(X=x, Y=y)
  return(grid)
}

