#' Creates tile layout for heatmaps
#'
#'
#' @export
#' @import ggplot2
#'

globalVariables(c("pfx_x", "pfx_z", "pitch_type", "average_x", "average_z"))

create_grid<-function(file= NULL){

  #Creating the grid coordinates
  unit.x<-(.85--.85)/3
  unit.y<-(3.5-1.6)/3

  max.x <- max(file$plate_x)
  max.y <- max(file$plate_z)

  min.x <- min(file$plate_x)
  min.y <- min(file$plate_z)

  num.x.max <- (max.x %/% unit.x)+1
  num.y.max <- (max.y %/% unit.y)+1

  num.x.min <- abs((min.x %/% unit.x))
  num.y.min <- abs(((2.55-min.y) %/% unit.y)+1)

  x<- seq(from=-unit.x*num.x.min+(unit.x/2), to=unit.x*(num.x.max)-(unit.x/2), by=unit.x)
  y<- seq(from=2.55-unit.y*num.y.min+(unit.y/2), to=unit.y*(num.y.max+1)+(unit.y/2), by=unit.y)
  grid <-expand.grid(X=x, Y=y)

  #Defining the grid cells

  grid.min.x <- min(grid$X)
  grid.min.y <- min(grid$Y)

  dim.x <- num.x.max+num.x.min -1
  dim.y <- num.y.max+num.y.min -3


  cell.geom<-array(data=NA,dim=c(dim.x,dim.y, 2,2,2))


  for(i in 1:dim.x){
    for(j in 1:dim.y){
      cell.geom[i,j,,,] = array(data=c(grid.min.x+(i-1)*unit.x,
                                       grid.min.x+(i-1)*unit.x,
                                       grid.min.x+(i)*unit.x,
                                       grid.min.x+(i)*unit.x,
                                       grid.min.y+(j-1)*unit.y,
                                       grid.min.y+(j-1)*unit.y,
                                       grid.min.y+(j)*unit.y,
                                       grid.min.y+(j)*unit.y)
                                , dim=c(2,2,2))
    }
  }


  return(list(cell.geom, dim.x, dim.y, x,y))
}

