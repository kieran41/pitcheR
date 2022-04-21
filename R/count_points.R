#' Counts Points in cell geom
#'
#'
#' @export
#' @import ggplot2
#'


count_points<-function(file = NULL){

cell.geom<-create_grid(file = file)[[1]]
dim.x<-create_grid(file = file)[[2]]
dim.y<-create_grid(file = file)[[3]]
x<-create_grid(file = file)[[4]]
y<-create_grid(file = file)[[5]]

counts <- matrix(1:dim.y*dim.x, nrow = dim.y, ncol = dim.x)

locations <- matrix(c(file$plate_x, file$plate_z), nrow = length(file$plate_x), ncol = 2, byrow=F)

for(i in 1:dim.x){
  for(j in 1:dim.y){
    counts[dim.y-(j-1),i] =sum(  (locations[,1]> cell.geom[i,j,1,1,1])&
                                   (locations[,1]< cell.geom[i,j,1,2,1])&
                                   (locations[,2]> cell.geom[i,j,1,1,2])&
                                   (locations[,2]< cell.geom[i,j,1,2,2])   )

    }
  }

  return(list(counts,x,y))

}
