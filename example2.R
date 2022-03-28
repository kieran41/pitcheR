library(ggplot2)
library(ggalt)
library(dplyr)


source("R/add_zone.R")
source("R/create_grid.R")
source("R/count_points.R")
source("R/location_heatmap.R")
source("R/contact_location_heatmap.R")

deGrom<- read.csv("inst/extdata/deGrom.csv")
wheeler <- read.csv("inst/extdata/WheelerZack.csv")

function.matrix.W <- count_points(file=wheeler, sq=5 )
function.matrix.D <- count_points(file=deGrom, sq=1 )




grid<- create_grid(file=deGrom, sq = 1)[[1]]

locations <- matrix(c(file$plate_x, file$plate_z), nrow = length(file$plate_x), ncol = 2, byrow=F)



ggplot(aes(x=X,y=Y ), data=grid)+
  geom_point()+
  geom_point(aes(x=plate_x,y=plate_z, col=ifelse(zone %in% 1:9,"green","red") ), data=file)+
  add_zone()



#Defining cells

sq=1

#Creating the grid coordinates
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
y<- seq(from=2.55-unit.y*num.y.min-(unit.y/2), to=unit.y*num.y.max+(unit.y/2), by=unit.y)
grid <-expand.grid(X=x, Y=y)


grid.min.x <- min(grid$X)
grid.min.y <- min(grid$Y)

dim.x <- num.x.max+num.x.min
dim.y <- num.y.max+num.y.min -1


cell.geom<-array(data=NA,dim=c(dim.x,dim.y, 2,2,2))


cell.geom[1,1,,,] = array(data=1:8, dim=c(2,2,2))

cell.geom[1,1,,,]

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

cell.geom[,,,,]

cell.geom[1,1,,,]

cell.geom[1,1,1,,]

cell.geom[1,1,1,1,]
cell.geom[1,1,1,2,]

cell.geom[1,1,1,1,1]
cell.geom[1,1,1,2,1]


#Count points

counts <- matrix(1:20, nrow = dim.y, ncol = dim.x)

for(i in 1:dim.x){
  for(j in 1:dim.y){
    counts[dim.y-(j-1),i] =sum(  (locations[,1]> cell.geom[i,j,1,1,1])&
                       (locations[,1]< cell.geom[i,j,1,2,1])&
                       (locations[,2]> cell.geom[i,j,1,1,2])&
                       (locations[,2]< cell.geom[i,j,1,2,2])   )

  }
}



