library(ggplot2)
library(ggalt)
library(dplyr)


source("R/add_zone.R")
source("R/create_grid.R")
source("R/location_heatmap.R")
source("R/contact_location_heatmap.R")

deGrom<- read.csv("inst/extdata/deGrom.csv")

plot(create_grid(file = deGrom))

file = deGrom

max.x <- max(file$plate_x)
max.y <- max(file$plate_z)

min.x <- min(file$plate_x)
min.y <- min(file$plate_z)

num.x.max <- (max.x %/% .34)+1
num.y.max <- (max.y %/% .38)+1

num.x.min <- abs((min.x %/% .34)-1)
num.y.min <- abs(((2.55-min.y) %/% .38)+1)

x<- seq(from=-.34*num.x.min+.17, to=.34*num.x.max+.17, by=.34)
y<- seq(from=2.55-.38*num.y.min+.19, to=.38*num.y.max+.19, by=.38)
grid <-expand.grid(X=x, Y=y)


grid<- create_grid(file=deGrom, sq = 11)

ggplot(aes(x=X,y=Y ), data=grid)+
  geom_point()+
  geom_point(aes(x=plate_x,y=plate_z, col=ifelse(zone %in% 1:9,"green","red") ), data=file)+
  add_zone()




locations <- matrix(c(file$plate_x, file$plate_z), nrow = length(file$plate_x), ncol = 2, byrow=F)


plot(grid, col="red")
points(locations)

ggplot(aes(x=plate_x,y=plate_z ), data=file)+
  geom_bin2d()+
  add_zone()



