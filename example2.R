library(ggplot2)
library(ggalt)
library(dplyr)


source("R/add_zone.R")
source("R/create_grid.R")
source("R/count_points.R")
source("R/location_heatmap.R")
source("R/contact_location_heatmap.R")

source("R/custom_heatmap.R")

deGrom<- read.csv("inst/extdata/deGrom.csv")
wheeler <- read.csv("inst/extdata/WheelerZack.csv")

names(deGrom)[1] <- "pitch_type"

names(wheeler)[1] <- "pitch_type"

location_heatmap(wheeler)

degrom.no.cut <- deGrom %>%
  filter(pitch_type != "CU")

graphics.degrom<-custom_heatmap(file = deGrom , sq=1)

graphics.degrom[[1]]
graphics.degrom[[2]]
graphics.degrom[[3]]
graphics.degrom[[4]]


deGrom %>%
  group_by(pitch_type) %>%
  summarize(n=n())


deGrom_list <- split(deGrom, f = deGrom$pitch_type)

deGrom_list <- deGrom_list[-2]

loc.matrix<-list()
location.information <- list()
x.list <- list()
y.list<- list()


for(i in 1:length(deGrom_list)){
  location.information[[i]] <- count_points(file = deGrom_list[[i]], sq=3)
}

for(i in 1:length(location.information)){
  loc.matrix[[i]] <- location.information[[i]][[1]]
  x.list[[i]] <- location.information[[i]][[2]]
  y.list[[i]]  <- location.information[[i]][[3]]
}

x.list[[1]][1]

x.update[[1]]<-(x.list[[1]][1]+x.list[[1]][1+1])/2

x.update<-list()
temp <- list()
for(i in 1:length(x.list)){
  for(j in 1:length(x.list[[i]])-1){
    temp[j] <- (x.list[[i]][j]+x.list[[i]][j+1])/2

  }
  x.update[[i]] <- temp
}


y.update<-list()
temp <- list()
for(i in 1:length(y.list)){
  for(j in 1:length(y.list[[i]])-1){
    temp[j] <- (y.list[[i]][j]+y.list[[i]][j+1])/2

  }
  y.update[[i]] <- temp
}

data<-list()
count.order <- list()

for(i in 1:length(deGrom_list)){
data[[i]] <-expand.grid(X=x.update[[i]], Y=y.update[[i]])
count.order[[i]] <- as.vector(t(loc.matrix[[i]][c(dim(loc.matrix[[i]])[1]:1),]))

}

length(count.order[[2]])
data[[2]]

for(i in 1:length(count.order)){
  data[[i]]$count <- count.order[[i]]
}


data[[1]]$count <- count.order[[1]]

data$count <- count.order

low.color <- "blue"
high.color <- "red"

df1 <- data[[1]]

data[[2]]

df1 <- as.data.frame(data[[1]])
unlist(df1$X)


graphic.test <- list()
for(i in 1:length(data)){
graphic.test[[i]]<-data[[i]] %>% ggplot()+
  geom_tile(aes(x=unlist(X),y = unlist(Y), fill = count))+
  scale_fill_gradient(low=low.color, high=high.color) +
  labs(title = "Location Heatmap by Pitch",
       x = "Location from Catcher's Perspective (ft)",
       y = "Location off the Ground (ft)")+
  add_zone()
}

graphic.test[[3]]


data <-expand.grid(X=x.update, Y=y.update)
count.order <- as.vector(t(loc.matrix[c(dim(loc.matrix)[1]:1),]))

dim(loc.matrix)[1]-length(y.update)

data$count <- count.order



graphic.test<-ggplot(data)+
  geom_tile(aes(X,Y, fill = count))+
  scale_fill_gradient(low=low.color,high=high.color) +
  add_zone()

location.information[2]


location_heatmap(file=wheeler, by_pitch_type = F, sq =15)

location.information <- count_points(file = wheeler, sq=12)
loc.matrix <- location.information[[1]]
x.list <- location.information[[2]]
y.list <- location.information[[3]]

x.update<-c()
for(i in 1:length(x.list)-1){
  x.update[i] <- (x.list[i]+x.list[i+1])/2
}

y.update<-c()
for(i in 1:length(y.list)-1){
  y.update[i] <- (y.list[i]+y.list[i+1])/2
}


data <-expand.grid(X=x.update, Y=y.update)
count.order <- as.vector(t(loc.matrix[c(dim(loc.matrix)[1]:1),]))

dim(loc.matrix)[1]-length(y.update)

data$count <- count.order



ggplot(data)+
  geom_tile(aes(X,Y, fill = count, size = 2))+
  add_zone()




max(deGrom$plate_x)
min(deGrom$plate_z)


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



