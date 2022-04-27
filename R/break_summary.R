#create a function that summarizes player stats (per player)



summary_func <- function(file = NULL){

file <- file%>%
  filter(!is.na(pfx_x), !is.na(pfx_z))

minx = min(file$pfx_x)
maxx= max(file$pfx_x)

minz = min(file$pfx_z)
maxz = max(file$pfx_z)
  file%>%
    group_by(pitch_type)%>%
    summarise(average_x = mean(pfx_x,na.rm=TRUE), average_z = mean(pfx_z,na.rm=TRUE))%>%
    ggplot(aes(x= average_x, y = average_z ,color = pitch_type))+geom_point()+geom_segment(aes( xend=0, yend=0))+xlim(minx,maxx)+ylim(minz,maxz)+xlab("Horizontal Break")+ylab("Verticle Break")+title("Average Break")




}


summary_func(file = deGrom)

