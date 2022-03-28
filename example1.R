
library(ggplot2)
library(ggalt)
library(dplyr)


source("R/add_zone.R")
source("R/location_heatmap.R")
source("R/contact_location_heatmap.R")

deGrom<- read.csv("inst/extdata/deGrom.csv")


Wheeler<- read.csv("inst/extdata/WheelerZack.csv")

names(Wheeler)[1] <- "pitch_type"

names(deGrom)[1] <- "pitch_type"

dg.contact <- deGrom %>%
  filter(description == "hit_into_play")

unique(deGrom$events)

topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.85
outKzone <- 0.85
define.zone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

deGrom %>%
  group_by(pitch_type) %>%
  ggplot(aes(x= plate_x, y= plate_z))+
  geom_hex(bins = 10)+
  add_zone()+
  facet_wrap(~pitch_type)


location_heatmap(file = Wheeler, by_pitch_type = F)
contact_location_heatmap(file = Wheeler, by_pitch_type = T, hit.status = F)



