library(ggplot2)
library(ggalt)
library(dplyr)
library(baseballr)
library(reshape2)
library(devtools)


source("R/add_zone.R")
source("R/create_grid.R")
source("R/count_points.R")
source("R/location_heatmap.R")
source("R/swg_strike.R")
source("R/velo_time.R")
source("R/custom_heatmap.R")

devtools::install_github("BillPetti/baseballr")



names(deGrom)[1] <- "pitch_type"
names(wheeler)[1] <- "pitch_type"


velo_time(pitcher_name=deGrom)

swg_strike(file= wheeler)

location_heatmap(file = wheeler)

contact_location_heatmap(file = wheeler)

custom_heatmap(file = deGrom)


ohtani420 <- read.csv("C:/Users/Kieran/Downloads/savant_data (3).csv", header=T)

names(ohtani420)[1]<- "pitch_type"

ohtani420.splitter <- ohtani420 %>%
  filter(pitch_type == "SL")

ohtani420.splitter$plate_z

custom_heatmap(ohtani420.splitter)

