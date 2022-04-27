library(ggplot2)
library(ggalt)
library(dplyr)
library(baseballr)
library(reshape2)
library(devtools)
library(stringr)
library(lubridate)


source("R/add_zone.R")
source("R/create_grid.R")
source("R/count_points.R")
source("R/location_heatmap.R")
source("R/contact_location_heatmap.R")
source("R/swg_strike.R")
source("R/velo_time.R")
source("R/custom_heatmap.R")
source("R/velo_by_pitch_type.R")
source("R/break_summary.R")

devtools::install_github("BillPetti/baseballr")

#### Read Data ###
load(file='data/deGrom.rda')
load(file='data/wheeler.rda')

names(deGrom)[1] <- "pitch_type"
names(wheeler)[1] <- "pitch_type"

velo_time(file = deGrom)

swg_strike(file= wheeler)

location_heatmap(file = wheeler)

contact_location_heatmap(file = wheeler)

custom_heatmap(file = deGrom)

velo_bp(file = wheeler)

break_func(file = wheeler)

break_func(file = deGrom)

wheeler %>%
  group_by(pitch_type) %>%
  summarize(n = n())


ohtani420 <- read.csv("C:/Users/Kieran/Downloads/savant_data (3).csv", header=T)

names(ohtani420)[1]<- "pitch_type"

ohtani420.splitter <- ohtani420 %>%
  filter(pitch_type == "SL")

ohtani420.splitter$plate_z

custom_heatmap(ohtani420.splitter)

