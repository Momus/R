#' Maping wind speeds of a hurricane.
library(tidyverse)
library(lubridate)
library(data.table)

#' Download the dataset
#' http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/

URL <- "http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/data/ebtrk_atlc_1988_2015.txt"

raw_file <- "./data/Atlantic_1988_2015.txt"

download_hurricane_data <- function(){
    if (!(file.exists("./data/Atlantic_1988_2015.txt"))) {
        download.file(url = URL, destfile = raw_file)
    }
}


## This datafile is kind of messed up: some of the windspeeds
## aren't sperated by anyting, thus get sucked into one column with
## NA's for the others.

## To write a general function the solution would be to look for rows
## with NA's than massage those values. However,I don't have time, so
## I cut and edited the data I needed directly.


caine_columns <- "_cccdd______dddddddddddd__"


## Names form the document file
## http://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/docs/ebtrk_readme.txt

caine_names <- c("name",
                "MDT",
                "year",
                "latitude",
                "longitude_W",
                "34NE",
                "34SE",
                "34SW",
                "34NW",
                "50NE",
                "50SE",
                "50SW",
                "50NW",
                "64NE",
                "64SE",
                "64SW",
                "64NW")

##Get the storm we need. Sadly, this was actually done in Emacs.
#Ike <- filter(firstCaine, name == "IKE", year == "2008")


Ike <- read_table2("../data/Ike.txt",
                  col_names = caine_names,
                  col_types = caine_columns)

#This didn't generate any NA values

datedIke <- Ike %>%
    mutate(date = ymd_hm(paste(year, "/",substring(MDT,1,2), "/" ,
                               substring(MDT,3,4), "/",
                               substring(MDT,5,6), "00", sep="")),
           longitude = -(longitude_W)) %>%
    unite("storm_id",
          c(name, year),
          sep="-")

##If I wanted to write a more generalized function one day, I could
##use: x <- function(y){print(match.call())[2]}


NEIke <- select(datedIke, storm_id, date, latitude, longitude,
                `34` = `34NE`, `50` = `50NE`,  `64` = `64NE`) %>%
    gather(`34`, `50`, `64`, key="wind_speed", value="NE")


SEIke <- select(datedIke, storm_id, date, latitude, longitude,
               `34` = `34SE`, `50` = `50SE`, `64` = `64SE`) %>%
    gather(`34`, `50`, `64`, key="wind_speed", value="SE")


NWIke <- select(datedIke, storm_id, date, latitude, longitude,
               `34` = `34NW`, `50` = `50NW`, `64` = `64NW`) %>%
    gather(`34`, `50`, `64`, key="wind_speed", value="NW")


SWIke <- select(datedIke, storm_id, date, latitude, longitude,
               `34` = `34SW`, `50` = `50SW`, `64` = `64SW`) %>%
    gather(`34`, `50`, `64`, key="wind_speed", value="SW")


cleanIke <- NEIke %>%
    full_join(NWIke) %>%
    full_join(SEIke) %>%
    full_join(SWIke)


