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




## read_fwf requires column names
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                         "hour", "year", "latitude", "longitude",
                         "max_wind", "min_pressure", "rad_max_wind",
                         "eye_diameter", "pressure_1", "pressure_2",
                         paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                         paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                        "storm_type", "distance_to_land", "final")


caine_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3,
                 4, 3, 3, 3, 4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)


caine_columns <- "_cccccdd______dddddddddddd__"


ext_tracks <- read_fwf("./data/Atlantic_1988_2015.txt", 
                      fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                      col_types = caine_columns,
                      na = "-99")

##These are more handy, and I don't feel like re-writing the code that
##uses them.
caine_names <- c("name", "month", "day", "hour", "year",
                "latitude", "longitude_W",
                "34NE", "34SE", "34SW", "34NW",
                "50NE", "50SE", "50SW", "50NW",
                "64NE", "64SE", "64SW", "64NW")

colnames(ext_tracks) <- caine_names


Ike <-  ext_tracks %>%
    filter(name == "IKE", year == "2008") %>%
    mutate(date = ymd_hm(paste(year,  "/",
                               month, "/",
                               day,   "/",
                               hour,   sep="")),
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


##Returns a single observation needed for the geom.
single_obs <- function(datetime){
    cleanIke %>% filter(date == datetime)
    }
