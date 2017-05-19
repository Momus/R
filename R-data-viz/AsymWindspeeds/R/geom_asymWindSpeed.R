#' Maping wind speeds of a hurricane.
library(tidyvese)
library(lubridate)

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


Ike <- read_table2("./data/Ike.txt",
                  col_names = caine_names,
                  col_types = caine_columns)

#This didn't generate any NA values


mutate(Ike,
       date = ymd_hm( paste(year,
                            "/",
                            substring(MDT,1,2),
                            "/" ,
                            substring(MDT,3,4),
                            "/", substring(MDT,5,6),
                            "00",
                            sep="")))
