## Peer-graded Assignment: Functional and Object-Oriented Programming
## Part 2: Longitudinal Data Class and Methods


## Write a series of generics and methods for interacting with
## longitudinal data.

library(tidyverse)

make_LD <- function(LD_tibble){
    structure(list(data = LD_tibble),
              class = "LongitudinalData")
}


print.LongitudinalData <- function(LDobj){
    
    number_subjects <-  length(table(factor((LDobj$data)$id)))
    
    cat("Longitudinal dataset with ",
        number_subjects,
        " subjects\n" )
}
              

subject <- function(LD_obj, pt_id){
    data <- LD_obj$data %>%
        filter(`id` == pt_id) %>%
        select(-`id`)
    
    structure(list(data = data,
                   "ID" = pt_id),
              class = "subject")
}


print.subject <-  function(LD_subject){
    if (is.na(LD_subject$data[1,1])) return(NULL)
    else cat("Subject ID: ", LD_subject$ID, "\n")
}


summary.subject <-  function(LD_subject){
    no_na_mean <-
        partial(mean, na.rm=TRUE)
    cat(cat("ID: ", LD_subject$ID, "\n"),
    print(LD_subject$data %>%
        spread(key=room, value=value)%>%
        select(-timepoint) %>%
        group_by(visit) %>%
        summarise_each(funs(no_na_mean))))
}


#A visit is to a single subject.
visit <- function(LD_subject, visit_number){
    data <- LD_subject$data %>%
        filter(`visit` == visit_number) %>%
        select(-`visit`)
    
    structure(list(data = data,
                   "ID" = LD_subject$ID,
                   "Visit" = visit_number),
              class = "visit")
}

##A room belongs to a single subject on a single visit
room <- function(LD_visit, room){
    data <- LD_visit$data %>%
        filter(`room` == room) %>%
        select(-`room`)
    
        structure(list(data = data,
                       "ID" = LD_visit$ID,
                       "Visit" = LD_visit$Visit,
                       "Room" = room),
                  class = "room")
}


print.room <- function(LD_room){
    cat("ID: ", LD_room$ID, "\n")
    cat("Visit: ", LD_room$Visit, "\n")
    cat("Room: ", LD_room$Room, "\n")
}


summary.room <- function(LD_room){
    (LD_room$data)$value  %>% summary
}
