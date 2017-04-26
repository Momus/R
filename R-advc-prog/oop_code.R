## Peer-graded Assignment: Functional and Object-Oriented Programming
## Part 2: Longitudinal Data Class and Methods


## Write a series of generics and methods for interacting with
## longitudinal data.

library(tidyverse)

make_LD <- function(LD_tibble){
    structure(list(data = LD_tibble),
              class = "LongitudinalData")
}
              

subject <- function(LD_obj, pt_id){
    
    data <- LD_obj$data %>%
        filter(`id` == pt_id) %>%
        select(-`id`)
    
    structure(list(data = data,
                   "ID" = pt_id),
              class = "subject")
}

#A visit is to a single subject.
visit <- function(LD_subject, number){
    structure(class = "visit")
}

#A room belongs to a single subject on a single visit
room <- function(LD_visit, room){
    structure(class = "room")
}


print.LongitudinalData <- function(LDobj){

    number_subjects <-  length(table(factor((LDobj$data)$id)))
    
    cat("Longitudinal dataset with ",
        number_subjects,
        " subjects\n" )
}


print.subject <-  function(LD_subject){
    #If first row,column is blank, object is empty
    if (is.na(LD_subject$data[1,1])) return(NULL)
    else cat("Subject ID: ", LD_subject$ID, "\n")
}

summary.subject <-  function(LD_subject){
    
}



subject(x, 54) %>% summary


room("bedroom")
> print(out)


room("bedroom") %>% summary

