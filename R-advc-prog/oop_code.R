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
    
    summaryout <- LD_subject$data %>%
        spread(key=room, value=value)%>%
        select(-timepoint) %>%
        group_by(visit) %>%
        summarise_each(funs(no_na_mean))

    ## We still need to mangle it some more in its very own print
    ## function, so let's give it some class, and let it remember it's ID
    class(summaryout) <- append(class(summaryout)
                             , "SubjectSummary"
                             , after=0) # New class must be at front
                                        # of vector to use its special
                                        # function.

    attr(summaryout,"ID") <- LD_subject$ID
    
    summaryout
}


##Mangle output to look like assignment wants
print.SubjectSummary <- function(LD_SubjectSummary){
    
    ## Replace NaN's with NA's
    LD_SubjectSummary[sapply(LD_SubjectSummary, is.na)] = NA
    
    ## Print it like the model
    cat("ID: ", attr(LD_SubjectSummary, "ID"), "\n")
    print.data.frame(LD_SubjectSummary)
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
room <- function(LD_visit, room_name){
    new_data <- (LD_visit$data %>%
                filter(`room` == room_name) %>%
                select(-`room`))
    
    structure(list(data = new_data,
                       "ID" = LD_visit$ID,
                       "Visit" = LD_visit$Visit,
                       "Room" = room_name),
                  class = "room")
}


print.room <- function(LD_room){
    cat("ID: ", LD_room$ID, "\n")
    cat("Visit: ", LD_room$Visit, "\n")
    cat("Room: ", LD_room$Room, "\n")
}


summary.room <- function(LD_room){
    room_out <- LD_room
    room_out$data <- (room_out$data)$value  %>% summary
    class(room_out) <- append(class(room_out)
                             , "RoomSummary"
                             , after=0)
    room_out
}


print.RoomSummary <- function(RoomSummary){
    cat("ID: ", RoomSummary$ID, "\n")
    RoomSummary$data
}
