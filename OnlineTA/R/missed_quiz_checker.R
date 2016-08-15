#' Missed Quiz Checker
#'
#' Use this function to track students who do not complete assignments. If you have a series of similar assignments in a given category, you can use this to identify students who are struggling before it is too late for them to save their grade.
#' @param tracker_path   A path to the file you use to keep track of student progress or the path to where you want to create that file
#' @param output_path   A path to a location where you want the output files saved. Must end with '.csv'.  Output file will include students who have increased their missed assignment count. Tracker file itself will remain in tracker_path location.
#' @param roster  A dataframe (probably created from CLIPS) with a roster and email addresses for all students in class. Must be supplied if the tracker file does not already exist. Make sure that this dataframe has three columns with these exact titles: "Name", "EID", and "Email".
#' @param current_gb  A dataframe that has grades for the most recent assignment in the series being tracked. Do not set a default grade before running this function or it will not work.
#' @param assignment_column_name  The name of the column that contains the new assignment to test

#' @return Outputs two files. The first is an updated version of the tracker file (or creates this if it does not already exist). The second is a file that contains only those entries that have increased their missed assignment number.
#' @export

track_students <- function(tracker_path, output_path, roster, current_gb, assignment_column_name){
  if (file.exists(tracker_path) == F){ #create track dataframe if file does not exist
    track <- roster[,c("Name", "EID", "Email")]
    track$current_missed <- 0
    track$previous_missed <- 0
  } else { #load track file if it exists
    track <- read.csv(file = tracker_path, header = T, sep = ",")
  }
  track$previous_missed <- track$current_missed #update current missed
  if(any(grepl("Muted", c(t(current_gb[1,])))) & any(grepl("Points Possible", current_gb[1:2,1]))){ #remove mute/points possile rows
    top <- c(1,2)
  }else{
    if(any(grepl("Muted", c(t(current_gb[1,])))) | any(grepl("Points Possible", current_gb[1:2,1]))){
      top <- 1
    } else{
      top <- (nrow(current_gb) + 1)
    }
  }
  current_gb <- current_gb[-top,]
  col_number <- grep(assignment_column_name, names(current_gb)) #translate assignment name to column number
  current_gb[,col_number] <- as.numeric(as.character(current_gb[,col_number])) #make sure that desired column is numeric
  gb_eid <- names(current_gb)[3] #get column name on eid column in gb
  track <- dplyr::left_join(track, current_gb[,c(1:5, col_number)], by = c("EID" = gb_eid)) #merge data frames
  t_col_num <- as.numeric(length(names(track)))
  track$current_missed[is.na(track[t_col_num])==T] <- track$current_missed[is.na(track[t_col_num])==T] + 1
  track <- track[,1:5]
  new_missed <- track[which(track$current_missed != track$previous_missed),]
  write.csv(track, file = tracker_path)
  write.csv(new_missed, file = output_path)
}
