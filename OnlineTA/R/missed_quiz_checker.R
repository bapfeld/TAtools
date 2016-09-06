#' Missed Quiz Checker
#'
#' Use this function to track students who do not complete assignments. If you have a series of similar assignments in a given category, you can use this to identify students who are struggling before it is too late for them to save their grade.
#' @param tracker_path   A path to the file you use to keep track of student progress or the path to where you want to create that file
#' @param output_path   A path to a location where you want the output files saved. Must end with '.csv'.  Output file will include students who have increased their missed assignment count. Tracker file itself will remain in tracker_path location.
#' @param roster  A dataframe (probably created from CLIPS) with a roster and email addresses for all students in class. Must be supplied if the tracker file does not already exist. Make sure that this dataframe has three columns with these exact titles: "Name", "EID", and "Email".
#' @param current_gb  A dataframe that has grades for the most recent assignment in the series being tracked. Do not set a default grade before running this function or it will not work.
#' @param assignment_column_name  The name of the column that contains the new assignment to test

#' @return Outputs two files. The first is an updated version of the tracker file (or creates this if it does not already exist). The second is a file that contains only those entries that have increased their missed assignment number.
#' @return Optionally, will output a third file if the function detects that your gradebook file contains new students not included in the tracker file.
#' @export

track_students <- function(tracker_path, output_path, roster, current_gb, assignment_column_name){
  if (file.exists(tracker_path) == F){ #create track dataframe if file does not exist
    track <- roster[,c("Name", "EID", "Email")]
    track$current_missed <- 0
    track$previous_missed <- 0
  } else { #load track file if it exists
    track <- read.csv(file = tracker_path, header = T, sep = ",")
    track <- track[, 2:6]
  }
  track$previous_missed <- track$current_missed #update current missed
  # remove unnecessary top lines from gb if present
  elim <- c()
  if(mute_check(current_gb) == T){
    elim <- 1
  }
  if(points_possible_check(current_gb) == T){
    elim <- c(elim, 2)
  }
  elim_stud <- test.student(df=current_gb)
  current_gb <- current_gb[-c(elim, elim_stud),]

  col_number <- grep(assignment_column_name, names(current_gb)) #translate assignment name to column number
  current_gb[,col_number] <- as.numeric(as.character(current_gb[,col_number])) #make sure that desired column is numeric
  gb_eid <- names(current_gb)[3] #get column name on eid column in gb
  track <- dplyr::full_join(track, current_gb[,c(1:5, col_number)], by = c("EID" = gb_eid)) #merge data frames
  new_stud <- which(is.na(track$previous_missed))
  if(length(new_stud) > 0 & length(new_stud) < nrow(track)){
    track$previous_missed[new_stud] <- 0
    new_stud_df <- track[new_stud,1:4]
    totl_missed_est <- grep(substr(assignment_column_name, 1, 3), names(track))
    warning(paste("New students detected! List of students will be written to ", getwd()),"\nYou will need to edit the tracking file by hand to accurately reflect missed assignments for these students.", "\nI estimate there are ", paste(totl_missed_est, "missed assignments, but this may not be correct.", sep = " "))
    write.csv(new_stud_df, file = "New_Students_Since_Last_Assignment.csv")
  }
  t_col_num <- as.numeric(length(names(track)))
  track$current_missed[is.na(track[t_col_num])==T] <- track$current_missed[is.na(track[t_col_num])==T] + 1
  track <- track[,1:5]
  new_missed <- track[which(track$current_missed != track$previous_missed),]
  write.csv(track, file = tracker_path)
  write.csv(new_missed, file = output_path)
}

# could re-write to include option for counting 0s instead of NAs since that's what you would expect for non-quiz activities
