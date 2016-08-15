#' Studio Attendance Randomization Function
#'
#' Use this function to randomize students to invite to the studio for lecture.
#' @details This function facilitates randomizing studio attendance and invititations for the online class. Create a poll in piazza asking students to complete it in order to add their name to the attendance drawing list.  (The poll can have a single option - "Yes, I'd like to attend")  Download the results of that poll and open the file in R.  Then process it using this function, which will place a CSV on your desktop with a list of 50 names, 25 for each day of the week, along with the student's email address.  This should facilitate contacting the students with directions for attending the studio.
#' @param piazza_survey A dataframe of the CSV file produced by the Piazza poll. Will not work with other data sources unless modified. See note in code if necessary.
#' @param days A listing of the days of the week for the course.  Default is Monday and Wednesday.
#' @param student_count The number of students you would like returned for each class day
#' @param ignore_list A vector of names that you want to exclude from the randomization
#' @return Dataframe with three columns: Name, email address, and day of week they should be invited.
#' @export


random_attendance <- function(piazza_survey, days=c("Monday", "Wednesday"), student_count = 25, ignore_list = c()) {
  attendance <- piazza_survey[1:(nrow(piazza_survey)-3),] #modify this line for alternative data sources
  #remove students from ignore list if present
  if(length(ignore_list > 0)){
    ignore_row <- c()
    for(i in 1:length(ignore_list)){
      ignore_row <- append(ignore_row, grep(ignore_list[i], attendance$name))
    }
    attendance <- attendance[-ignore_row,]
  }
  if(nrow(attendance) < 2*student_count){
    warning("You only have ", nrow(attendance), " student responses to the survey. Please reduce student_count or reconsider if you really need this function.")
    stop
  }
  shuffled <- attendance[sample(nrow(attendance)),]
  shuffled <- shuffled[1:2*student_count,1:2]
  shuffled$day[1:student_count] <- days[1]
  shuffled$day[student_count+1:student_count*2] <- days[2]
  write.csv(shuffled, file="~/Desktop/randomized_attendance_list.csv")
}
