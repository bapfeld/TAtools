#' Registrar Formatted Final Grades
#'
#' Use this function if you need to adjust final grades based on an alternative point scheme. Useful for the Shaw/McDaniel version of 310.
#' @param gradebook A dataframe of the final gradebook downloaded from Canvas.
#' @param class A character string of the abbreviated class name.  Default is GOV310L.  "GOV" must be in capital letters.
#' @param non.enrolled A vector of character strings indicating the non-enrolled EIDs for the course.  Defaults are for speede, Tester McStudent, and Student McStudent.  Registrar allows no more than 3 non-enrolled eids in a course for the upload to succeed, so it may be necessary to add in student workers as well.
#' @param points A vector of length 11 indicating the LOWER point values for each grade break A through D-.  Use only integers- the function will take care of rounding. Breaks do not need to be any particular order.
#' @param round Should grades be rounded? If set to "normal", then the final point total rounds according to standard mathematical rules and assigns letters based on that rounded score. If set to "floor," then final grades are based on only the whole number. I.e. an 88.97 is treated as an 88 and not an 89.
#' @param full.out Should the TA/professor copy outputted include the entire gradebook or only the registrar version plus final points? Default is TRUE.
#' @param final Should the "Final Points" or "Final Score" column be used to assign letter grades? Set to either "points" or "score".
#' @return Outputs two files to a new folder in the working directory: "FinalGradeSubmission.txt" is the file that can be uploaded to the registrar; "Final Grades - Letters and Numbers.csv" is a copy for TA/professor records that includes point totals next to the grade letters.  Folder created is named "Final Grades -- Registrar Formatting"
#' @examples
#' final.grades(gradebook.file, class="GOV312L", points=c(487, 462, 451, 430, 412, 400, 392, 386, 350, 330, 312), final = "points")
#' @export

final.grades <- function(gradebook, class = "GOV310L", non.enrolled = c("speede", "sm56684", "tm29778"), points = c(94, 90, 87, 84, 80, 77, 74, 70, 67, 64, 60), round = TRUE, drop_decimal = FALSE, full.out = TRUE, final = c("points", "score")){
  #make sure final is set correctly
  if(length(final) > 1){
    stop("You must choose either 'points' or 'score' for the final variable.")
  }
  #check length of points vector
  if (length(points)!=11) stop("Points vector not of length 11")
  # clean the gb
  clean_gb <- gradebook_clean(gradebook = gradebook, non_enrolled = non.enrolled)
  gradebook <- clean_gb[[1]]
  #Are there any muted assignments? or the points possible row?
  if(clean_gb[[3]] == T){
    stop("Muted assignments detected - failed to export final grades.\n Muted assignments are not included in final grade calculations by Canvas. If you are confident your final points column is correct even with the muted assignments, you may ignore this message by deleting the muted assignment row (likely row 1) before you pass the gradebook to this function.")
  }

  #column for unique number
  gradebook$unique <- as.numeric(substr(gradebook$Section, 9, 13))

  # Creating a vector of scores based on points or score
  if(final %in% c("points", "point", "Points", "Point", "p", "P")){
    gradebook$final_num <- gradebook$`Final Points`
  }else if(final %in% c("score", "scores", "Score", "score", "S", "s")){
    gradebook$final_num <- gradebook$`Final Score`
  }else{
    stop("You must set parameter 'final' to either 'points' or 'score' to indicate which column to use to assign final grades.\n Final grades not exported.")
  }

   ## Dealing with grades
  points <- sort(c(points, Inf, 0), decreasing=F)
  if(drop_decimal == TRUE){
    gradebook$Grade_Based_on_Points <- as.numeric(substr(gradebook$`Final Points`, 1,3))
    gradebook$letter <- num_to_letter(gradebook$Grade_Based_on_Points, cutpoints = points, round = F)
  }

  if(round == TRUE){
    gradebook$Grade_Based_on_Points <- round(as.numeric(gradebook$`Final Points`))
    gradebook$letter <- num_to_letter(gradebook$Grade_Based_on_Points, cutpoints = points, round = T)
  } else{
    gradebook$Grade_Based_on_Points <- as.numeric(gradebook$`Final Points`)
    gradebook$letter <- num_to_letter(gradebook$Grade_Based_on_Points, cutpoints = points, round = F)
  }

  #Add columns for absences & remarks
  gradebook$absences <- NA
  gradebook$remarks <- NA

  #Keep only necessary columns
  if (full.out == TRUE){
    grades <- gradebook
  } else if(final %in% c("points", "point", "Points", "Point", "p", "P")){
    grades <- subset(gradebook, select = c("Student", "SIS User ID", "letter", "absences", "remarks", "unique", "Final Points"))
  } else{
    grades <- subset(gradebook, select = c("Student", "SIS User ID", "letter", "absences", "remarks", "unique", "Final.Score"))
  }
  gradebook <- subset(gradebook, select=c("Student", "SIS User ID", "letter", "absences", "remarks", "unique"))
  #rename to match registrar requirements
  colnames(gradebook)<- c("Name", "EID", "Grade", "Absences", "Remarks", "Unique")
  #export the results
  dir.create("Final Grades -- Registrar Formatting")
  write.table(gradebook, file = paste(getwd(), "Final Grades -- Registrar Formatting", "FinalGradeSubmission.txt", sep="/"), sep = "\t", row.names = F, na = "")
  write.csv(grades, file = paste(getwd(), "Final Grades -- Registrar Formatting", "Final Grades - Letters and Numbers.csv", sep = "/"))
}
