#' Registrar Formatted Final Grades
#'
#' Use this function if you need to adjust final grades based on an alternative point scheme. Useful for the Shaw/McDaniel version of 310.
#' @param gradebook A dataframe of the final gradebook downloaded from Canvas.
#' @param class A character string of the abbreviated class name.  Default is GOV310L.  "GOV" must be in capital letters.
#' @param non.enrolled A vector of character strings indicating the non-enrolled EIDs for the course.  Defaults are for speede, Tester McStudent, and Student McStudent.  Registrar allows no more than 3 non-enrolled eids in a course for the upload to succeed, so it may be necessary to add in student workers as well.
#' @param points A vector of length 11 indicating the LOWER point values for each grade break A through D-.  Use only integers- the function will take care of rounding. Breaks do not need to be any particular order.
#' @param round Should grades be rounded? If TRUE, then the final point total rounds according to standard mathematical rules and assigns letters based on that rounded score. If FALSE, then no rounding is performed.
#' @param drop_decimal Logical depending on whether final grades should be based on only the whole number and drop any decimal values. I.e. an 88.97 is treated as an 88 and not an 89. This is an unusual setting. Defaults to FALSE. If set to TRUE, round must be FALSE.
#' @param full.out Should the TA/professor copy outputted include the entire gradebook or only the registrar version plus final points? Default is TRUE.
#' @return Outputs two files to a new folder in the working directory: "FinalGradeSubmission.txt" is the file that can be uploaded to the registrar; "Final Grades - Letters and Numbers.csv" is a copy for TA/professor records that includes point totals next to the grade letters.  Folder created is named "Final Grades -- Registrar Formatting"
#' @examples
#' final.grades(gradebook.file, class="GOV312L", points=c(487, 462, 451, 430, 412, 400, 392, 386, 350, 330, 312))
#' @export


final.grades <- function(gradebook, class="GOV310L", non.enrolled=c("speede", "sm56684", "tm29778"), points=c(1:11), round = TRUE, drop_decimal = FALSE, full.out=TRUE){
  #check length of points vector
  if (length(points)!=11) stop("Points vector not of length 11")
  if (drop_decimal == TRUE & round == TRUE) stop("drop_decimal and round cannot both be TRUE")
  #Are there any muted assignments? or the points possible row?
  elim <- c()
  if(mute_check(gradebook) == T){
    elim <- 1
  }
  if(points_possible_check(gradebook) == T){
    elim <- c(elim, 2)
  }

  #eliminate non-enrolled students and other top junk
  elim_stud <- test.student(EIDs=non.enrolled, df=gradebook)
  gradebook <- gradebook[-c(elim, elim_stud),]
  #column for unique number
  gradebook$unique <- as.numeric(substr(gradebook$Section, 9, 13))
  #if gb was imported using check.names=F, remove space and add period
  names(gradebook) <- sub(" ", ".", names(gradebook))
  #make sure final points is numeric
  gradebook$Final.Points <- as.numeric(as.character(gradebook$Final.Points))
  ## Dealing with grades
  points <- sort(c(points, Inf, 0), decreasing=F)
  if(drop_decimal == TRUE){
    gradebook$Grade_Based_on_Points <- as.numeric(substr(gradebook$Final.Points, 1,3))
    gradebook$letter <- num_to_letter(gradebook$Grade_Based_on_Points, cutpoints = points, round = F)
  }

  if(round == TRUE){
    gradebook$Grade_Based_on_Points <- round(as.numeric(gradebook$Final.Points))
    gradebook$letter <- num_to_letter(gradebook$Grade_Based_on_Points, cutpoints = points, round = T)
  } else{
    gradebook$Grade_Based_on_Points <- as.numeric(gradebook$Final.Points)
    gradebook$letter <- num_to_letter(gradebook$Grade_Based_on_Points, cutpoints = points, round = F)
  }

  #Add columns for absences & remarks
  gradebook$absences <- NA
  gradebook$remarks <- NA
  #Keep only necessary columns
  if (full.out==TRUE){
    grades <- gradebook
  } else{
    grades <- subset(gradebook, select=c("Student", "SIS.User.ID", "letter", "absences", "remarks", "unique", "Final.Points"))
  }
  gradebook <- subset(gradebook, select=c("Student", "SIS.User.ID", "letter", "absences", "remarks", "unique"))
  #rename to match registrar requirements
  colnames(gradebook)<- c("Name", "EID", "Grade", "Absences", "Remarks", "Unique")
  #export the results
  dir.create("Final Grades -- Registrar Formatting")
  write.table(gradebook, file=paste(getwd(), "Final Grades -- Registrar Formatting", "FinalGradeSubmission.txt", sep="/"), sep="\t", row.names=F, na="")
  write.csv(grades, file=paste(getwd(), "Final Grades -- Registrar Formatting", "Final Grades - Letters and Numbers.csv", sep="/"))
}
