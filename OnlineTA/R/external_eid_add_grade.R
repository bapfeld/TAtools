#' Generating Grades From Non-Canvas Grade Source
#'
#' Use this function to add grades in a canvas gradebook for a complete/incomplete assignment based on a list of EIDs.
#' @param qualtrics_vector   A vector of EIDs for students who should be given full credit. Typically, from a qualtrics survey, though other sources are possible. Make sure that the vector does not contain any blank rows or all students will be given credit.
#' @param gradebook   A gradebook file from Canvas. You do not need to worry about formatting.
#' @param assignment_col_number   The column number of the assignment to give points to. If you are uncertain, use the names() function on your gradebook file to determine the correct value.
#' @param point_value   The number of points that should be awarded for completing the assignment.
#' @param missing_as_zero  Logical for whether students not included in the EID vector should be assigned a 0. Default is TRUE.
#' @param ignore_length   Default is FALSE. If TRUE, then function will not check the length of your EIDs. Checking length is useful if students entered their own EIDs. Default minimum is < 5 will throw a warning. It is possible for a student to have a 4 character EID.
#' @return Outputs an updated gradebook file. Note: Depending on how you imported the gradebook, R may convert () into "."s.  This will result in a file that Canvas will not accept. Copy/paste the single column as appropriate in a program outside of R and upload the same file that was downloaded. Also automatically outputs a warning message reporting which assignment you updated. Importing using rio::import("path_to_file.csv") and exporting with rio::export(gb, file = "path_to_file.csv") is recommended to avoid these problems.
#' @export

Offline.grade <- function(qualtrics_vector, gradebook, assignment_col_number, point_value, missing_as_zero = TRUE, ignore_length = FALSE){
  q_vec <- levels(qualtrics_vector)
  if(length(which(q_vec == "userid")) != 0){
    q_vec <- q_vec[-which(q_vec == "userid")]
  }
  col_num <- assignment_col_number
  stud.row <- c()
  clean_gb <- gradebook_clean(gradebook, non_enrolled = c("speede", "sm56684", "tm29778"))
  gradebook <- clean_gb[[1]]
  if(any(sapply(q_vec, nchar) < 4)){
    warning("It looks like you have EIDs that are too short. Please double check the values in the vector of EIDs you are using.  If you are certain there are no errors, you may set the ignore_length option to TRUE", immediate. = TRUE)
    if(ignore_length == FALSE){
      stop
    }
  }
  if (length(grep(" ", q_vec)) > 0){
    warning("It looks like you have space(s) in your vector of EIDs. You must remove these for this function to work.", immediate. = TRUE)
    stop
  }
  stud_points <- match(q_vec, gradebook$`SIS User ID`)
  gradebook[stud_points,col_num] <- point_value
  if (missing_as_zero == TRUE){
    gradebook[-stud_points,col_num] <- 0
  }
  warning("You updated the assignment ", colnames(gradebook[assignment_col_number]), ". If this is incorrect, please verify you input the correct column number for the assignment.")
  return(gradebook)
}
