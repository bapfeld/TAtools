#' Number to Letter
#'
#' Use this function to convert numeric student grades to letter.
#' @details This function facilitates randomizing studio attendance and invititations for the online class. Create a poll in piazza asking students to complete it in order to add their name to the attendance drawing list.  (The poll can have a single option - "Yes, I'd like to attend")  Download the results of that poll and open the file in R.  Then process it using this function, which will place a CSV on your desktop with a list of 50 names, 25 for each day of the week, along with the student's email address.  This should facilitate contacting the students with directions for attending the studio.
#' @param numbergrade A vector with numeric grades to be converted
#' @param cutpoints A vector with the lower bounds of each half-letter grade and Inf to bound the top
#' @param letters A vector of the letters to be assigned (no reason to change this ever)
#' @param round A logical of whether you want to round numeric values for letter assignment.
#' @return Vector with letters based on the grading scheme

num_to_letter <- function(numbergrade,
                          cutpoints = c(0, 60, 63, 67, 70, 73,
                                        77, 80, 83, 87, 90, 93, Inf),
                          letters = c("F", "D-", "D", "D+", "C-", "C",
                                      "C+", "B-", "B", "B+", "A-", "A"),
                          round = TRUE) {
  if (round == TRUE){
    cutpoints <- cutpoints - 0.5
  }
  cut(x = numbergrade,
      breaks = cutpoints,
      labels = letters,
      right = FALSE)
}
