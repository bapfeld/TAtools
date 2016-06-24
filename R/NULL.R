#' Sample Gradebook
#'
#' A dataset containing a sample gradebook, as it would appear if downloaded from Canvas.
#' 
#'
#' \itemize{
#'   \item Student Names of students in format Last, First
#'   \item ID System assigned 7-digit student ID number
#'   \item SIS.User.ID EID
#'   \item SIS.Login.ID EID again (differs only for universal test student)
#'   \item Section A string indicating the short course name and its unique number
#'   \item Quiz.x 11 quiz variables
#'   \item In.Class.x 9 in class activity variables
#'   \item Reading.Assignment an assignment
#'   \item "Required Agreement" Another assignment
#'   \item Video.Submission Another assignment
#'   \item Release.of.Video Another assignment
#'   \item Paper.1 First essay assignment
#'   \item Paper.2 Second essay assignment
#'   \item Reading.x 10 chapters of reading assignments
#'   \item Pre.class.Questionnire An out-of-class assignment
#'   \item Introductory.Survey An out-of-class assignment
#'   \item Paper.1.Prompt.Survey An out-of-class assignment
#'   \item Mid.term.Evaluation An out-of-class assignment
#'   \item Survey.3 An out-of-class assignment
#'   \item Paper.2.Prompt.Survey An out-of-class assignment
#'   \item Survey.4 An out-of-class assignment
#'   \item Quizzes... A set of 4 variables for each course category.  Essentially useless.
#' }
#'
#' @format A data frame with 918 rows and 82 variables.  The top two lines are junk, but are preserved, as this is how downloaded gradebooks appear.
#' @name SampleGradebook
NULL