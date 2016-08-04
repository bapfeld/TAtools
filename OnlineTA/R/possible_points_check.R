#' Checking if total points row is present
#'
#' Internal use function to test whether total points possible row is present in a given gradebook input
#' @param gradebook   A gradebook file from Canvas
#' @return Outputs True/False value

points_possible_check <- function(gradebook){
  check_vec <- c(t(gradebook[1:2,]))
  check <- grep("Points Possible", check_vec)
  ifelse(length(check) == 0, FALSE, TRUE)
}
