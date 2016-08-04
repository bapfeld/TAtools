#' Checking if any grades are muted
#'
#' Internal use function to test whether any grades are muted in a given gradebook input
#' @param gradebook   A gradebook file from Canvas
#' @return Outputs True/False value

mute_check <- function(gradebook){
  check_vec <- c(t(gradebook[1:2,]))
  check <- grep("Muted", check_vec)
  ifelse(length(check) == 0, FALSE, TRUE)
}
