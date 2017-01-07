#' Gradebook Clean
#'
#' Clean up a gradebook all in one fell swoop
#'
#' @param gradebook A gradebook dataframe
#' @param non_enrolled A vector of EIDs of non-enrolled students
#' @export

gradebook_clean <- function(gradebook, non_enrolled = c()){
  gb <- gradebook

  # test.student
  elim_stud <- test.student(EIDs = non_enrolled, df = gb)
  es <- ifelse(length(elim_stud) == 0, F, T)
  if(length(elim_stud) != 0){
    gb <- gb[-elim_stud,]
  }
  # mute.check
  mute <- mute_check(gb)
  if(mute == T){
    gb <- gb[-1,]
  }

  # points.check
  pc <- points_possible_check(gb)
  if(pc == T){
    gb <- gb[-1,]
  }

  # convert to numeric
  gb <- as.data.frame(apply(gb, 2, maybe_as_numeric))

  # make sure grade columns don't contain periods
  names(gb) <- gsub("\\.", " ", names(gb))

  out <- list(gb = gb, eliminated_students = es, mute_check = mute, points_possible_check = pc)
  return(out)
}
