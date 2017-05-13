#' A quick test student search function
#'
#' This function identifies which rows in a data frame contain test students.  Required for other functions in package.  Automatically extracts line for universal "Student, Tester" as well.
#' @param EIDs List of tester student EIDs.  Defaults for Speede, Tester McStudent, and Student McStudent
#' @param df A dataframe of a class roster.  Typically a recent copy of the Canvas gradeboook.
#' @return A vector of the row numbers in which there are test students
#' @examples
#' test.student.search.func()
test.student <- function(EIDs=c("speede", "sm56684", "tm29778"), df){
  out <- c()
  for (i in 1:length(EIDs)){ #loop to search for inputted eids
    temp <- which(apply(df, 1, function(x) any(grepl(EIDs[i], x))))
    out <- c(out, temp)
  }
  #eliminate universal class test student
  temp2 <- which(apply(df, 1, function(x) any(grepl("Student, Test", x))))
  out <- as.numeric(c(out, temp2))
  if (length(out) == 0){
    out
  } else {for (j in 1:length(out)){ #loop to eliminate 0s if eids were not found
    if (out[j] == 0){
      out <- out[-j]
    }
  }
    #eliminate duplicated
    d <- duplicated(out)
    out <- out[d == FALSE]
    out
  }
}
