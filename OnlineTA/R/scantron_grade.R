#' Importing Grades from Scanned Test Results
#'
#' Use this function to add grades in a canvas gradebook for an assignment based on a CSV file you download from scanning services.
#' @param gb  A dataframe of a gradebook file from Canvas.
#' @param scantron  A dataframe of the scanned scores from scanning servies. Should be the CSV listed under the "file to upload grades to Canvas or Blackboard" or something like that.
#' @param assignment_name  The name of the assignment in the gradebook that will receive the grade (e.g. "Exam 1"). Should be formatted exactly as it appears in the dataframe, but can be a partial match (assuming no overlap with other names)
#' @return Outputs an updated gradebook file. The muted assignments and points possible rows will be removed from top if present in original gradebook. Will also write NA to students who have no grade for the assignment. If a grade is already entered for that assignment, the function will not overwrite. This means that if you give a test where students had the option to take it online or in person, it will preserve the online grades and just merge in the paper scantron ones.
#' @export

scantron_grade <- function(gb, scantron, assignment_name){
  # set some values we'll need later
  n_columns <- ncol(gb)
  assignment_column <- grep(assignment_name, names(gb))
  df_names <- names(gb)

  # clean gb
  clean_gb <- gradebook_clean(gb, non_enrolled = c("speede", "sm56684", "tm29778"))
  gb <- clean_gb[[1]]

  # remove unnecessary lines from scantron if present
  scantron <- scantron[which(scantron$`Last Name` != "Total Records Read"), c("EID", "Score")]

  # make compatible to merge
  names(scantron) <- c("SIS User ID", "new_val")
  scantron[,1] <- tolower(as.character(scantron[,1]))
  scantron[,1] <- gsub(" ", "", scantron[,1])

  # merge by EID
  gb <- merge(gb, scantron, all = T, by = "SIS User ID")

  # heart of the function
  gb <- apply(gb, 1, function(dat){
    if(is.na(dat[assignment_column]) == T){
      dat[assignment_column] <- dat[length(dat)]
    }
    dat
  })
  df_out <- data.frame(t(gb))
  df_out <- df_out[,-(n_columns+1)]
  names(df_out) <- df_names
  df_out
}
