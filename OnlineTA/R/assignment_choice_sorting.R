#' Assignment Choice Sorting
#'
#' Use this function if you have an assignment in which students had a choice (e.g. essay topics) and indicated which assignment they chose through a qualtrics survey.
#' @param survey.results A dataframe created from the qualtrics survey.
#' @param class.roster A dataframe of the class roster. Use a recently downloaded copy of the gradebook.
#' @param varnames A vector of character strings indicating the column names of the responses in the qualtrics survey.  Must be entered in the following order: eid, last name, first name, essay choice
#' @param test.EIDs A vector of character strings indicating the test student EIDs used in the class.  Defaults are for Speede, Student McStudent, and Tester McStudent. Function works even if these students are not present.
#' @return Outputs three .csv files to a new folder in the working directory named to avoid any duplication or overwriting of previous files: "duplicated responses.csv" indicates any students who have submitted more than one unique response to the qualtrics survey. Ignores misspelled names and identical submissions; "missing responses.csv" indicates any students who did not submit a qualtrics survey response; "all responses.csv" contains all students including duplicates and missing students.
#' @examples
#' assignment.choice(survey, roster, varnames=c("userid", "Q3", "Q5", "Q7"))
#' @export
assignment.choice <- function(survey.results, class.roster, varnames=c(), test.EIDs=c("speede", "sm56684", "tm29778")){
  survey <- subset(survey.results, select=varnames)[-1,]
  colnames(survey) <- c("EID", "Last Name", "First Name", "Essay Choice")
  roster <- class.roster
  #Are there any muted assignments?
  mute <- c(t(roster[1,]))
  if(grep("Muted", mute)[1]>1){
    top <- c(1,2)
  } else{
    top <- 1
  }
  roster <- subset(roster, select=c(SIS.User.ID, ID, Student))
  elim <- test.student(EIDs=test.EIDs, df=roster)
  roster <- roster[-c(top, elim),]
  colnames(roster) <- c("EID", "ID", "Student")
  #Now, merge the two of them
  merged.data <- merge(roster, survey, by="EID", all.x=T)
  dup <- subset(merged.data, select=c("EID", "ID"))
  dup$duplicate <- duplicated(dup) | duplicated(dup, fromLast=TRUE)
  dup <- dup[!duplicated(dup),]
  essay <- merge(merged.data, dup, by=c("EID", "ID"))
  #Finally, eliminate any identical responses
  essay$ed <- FALSE
  for (i in 1:(nrow(essay)-1)){
    if (essay$ID[i]==essay$ID[i+1]){
      if(essay$"Essay Choice"[i]==essay$"Essay Choice"[i+1]){
        essay$duplicate[i] <- FALSE
        essay$ed[i+1] <- TRUE
      }
    }
  }
  essay <- essay[essay$ed==FALSE,-8]
  duplicated.essays <- essay[essay$duplicate==TRUE,] #possible there are none
  missing.essays <- subset(essay, is.na(essay$"Essay Choice"))
  #write the output to a new folder
  time <- Sys.time()
  folder.name <- paste("Assignment Choice", time)
  dir.create(folder.name)
  write.csv(duplicated.essays, file=paste(getwd(), folder.name, "duplicated responses.csv", sep="/"))
  write.csv(missing.essays, file=paste(getwd(), folder.name, "missing responses.csv", sep="/"))
  write.csv(essay, file=paste(getwd(), folder.name, "all responses.csv", sep="/"))
}


