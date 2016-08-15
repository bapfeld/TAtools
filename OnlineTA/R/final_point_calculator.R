#' Final Grade Calculator
#'
#' This funciton has two primary purposes: first, to calculate a curve based on a desired percentage of students to receive either an A or an A or a B; second, to calculate the number of students who would receive a certain grade if top point value were adjusted.
#' @param gradebook A dataframe of the final gradebook downloaded from Canvas.
#' @param method Four methods are supported: "McDS", "top points", "curve-a", and "curve-ab".  See Methods below for details.
#' @param top.score What value should be tested for McDS and top points methods. Default is maximum score in gradebook.
#' @param curve.a Percentage of students who should receive an A.  Must be input as an integer. For use with curve-a method only.
#' @param curve.ab Percentage of students who should receive either an A or a B. Must be input as an integer. For use with curve-ab method only.
#' @param total.points What is the total number of points in the class? For use with curve-a and curve-ab methods.
#' @param grade.scale A vector of length 11 indicating the LOWER point values for each grade break A through D-.  Use only integers- the function will take care of rounding. Breaks do not need to be any particular order. Default is UT standard division of grades.
#' @return List of 2. Dataframe with four columns: grade, scale, number, and percent, which refer to letter grade, point value at lower end of range, number of students who will receive that grade, and the percent of students who will receive that grade, respectively. Second item in list is the maximum number of points, which is both use and method dependent.
#' @return "McDS" For use with the McDaniel-Shaw version of Online310 only- calculation and rounding are non-standard!  It takes a given top.score value and calculates the number of students who would fall in each grade category. "Maximum points" return is top score in class if default used, otherwise returns inputted value.
#' @return "top points" Takes a given top.score value and calculate the number of students who would fall in each grade category. "Maximum points" return is top score in class if default used, otherwise returns inputted value.
#' @return "curve-a" Takes the desired percentage of students who should receive an A and calculates what the top point value should become to achieve that value. Results may differ by a few students due to rounding. "Maximum points" return is the top point value.
#' @return "curve-ab" Takes the desired percentage of students who should receive either an A or a B and calculates what the top point value should become to achieve that value. Results may differ by a few students due to rounding. "Maximum points" return is the top point value.
#' @examples
#' set.curve(gradebook.file, method=c("top points"), top.score="475")
#' set.curve(gradebook.file, method=c("curve-ab"), curve.ab=18, total.points=450)
#' @export

set.curve <- function(gradebook, method=c("McDS", "top points", "curve-a", "curve-ab"), top.score="max score", curve.a=10, curve.ab=20, total.points=500, grade.scale=c(93, 90, 87, 83, 80, 77, 73, 70, 67, 63, 60)){
  ## Clean up data
  #Are there any muted assignments? or the points possible row?
  if(any(grepl("Muted", c(t(gradebook[1,])))) & any(grepl("Points Possible", gradebook[1:2,1]))){
    top <- c(1,2)
  }else{
    if(any(grepl("Muted", c(t(gradebook[1,])))) | any(grepl("Points Possible", gradebook[1:2,1]))){
      top <- 1
    } else{
      top <- (nrow(gradebook) + 1)
    }
  }
  #eliminate non-enrolled students
  elim <- test.student(df=gradebook)
  gradebook <- gradebook[-c(top, elim),]
  gradebook$Final.Points <- as.numeric(gradebook$Final.Points)
  ## Method 1 (very specific to McDaniel and Shaw)
  if(method=="McDS"){ #insert point value and calculate #/%s of students who fall into each grade using McDaniel Shaw method
    #is top score used or other number?
    if(top.score==c("max score")){
      top <- max(gradebook$Final.Points)
    } else {
      top <- top.score
    }
    stud <- nrow(gradebook)
    out.1 <- data.frame(Grade=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F"), Scale=c((sort(grade.scale, decreasing=T))/100, 0), Points.Level=(NA), Number=c(NA), Percent=c(NA))
    out.1$Points.Level <- round((out.1$Scale*top), digits=0)
    for(i in 2:12){
      out.1$Number[i] <- sum(gradebook$Final.Points>=out.1$Points.Level[i] & gradebook$Final.Points < out.1$Points.Level[i-1])
    }
    out.1$Number[1] <- sum(gradebook$Final.Points >= out.1$Points.Level[1])
    out.1$Percent <- (out.1$Number/stud)*100
    list(out.1, "Max Points"=max(gradebook$Final.Points))
  } #end method "McDS"
  ## Method 2
  else if(method=="top points"){ #insert point value and calculate #/%s of students who fall into each grade
    #is top score used or other number?
    if(top.score==c("max score")){
      top <- max(gradebook$Final.Points)
    } else {
      top <- top.score
    }
    stud <- nrow(gradebook)
    gradebook$calc <- gradebook$Final.Points/top
    out.1 <- data.frame(Grade=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F"), Scale=c((sort(grade.scale, decreasing=T)-.5)/100, 0), Number=c(NA), Percent=c(NA))
    for(i in 2:12){
      out.1$Number[i] <- sum(gradebook$calc>=out.1$Scale[i] & gradebook$calc < out.1$Scale[i-1])
    }
    out.1$Number[1] <- sum(gradebook$calc >= out.1$Scale[1])
    out.1$Percent <- (out.1$Number/stud)*100
    list(out.1, "Max Points"=max(gradebook$Final.Points))
  } #end method "top points"
  ## Method 3
  else if (method=="curve-a"){ #what is curve level for desired % of A's
    num.stud <- ceiling(nrow(gradebook)*(curve.a/100))
    trial <- total.points
    gradebook$calc <- gradebook$Final.Points/trial
    a.level <- sort((grade.scale-.5)/100, decreasing=T)[2]
    current.a <- sum(gradebook$calc>=a.level)
    if(current.a >= num.stud) stop ("At least inputted % of students have an A with no curve")
    while(sum(gradebook$calc>=a.level)<num.stud){
      gradebook$calc <- gradebook$Final.Points/trial
      trial <- trial-1
    }
    out.1 <- data.frame(Grade=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F"), Scale=c((sort(grade.scale, decreasing=T)-.5)/100, 0), Number=c(NA), Percent=c(NA))
    for(i in 2:12){
      out.1$Number[i] <- sum(gradebook$calc>=out.1$Scale[i] & gradebook$calc < out.1$Scale[i-1])
    }
    out.1$Number[1] <- sum(gradebook$calc >= out.1$Scale[1])
    out.1$Percent <- (out.1$Number/nrow(gradebook))*100
    list(out.1, "Max Points"=trial+1)
  } #end curva-a method
  ## Method 4
  else if (method=="curve-ab"){ #what is curve level for desired % of A's & B's
    num.stud <- ceiling(nrow(gradebook)*(curve.ab/100))
    trial <- total.points
    gradebook$calc <- gradebook$Final.Points/trial
    ab.level <- sort((grade.scale-.5)/100, decreasing=T)[5]
    current.ab <- sum(gradebook$calc>=ab.level)
    if(current.ab >= num.stud) stop ("At least inputted % of students have an A or a B with no curve")
    while(sum(gradebook$calc>=ab.level)<num.stud){
      gradebook$calc <- gradebook$Final.Points/trial
      trial <- trial-1
    }
    out.1 <- data.frame(Grade=c("A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F"), Scale=c((sort(grade.scale, decreasing=T)-.5)/100, 0), Number=c(NA), Percent=c(NA))
    for(i in 2:12){
      out.1$Number[i] <- sum(gradebook$calc>=out.1$Scale[i] & gradebook$calc < out.1$Scale[i-1])
    }
    out.1$Number[1] <- sum(gradebook$calc >= out.1$Scale[1])
    out.1$Percent <- (out.1$Number/nrow(gradebook))*100
    list(out.1, "Max Points"=trial+1)
  } #end curva-ab method
}
