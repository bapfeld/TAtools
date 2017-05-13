#' Alex's maybe_numeric function
#'
#' Internal function used to convert columns to numbers when they should be
#'
#' @param v A gradebook dataframe

maybe_as_numeric <- function(v){
  v <- ifelse(is.factor(v), as.character(v), v)
  if (is(tryCatch(as.numeric(v),
                  warning = function(w) w),
         "warning")){
    return(v)
  } else{
    return(as.numeric(v))
  }
}
