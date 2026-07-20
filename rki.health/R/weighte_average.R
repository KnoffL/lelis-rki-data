#' function to calculate weighted average
#'
#' @param value a numeric vector containing the values to be calculated
#' @param sample_size a numeric vector containing the n for each value
#' @return one value which added each value together weighted by their n
#' @export
#' @examples
#' final_grade <- weighted_average(grades, ETCS)
#'
#' @description value should contain the values to be summed up, sample_size
#' indicates the weight of the value with the same position, the weight is
#' calculated by dividing the individual sample_size value with the summ of
#' sample_size
weighted_average <- function(value, sample_size) {
  if (!is.vector(value) | !is.vector(sample_size)) {
    stop("At least one of the inputs isn't a vector")
  }
  s <- sum(sample_size)
  result <- 0
  for (x in 1:length(value)) {
    result <- result + value[x] * sample_size[x] / s
  }
  return(result)
}

devtools::load_all()
devtools::document()
?weighted_average
