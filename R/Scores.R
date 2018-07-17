
#' Computes the quality scores of the respondents in \code{dataset}. The score is defined as the proportion of scores that has the value for a variable as the variable before it.
#'
#' @param dataset A dataset that contains all variables to base the score on.
#' @return A numeric vector of scores.
#' @note  This function requires at least two variables in the dataset.
#'
#' @export
computeQualityScores <- function(dataset) {

  unlist(sapply(1:nrow(dataset), function(x){
    computeQualityScore(x, dataset)
  }))

}

#' Computes the quality score of the respondent at \code{index} of \code{dataset}. The score is defined as the proportion of scores that has the value for a variable as the variable before it.
#'
#' @param index The index of the respondent in the dataset.
#' @param dataset A dataset that contains all variables to base the score on.
#' @return A numeric score
#' @note  This function requires at least two variables in the dataset.
#'
#' @export
computeQualityScore <- function(index, dataset) {
  if(index > nrow(dataset)) {
    stop("index is higher than number of rows in dataset")
  }

  row <- t(dataset[index,])

  if(NA %in% row[,1]) {
    warning("Missing data in respondent's values. Omitting.")
    row <- na.omit(row)
  }

  if(length(row[,1]) == 0) {
    stop("There is no data for this respondent.")
  }

  Reduce(f = function(sum, i){
    if(row[i,1] == row[i-1,1]) {
      sum + 1
    } else {
      sum
    }
  }, x = 2:length(row), init = 0) / (length(row) - 1)

}
