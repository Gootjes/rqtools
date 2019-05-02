
#' Computes the quality scores of the rows in \code{dataset}.
#'
#' @param x A dataset that contains every subject (rows) and all variables (columns) to be used. Or a numeric vector to compute a single score.
#' @param id.var Either a numeric or character vector identifying a column in the dataset, or a vector of the same length as the amount of rows in x.
#' @return A numeric vector of scores. A named vector in case id.var is non NULL.
#' @note  The score is defined as the proportion of scores that has the value for a variable as the variable before it.
#'
#' @export
compute_flatline_score <- function(x, id.var = NULL) {
  UseMethod("compute_flatline_score", x)
}

#' @export
compute_flatline_score.data.frame <- function(x, id.var = NULL) {

  if(is.null(id.var)) {
    ids <- 1:nrow(x)
  } else {
    if(length(id.var) == 1) {
      if(is.numeric(id.var)) {
        if(id.var > ncol(x)) {
          stop("id.var refers to an unidentified column")
        }
      } else if(is.character(id.var)) {
        if(!(id.var %in% colnames(x))) {
          stop("id.var refers to an unidentified column")
        }
      } else {
        stop("id.var is of unknown type")
      }
      ids <- x[[id.var]]
      x[[id.var]] <- NULL
    } else if(length(id.var) == nrow(x)) {
      ids <- id.var
    } else {
      stop("id.var must either be of length one, or of the same length as the amount of rows in x")
    }
  }

  if(ncol(x) < 2) {
    stop("x needs to consist of at least two data columns to be able to compute rq scores")
  }

  if(nrow(x) < 1) {
    stop("x needs to consist of at least one row to be able to compute rq scores")
  }

  scores <- setNames(Map(f = function(rowi) {
    data <- unlist(c(x[rowi,]))
    score <- compute_flatline_score(data)
  }, 1:nrow(x)), ids)

  unlist(scores)
}

#' @export
compute_flatline_score.numeric <- function(x, ...) {

  if(any(is.na(x))) {
    warning("Missing data in respondent's values. Omitting.")
    x <- na.omit(x)
  }

  if(length(x) == 0) {
    stop("There is no data. Vector is of length zero.")
  }

  score <- Reduce(f = function(sum, i){
    if(x[i] == x[i-1]) {
      sum + 1
    } else {
      sum
    }
  }, x = 2:length(x), init = 0) / (length(x) - 1)

  score
}
