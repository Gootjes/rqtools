
library(ggplot2)

#' @title Return plots for all respondents in \code{x}
#'
#' @return A ggplot
#'
#' @export
plot_respondents <- function(x, id.var = NULL) {
  UseMethod("plot_respondents", x)
}

#' @export
plot_respondents.data.frame <- function(x, id.var = NULL) {
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

  setNames(Map(f = function(rowi){
    plot_respondent(x[rowi,], title = ids[rowi])
  }, 1:nrow(x)), ids)
}

#' @title Returns a plot for the respondent at \code{index} of \code{data}
#'
#' @param index The index of the respondent in the data.
#' @param data A dataset that contains all variables and respondents to be plotted.
#' @param xlab A vector of titles that should be given to the plots. Can be \code{null}. See examples.
#' @return A ggplot
#'
#' @export
plot_respondent <- function(data, title = NULL) {
  UseMethod("plot_respondent", data)
}

#' @export
plot_respondent.data.frame <- function(dataset, title = NULL) {

  data <- data.frame(response = t(dataset)[,1])

  #colnames(data) <- index
  data$x <- rownames(data)
  data$x <- factor(data$x, levels = data$x)

  g <- ggplot(data = data, aes(x=x)) +
    geom_line(aes(y=response, group = 1)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=90, vjust=0.6), legend.position = "none") +
    labs(title = title, y = "responses", x = "variables",
         subtitle = paste("Flatline score:", round(compute_flatline_score(unlist(c(data$response))), 2)))

  g
}
