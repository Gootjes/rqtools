# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'




library(ggplot2)

#' Return plots for all respondents in \code{data}
#'
#' @param data A dataset that contains all variables and respondents to be plotted.
#' @param titles A vector of titles that should be given to the plots. Can be \code{null}. See examples.
#' @return A ggplot
#' @examples
#' ids <- paste0("ID_",1:10)
#' data <- data.frame(ID = ids, L = sample(1:7, 10, replace = T), P = sample(1:7, 10, replace = T))
#' plots <- plotRespondents(data = mpg, titles = ids)
#'
#' @export
plotRespondents <- function(data, id.var = NULL) {
  UseMethod("plotRespondents", data)
}

#' @export
plotRespondents.data.table <- function(data, id.var) {
  if(missing(id.var)) {
    stop("No identity variable specified")
  } else {
    if(is.null(data[[id.var]])) {
      stop("id.var not in dataset")
    }
  }

  Map(f = function(id){
    plotRespondent(data[data[[id.var]] == id,], title = id)
  }, data[[id.var]])

}

#' @export
plotRespondents.data.frame <- function(data, id.var) {
  if(missing(id.var)) {
    stop("No identity variable specified")
  } else {
    if(is.null(data[[id.var]])) {
      stop("id.var not in dataset")
    }
  }

  Map(f = function(id){
    plotRespondent(data[data[[id.var]] == id,], title = id)
  }, data[[id.var]])
}

#' Returns a plot for the respondent at \code{index} of \code{data}
#'
#' @param index The index of the respondent in the data.
#' @param data A dataset that contains all variables and respondents to be plotted.
#' @param xlab A vector of titles that should be given to the plots. Can be \code{null}. See examples.
#' @return A ggplot
#' @examples
#' ids <- paste0("ID_",1:10)
#' data <- data.frame(ID = ids, L = sample(1:7, 10, replace = T), P = sample(1:7, 10, replace = T))
#' plot <- plotRespondent(index = 1, data = mpg, title = ids[1])
#'
#' @export
plotRespondent <- function(data, title = index) {
  UseMethod("plotRespondent", data)
}

#' @export
plotRespondent.data.table <- function(data, title = index) {

  plotRespondent.data.frame(as.data.frame(data), title = index)

}

#' @export
plotRespondent.data.frame <- function(dataset, title = index) {

  data <- data.frame(response = t(dataset)[,1])

  #colnames(data) <- index
  data$x <- rownames(data)
  data$x <- factor(data$x, levels = data$x)

  g <- ggplot(data = data, aes(x=x)) +
    geom_line(aes(y=response, group = 1)) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=90, vjust=0.6), legend.position = "none") +
    labs(x = title, y = "responses")

  g
}
