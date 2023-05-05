
#' Create a dataset of fake individuals with some characteristics
#'
#' @param n number of individuals
#'
#' @return data.table object of n rows and 5 variables
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(123)
#' individuals <- create_data_example(n = 1e5)
#' individuals[, .N, by = .(edu, sex)]
#' individuals[, .N, by = .(sex, age)]
#' individuals[, .N, by = .(geo)]
create_data_example <- function(n = 1e5){

  require(data.table)

  individuals <- data.table(
    ident = 1:n,
    geo = ceiling(rexp(n, rate = 1/10)),
    edu = sample(c(1:5), n, replace = TRUE, prob = c(0.1,0.15,0.25,0.4,0.1)),
    sex = sample(1:2, n, replace = TRUE, prob = c(0.45, 0.55)),
    age = sample(seq(0,105,5), n, replace = TRUE, prob = c(rep(1/25,4), rep(1/20,12), rep(1/25,6)))
  )

  return(individuals)
}
