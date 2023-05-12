
#' Create a dataset of fake individuals with some characteristics
#'
#' @param n number of individuals
#'
#' @return data.table object of n rows and 7 variables
#' (one ident, one geo var, three cat vars and two var for the risk)
#'
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(123)
#' individuals <- create_data_example(n = 1e5)
#' individuals[, .N, by = .(edu, sex)]
#' individuals[, .N, by = .(sex, age)]
#' individuals[, .N, by = .(geo)]
#' individuals <- create_data_example(n = 1e5, add_geo = TRUE)
#' individuals[, .N, by = .(geo2)]
#' individuals[, .N, by = .(scope_risk)]
create_data_example <- function(n = 1e5, add_geo = FALSE){

  individuals <- data.table(
    ident = 1:n,
    geo = ceiling(rexp(n, rate = 1/10)),
    edu = sample(c(1:5), n, replace = TRUE, prob = c(0.1,0.15,0.25,0.4,0.1)),
    sex = sample(1:2, n, replace = TRUE, prob = c(0.45, 0.55)),
    age = sample(seq(0,105,5), n, replace = TRUE, prob = c(rep(1/25,4), rep(1/20,12), rep(1/25,6))),
    is_risky = FALSE,
    scope_risk = "NA"
  )

  if(add_geo){
    individuals[, geo2 := (geo %% 5) + 1]
    individuals[sample(1:n, ceiling(n*0.05)), `:=`(is_risky = TRUE, scope_risk = "geo")]
    individuals[sample(1:n, ceiling(n*0.03)), `:=`(is_risky = TRUE, scope_risk = "geo2")]
    individuals[, scope_risk := ifelse(scope_risk == "NA", NA, scope_risk)]
  }else{
    individuals[sample(1:n, ceiling(n*0.07)), `:=`(is_risky = TRUE, scope_risk = "geo")]
    individuals[, scope_risk := ifelse(scope_risk == "NA", NA, scope_risk)]
  }

  return(individuals)
}
