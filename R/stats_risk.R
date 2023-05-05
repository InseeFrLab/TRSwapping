#' Compute the number of donors and the number of donors to draw
#' for each crossing of geo and similarities
#'
#' @param donors data.table of individual donors
#' @param risks data.table of individuals that are considered as risky
#' @param similar character vector of the variables of similarity
#' @param geo_level character vector of the variable of the geographic level to consider
#'
#' @return data.table crossing similarities and geo and with the following counts:
#' - `n_don`: number of potential donors (all individuals of  `donors` table that
#' are similar elsewhere: same values on similar variables but different geography)
#' - `n_risk`: number of risky individuals by geo * similar
#' - `n_to_draw`: number of donors to draw
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(123)
#' n = 1e3
#' donors <- create_data_example(n)
#' risks <- donors[sample(1:n, ceiling(n*0.07)),]
#' stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")
summary_risk <- function(donors, risks, similar, geo_level){

  require(data.table)
  stats_donors1 <- donors[, .(n_ind = .N), by = c(similar, geo_level)]

  stats_donors2 <- purrr::map_dfr(
    unique(stats_donors1[[geo_level]]),
    \(g){
      d <- stats_donors1[ stats_donors1[[geo_level]] != g, .(n_don = sum(n_ind)), by = c(similar)]
      d[, c(geo_level) := g]
    }
  )
  stats_donors <- merge(stats_donors1, stats_donors2, by = c(similar, geo_level), all = TRUE)

  stats_risks <- merge(
    stats_donors,
    risks[, .(n_risk = .N), by = c(similar, geo_level)],
    by = c(similar, geo_level),
    all.x = FALSE,
    all.y = TRUE
  )
  stats_risks[,n_to_draw := pmin(n_risk, n_don)]

  return(stats_risks)

}

