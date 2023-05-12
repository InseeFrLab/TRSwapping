#' Compute the number of donors and the number of donors to draw
#' for each crossing of geo and similarities
#'
#' @param donors data.table of individual donors
#' @param risks data.table of individuals that are considered as risky
#' @param similar character vector of the variables of similarity
#' @param geo_level character vector of the variable of the geographic level to consider
#' @param geo_level_sup character vector
#'
#' @return data.table crossing similarities and geo and with the following counts:
#' - `n_don`: number of potential donors (all individuals of  `donors` table that
#' are similar elsewhere: same values on similar variables but different geography)
#' - `n_risk`: number of risky individuals by geo * similar
#' - `n_to_draw`: number of donors to draw
#' @export
#' @import data.table
#' @examples
#' library(data.table)
#' set.seed(123)
#' n = 1e3
#' data <- create_data_example(n)
#' data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), "geo")
#' risks <- data_prep$risks
#' donors <- data_prep$donors
#' stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")
#'
#' # with two geo levels
#' n = 1e3
#' data <- create_data_example(n, add_geo = TRUE)
#' data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), c("geo","geo2"))
#' risks <- data_prep$risks
#' donors <- data_prep$donors
#' stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo", geo_level_sup = "geo2")
summary_risk <- function(donors, risks, similar, geo_level, geo_level_sup = NULL){

  stats_donors1 <- donors[, .(n_ind = .N), by = c(similar, geo_level, geo_level_sup)]
  data.table::setkeyv(stats_donors1, c(geo_level, geo_level_sup))

  stats_risks1 <- risks[, .(n_risk = .N), by = c(similar, geo_level, geo_level_sup)]

  all_geos <- unique(stats_risks1[, .SD, .SDcols = c(geo_level, geo_level_sup)])
  data.table::setkeyv(all_geos, c(geo_level))

  stats_donors2 <- purrr::map_dfr(
    all_geos[[geo_level]],
    \(g){
      if(is.null(geo_level_sup)){
        other_geos <- all_geos[!J(g),][, get(geo_level)]
        d <- stats_donors1[ J(other_geos), .(n_don = sum(n_ind)), by = c(similar)]
        d[, c(geo_level) := g]
      }else{
        g_sup <- all_geos[J(g), get(geo_level_sup)]
        other_geos <- all_geos[!J(g),][get(geo_level_sup) == g_sup, get(geo_level)]
        d <- stats_donors1[ J(other_geos), .(n_don = sum(n_ind)), by = c(similar)]
        d[, c(geo_level) := g][, c(geo_level_sup) := g_sup]
      }
    }
  )

  stats_donors <- merge(
    stats_donors1,
    stats_donors2,
    by = c(similar, geo_level, geo_level_sup),
    all = TRUE
  )

  stats_risks <- merge(
    stats_donors,
    stats_risks1,
    by = c(similar, geo_level, geo_level_sup),
    all.x = FALSE,
    all.y = TRUE
  )
  stats_risks[, n_to_draw := pmin(n_risk, n_don)]

  return(stats_risks[])

}

