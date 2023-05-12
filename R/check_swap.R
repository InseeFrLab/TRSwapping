#' Check the result of the swapping
#'
#' @param risks data.table
#' @param donors data.table
#' @param res_swap data.table result of the swap
#'
#' @return list
#' @export
#'
#' @examples
#' #' library(data.table)
#' set.seed(123)
#' n = 1e3
#' donors <- create_data_example(n)
#' risks <- donors[sample(1:n, ceiling(n*0.07)),][, scope_risk := "geo"]
#' donors_drawn <- draw_donors_multi_geo_multi_sim(donors, risks, l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")), geo_levels = "geo")
#' check_res <- check_swap(risks, donors, donors_drawn)
check_swap <- function(risks, donors, res_swap){

  ind_swapped <- c(res_swap$orig, res_swap$dest)
  uniqueness <- length(ind_swapped) == length(unique(ind_swapped))
  risks_not_swapped <- risks[ ! ident %in% unique(ind_swapped), ]
  nb_not_swapped <- nrow(risks_not_swapped)
  swapped_between_risks <- res_swap[orig %in% risks$ident & dest %in% risks$ident, ]
  swap_rate_observed <- (length(unique(ind_swapped)) - nrow(risks_not_swapped))/nrow(donors)*100

  stats <- res_swap[, .N, by = .(geo_level, similar)]

  return(
    list(
      uniqueness = uniqueness,
      risks_not_swapped = risks_not_swapped,
      swapped_between_risks = swapped_between_risks,
      nb_not_swapped = nb_not_swapped,
      swap_rate_observed = swap_rate_observed,
      stats = stats
    )
  )
}
