#' Title
#'
#' @param donors
#' @param risks
#' @param stats_risks
#' @param geo_level
#'
#' @return
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(123)
#' n = 1e3
#' donors <- create_data_example(n)
#' risks <- donors[sample(1:n, ceiling(n*0.07)),]
#' stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")
#' donors_drawn <- draw_donors(donors, risks, stats_risks, "geo")
draw_donors <- function(donors, risks, stats_risks, geo_level){

  cases <- 1:nrow(stats_risks)
  res <- as.list(cases)

  remaining_donors <- data.table::copy(donors)
  remaining_risks <- data.table::copy(risks)
  i = 0

  while(nrow(remaining_risks)*nrow(remaining_donors) > 0 & i <= length(cases)){
    i = i+1
    g = stats_risks[[geo_level]][i]
    n_draw = stats_risks$n_to_draw[i]
    res[[i]] <- remaining_donors[ remaining_donors[[geo_level]] != g, .SD[sample(.N, n_draw)]]$ident

    remaining_donors <- remaining_donors[! ident %in% res[[i]], ]
    remaining_risks <- remaining_risks[! ident %in% res[[i]], ]

  }

  return(list(drawn_donors = res, remain_donors, remain_risks))

}
