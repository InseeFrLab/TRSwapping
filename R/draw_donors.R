#' Match similar risks-donors at one geographic level
#'
#' @param donors data.table
#' @param risks data.table
#' @param stats_risks data.table
#' @param similar character vector
#' @param geo_level character vector
#'
#' @return a data.table
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(123)
#' n = 1e3
#' donors <- create_data_example(n)
#' risks <- donors[sample(1:n, ceiling(n*0.07)),]
#' stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")
#' donors_drawn <- draw_donors(donors, risks, stats_risks, similar = c("edu", "sex"), geo_level = "geo")
draw_donors <- function(donors, risks, stats_risks, similar, geo_level){

  cases <- 1:nrow(stats_risks)
  res <- as.list(cases)

  remaining_donors <- data.table::copy(donors)
  remaining_risks <- data.table::copy(risks)
  setkeyv(remaining_risks, c(similar, geo_level))
  setkeyv(remaining_donors, c(similar))
  i = 0

  while(nrow(remaining_risks) > 0 & nrow(remaining_donors) > 0 & i < length(cases)){
    i = i+1

    # stats_risks <- summary_risk(remaining_donors, remaining_risks, similar = c("edu", "sex"), geo_level = "geo")

    stats <- copy(stats_risks[i,])
    setkeyv(stats, c(similar, geo_level))

    g = stats_risks[[geo_level]][1]

    concerned_risks <- remaining_risks[stats]
    # merge(stats_risks[i,], remaining_risks, by = c(similar, geo_level))
    nr <- nrow(concerned_risks)

    stats[,c(geo_level):=NULL]
    concerned_donors <- remaining_donors[stats]
    concerned_donors <- concerned_donors[concerned_donors[[geo_level]] != g,]
    nd <- nrow(concerned_donors)

    n_draw = if(nr <= nd) nr else nd

    orig <- concerned_risks$ident
    dest <- concerned_donors[ , .SD[sample(.N, n_draw)]]$ident

    res[[i]] <- data.table( orig = orig, dest = dest )
    remaining_donors <- remaining_donors[ ! ident %in% c(dest,orig), ]
    remaining_risks <- remaining_risks[ ! ident %in% c(dest,orig), ]

  }

  return(list(drawn_donors = rbindlist(res), remain_donors = remaining_donors, remain_risks = remaining_risks))

}


#' Title
#'
#' @param donors
#' @param risks
#' @param l_similar list of character vectors
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
#' donors_drawn <- draw_donors_multi_sim(donors, risks, l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")), geo_level = "geo")
draw_donors_multi_sim <- function(donors, risks, l_similar, geo_level){

  remaining_donors <- data.table::copy(donors)
  remaining_risks <- data.table::copy(risks)
  s = 0

  res <- as.list(rep(NA, length(l_similar)))

  while(nrow(remaining_risks) > 0 & nrow(remaining_donors) > 0 & s < length(l_similar)){
    s = s+1
    sim = l_similar[[s]]
    cat("Search of donors similar on : ", sim, "\n")

    stats_risks <- summary_risk(remaining_donors, remaining_risks, similar = sim, geo_level)
    matching <- draw_donors(remaining_donors, remaining_risks, stats_risks, similar = sim, geo_level)

    remaining_donors <- data.table::copy(matching$remain_donors)
    remaining_risks <- data.table::copy(matching$remain_risks)

    res[[s]] <- matching$drawn_donors
  }

  return(res)

}


