#' Match similar risks-donors at one geographic level
#'
#' @param donors_geo data.table
#' @param risks_geo data.table
#' @param stats_risks data.table
#' @param similar character vector
#' @param geo_level character vector
#' @param geo_level_sup character vector
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
#' donors_geo = data.table::copy(donors); risks_geo = data.table::copy(risks)
#' donors_drawn <- draw_donors_one_geo_one_sim(donors, risks, stats_risks, similar = c("edu", "sex"), geo_level = "geo")
draw_donors_one_geo_one_sim <- function(donors_geo, risks_geo, stats_risks, similar, geo_level, geo_level_sup = NULL){

  ns <- nrow(stats_risks)
  cases <- 1:ns
  res <- list() #as.list(rep(NULL, ns))

  # donors_geo <- data.table::copy(donors_geo)
  # remaining_risks <- data.table::copy(risks)

  setkeyv(risks_geo, c(similar, geo_level, geo_level_sup))
  setkeyv(donors_geo, c(similar, geo_level_sup))

  i = 1

  while(nrow(risks_geo) > 0 & nrow(donors_geo) > 0 & i <= ns){

    stats <- copy(stats_risks[i,])
    setkeyv(stats, c(similar, geo_level, geo_level_sup))

    g <- stats[[geo_level]][1]
    g_sup <- if(is.null(geo_level_sup)) NULL else stats[[geo_level_sup]][1]

    concerned_risks <- risks_geo[stats][!is.na(ident),]
    # merge(stats_risks[i,], risks_geo, by = c(similar, geo_level))
    nr <- nrow(concerned_risks)

    if(nr == 0){
      res[[i]] <- data.table( orig = NULL, dest = NULL )
    }else{

      stats[,c(geo_level):=NULL]
      concerned_donors <- donors_geo[stats][!is.na(ident),]
      setkeyv(concerned_donors, geo_level)
      concerned_donors <- concerned_donors[!J(g),]
      nd <- nrow(concerned_donors)
      setkey(concerned_donors, NULL)

      if(nd == 0){
        res[[i]] <- data.table( orig = NULL, dest = NULL )
      }else{
        n_draw <- min(nr, nd)

        orig <- concerned_risks$ident[sample(1:nr, n_draw)]
        dest <- concerned_donors$ident[sample(1:nd, n_draw)]

        res[[i]] <- data.table( orig = orig, dest = dest )

        donors_geo <- donors_geo[ ! ident %in% c(dest,orig), ]
        risks_geo <- risks_geo[ ! ident %in% c(dest,orig), ]
      }
    }

    # print(res[[i]])
    i <- i + 1
  }

  return(list(match = rbindlist(res), risks_geo = risks_geo, donors_geo = donors_geo))

}


#' Title
#'
#' @param donors_geo data.table
#' @param risks_geo data.table
#' @param l_similar list of character vectors
#' @param geo_level character vctor
#' @param geo_level_sup character vector
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
#' donors_geo = data.table::copy(donors); risks_geo = data.table::copy(risks)
#' donors_drawn <- draw_donors_one_geo_multi_sim(donors_geo, risks_geo, l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")), geo_level = "geo")
draw_donors_one_geo_multi_sim <- function(donors_geo, risks_geo, l_similar, geo_level, geo_level_sup = NULL){

  # remaining_donors <- data.table::copy(donors)
  # remaining_risks <- data.table::copy(risks)
  n_sim = length(l_similar)
  s = 1

  res <- list()

  while(nrow(risks_geo) > 0 & nrow(donors_geo) > 0 & s <= n_sim){

    similar = l_similar[[s]]
    cat("Search of donors similar on : ", similar, "\n")

    stats_risks <- summary_risk(donors_geo, risks_geo, similar, geo_level)
    matching <- draw_donors_one_geo_one_sim(donors_geo, risks_geo, stats_risks, similar, geo_level, geo_level_sup)

    donors_geo <- data.table::copy(matching$donors_geo)
    risks_geo <- data.table::copy(matching$risks_geo)

    res[[s]] <- matching$match

    s = s+1
  }

  return(res)

}


