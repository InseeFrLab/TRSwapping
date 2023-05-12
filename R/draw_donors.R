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
#' data <- create_data_example(n)
#' data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), "geo")
#' risks <- data_prep$risks
#' donors <- data_prep$donors
#' stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")
#' donors_drawn <- draw_donors_one_geo_one_sim(donors, risks, stats_risks, similar = c("edu", "sex"), geo_level = "geo")
draw_donors_one_geo_one_sim <- function(donors_geo, risks_geo, stats_risks, similar, geo_level, geo_level_sup = NULL){

  ns <- nrow(stats_risks)
  cases <- 1:ns
  res <- replicate(ns, NULL, simplify = FALSE)

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

        res[[i]] <- data.table(
          orig = orig,
          dest = dest,
          geo_level = geo_level,
          geo_level_sup = geo_level_sup,
          similar = paste0(similar, collapse = ";")
        )

        donors_geo <- donors_geo[ ! ident %in% c(dest,orig), ]
        risks_geo <- risks_geo[ ! ident %in% c(dest,orig), ]
      }
    }

    i <- i + 1
  }

  return(list(match = data.table::rbindlist(res), risks_geo = risks_geo, donors_geo = donors_geo))

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
#' data <- create_data_example(n)
#' data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), "geo")
#' risks <- data_prep$risks
#' donors <- data_prep$donors
#' donors_drawn <- draw_donors_one_geo_multi_sim(donors, risks, l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")), geo_level = "geo")
draw_donors_one_geo_multi_sim <- function(donors_geo, risks_geo, l_similar, geo_level, geo_level_sup = NULL){

  n_sim = length(l_similar)
  s = 1

  res <- replicate(n_sim, NULL, simplify = FALSE)

  while(nrow(risks_geo) > 0 & nrow(donors_geo) > 0 & s <= n_sim){

    similar = l_similar[[s]]
    cat("------- Search of donors similar on : ", similar, "\n")

    stats_risks <- summary_risk(donors_geo, risks_geo, similar, geo_level)
    matching <- draw_donors_one_geo_one_sim(donors_geo, risks_geo, stats_risks, similar, geo_level, geo_level_sup)

    donors_geo <- data.table::copy(matching$donors_geo)
    risks_geo <- data.table::copy(matching$risks_geo)

    res[[s]] <- matching$match

    s = s+1
  }

  return(data.table::rbindlist(res))

}


#' Title
#'
#' @param donors data.table
#' @param risks data.table
#' @param l_similar list of character vector
#' @param geo_levels character vector
#'
#' @return a data.table
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(123)
#' n = 1e3
#' data <- create_data_example(n)
#' data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), "geo")
#' risks <- data_prep$risks
#' donors <- data_prep$donors
#' donors_drawn <- draw_donors_multi_geo_multi_sim(donors, risks, l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")), geo_levels = "geo")
#' check_res <- check_swap(risks, donors, donors_drawn)
draw_donors_multi_geo_multi_sim <- function(donors, risks, l_similar, geo_levels){

  remaining_donors <- data.table::copy(donors)
  setkey(remaining_donors, ident)
  remaining_risks <- data.table::copy(risks)
  setkey(remaining_risks, ident)

  n_geo = length(geo_levels)
  j = 1

  res <- replicate(n_geo, NULL, simplify = FALSE)

  while(nrow(remaining_donors) > 0 & nrow(remaining_risks) > 0 & j <= n_geo){

    geo <- geo_levels[j]
    cat("---- Search of donors for geo_level : ", geo, "\n")

    geo_sup <- if(j == n_geo) NULL else geo_levels[j+1]

    scopes <- purrr::map_chr(1:j, \(i) geo_levels[i])
    risks_geo <- remaining_risks[scope_risk %in% scopes,]

    exclude_risks_as_donors <- remaining_risks[! scope_risk %in% scopes,]$ident
    donors_geo <- remaining_donors[!J(exclude_risks_as_donors),]

    res[[j]] <- draw_donors_one_geo_multi_sim(
      donors_geo,
      risks_geo,
      l_similar,
      geo_level = geo,
      geo_level_sup = geo_sup
    )

    remaining_risks <- remaining_risks[!J(c(res[[j]]$orig, res[[j]]$dest)),]
    remaining_donors <- remaining_donors[!J(c(res[[j]]$orig, res[[j]]$dest)),]
    j = j + 1
  }

  return(data.table::rbindlist(res))

}





