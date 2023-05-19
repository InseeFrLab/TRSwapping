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
  all_drawn = c()

  # samp_donors <- donors_geo[, .SD[sample(.N, ceiling(.N*0.1))], by = c(similar, geo_level_sup)]
  # samp_donors <- unique(rbindlist(list(samp_donors, risks_geo[,.SD, .SDcols = names(samp_donors)]), use.names = TRUE))

  # donors_geo <- data.table::copy(donors_geo)
  # remaining_risks <- data.table::copy(risks)

  # setkeyv(risks_geo, c(similar, geo_level, geo_level_sup))
  # setkeyv(donors_geo, c(similar, geo_level_sup))

  i = 1

  while(nrow(risks_geo) > 0 & nrow(donors_geo) > 0 & i <= ns){

    stats <- copy(stats_risks[i,])
    setkeyv(stats, c(similar, geo_level, geo_level_sup))

    g <- stats[[geo_level]][1]
    g_sup <- if(is.null(geo_level_sup)) NULL else stats[[geo_level_sup]][1]

    concerned_risks <- risks_geo[!J(all_drawn),][stats, on = c(similar, geo_level, geo_level_sup), nomatch=NULL]
    # concerned_risks <- merge(
    #   stats,
    #   risks_geo[!J(all_drawn),],
    #   by = c(similar, geo_level, geo_level_sup),
    #   all.x = TRUE,
    #   all.y = FALSE
    # )
    nr <- nrow(concerned_risks)

    if(nr == 0){
      res[[i]] <- data.table( orig = NULL, dest = NULL )
    }else{

      stats[,c(geo_level):=NULL]
      concerned_donors <- donors_geo[
        !J(all_drawn),
      ][
        stats, on = c(similar, geo_level_sup), nomatch=NULL
      ]
      # concerned_donors <- merge(
      #   stats,
      #   donors_geo[],
      #   by = c(similar, geo_level_sup),
      #   all.x = TRUE,
      #   all.y = FALSE
      # )
      setkeyv(concerned_donors, geo_level)
      concerned_donors <- concerned_donors[!J(g),]
      # # setkey(concerned_donors, NULL)
      # # setkeyv(concerned_donors, c("ident"))
      # concerned_donors <- concerned_donors[!(all_drawn), on = "ident"]
      # setkey(concerned_donors, NULL)
      nd <- nrow(concerned_donors)


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
          geo_level_sup = ifelse(is.null(geo_level_sup), NA, geo_level_sup),
          similar = paste0(similar, collapse = ";")
        )

        all_drawn <- sort(unique(c(all_drawn, dest, orig)))
        # setkey(donors_geo, NULL)
        # setkeyv(donors_geo, c("ident"))
        # donors_geo <- donors_geo[ !J(all_drawn), ]
        # setkey(risks_geo, NULL)
        # setkeyv(risks_geo, c("ident"))
        # risks_geo <- risks_geo[ !J(all_drawn), ]
        # donors_geo <- donors_geo[ ! ident %in% all_drawn, ]
        # risks_geo <- risks_geo[ ! ident %in% all_drawn, ]
      }
    }

    i <- i + 1
  }
  gc()

  # return(list(match = data.table::rbindlist(res), risks_geo = risks_geo, donors_geo = donors_geo))
  return(data.table::rbindlist(res))

}

#' Title
#'
#' @param donors_geo data.table
#' @param risks_geo data.table
#' @param stats_risks data.table
#' @param similar character vector
#' @param geo_level character vector
#' @param geo_level_sup character vector
#'
#' @return data.table
#' @export
#'
#' @examples
#' library(data.table)
#' set.seed(123)
#' n = 1e3
#' data <- create_data_example(n, add_geo = TRUE)
#' data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), c("geo","geo2"))
#' risks <- data_prep$risks
#' donors <- data_prep$donors
#' stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo", geo_level_sup = "geo2")
#' donors_drawn <- draw_donors_one_geo_one_sim2(donors, risks, stats_risks, similar = c("edu", "sex"), geo_level = "geo", geo_level_sup = "geo2")
draw_donors_one_geo_one_sim2 <- function(donors_geo, risks_geo, stats_risks, similar, geo_level, geo_level_sup = NULL){

  risks_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
  donors_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
  stats_risks[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]

  risks_geo_c <- risks_geo[, .SD, .SDcols = c("ident", "sim", geo_level, geo_level_sup)]
  setnames(risks_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

  donors_geo_c <- donors_geo[, .SD, .SDcols = c("ident", "sim", geo_level, geo_level_sup)]
  setnames(donors_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

  stats_risks_c <- stats_risks[, .SD, .SDcols = c("sim", geo_level, geo_level_sup)]
  setnames(stats_risks_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

  m_stats <- as.matrix(stats_risks_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup")])
  m_risks <- as.matrix(risks_geo_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup","ident")])
  m_donors <- as.matrix(donors_geo_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup","ident")])

  # samp_donors <- s_donors[, .SD[sample(.N, ceiling(.N*0.1))], by = c("sim", "geo_sup")][,.SD,.SDcols = c("sim", "geo", "geo_sup","ident")]
  # samp_donors <- unique(rbindlist(list(samp_donors, s_risks)))

  # m_risks <- as.matrix(risks_geo_c)
  # m_donors <- as.matrix(donors_geo_c)

  # Tirer un premier echantillon de donneurs pour accélérer les calculs
  # Tirage à 10% (par exemple) par geo_sup*sim
  resC <- drawC(m_stats, m_risks, m_donors)

  return(rbindlist(resC))
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

  setkey(donors_geo, ident)
  setkey(risks_geo, ident)

  n_sim = length(l_similar)
  s = 1

  res <- replicate(n_sim, NULL, simplify = FALSE)

  while(nrow(risks_geo) > 0 & nrow(donors_geo) > 0 & s <= n_sim){

    similar = l_similar[[s]]
    cat("------- Search of donors similar on : ", similar, "\n")

    stats_risks <- summary_risk(donors_geo, risks_geo, similar, geo_level, geo_level_sup)
    res[[s]] <- draw_donors_one_geo_one_sim(donors_geo, risks_geo, stats_risks, similar, geo_level, geo_level_sup)

    donors_geo <- donors_geo[!J(c(res[[s]]$orig, res[[s]]$dest)),]
    risks_geo <- risks_geo[!J(c(res[[s]]$orig, res[[s]]$dest)),]

    # matching <- draw_donors_one_geo_one_sim(donors_geo, risks_geo, stats_risks, similar, geo_level, geo_level_sup)
    #
    # donors_geo <- data.table::copy(matching$donors_geo)
    # risks_geo <- data.table::copy(matching$risks_geo)

    # res[[s]] <- matching$match

    s = s+1
  }
  gc()

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
draw_donors_multi_geo_multi_sim <- function(donors, risks, l_similar, geo_levels, parallel = FALSE, n_cores = 2){

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

    if ( is.null(geo_sup) | isFALSE(parallel) ) {
      res[[j]] <- draw_donors_one_geo_multi_sim(
        donors_geo,
        risks_geo,
        l_similar,
        geo_level = geo,
        geo_level_sup = geo_sup
      )
    } else {
      setorderv(donors_geo, c(geo_sup))
      setorderv(risks_geo, c(geo_sup))

      l_donors <- split(donors_geo, by = geo_sup)
      l_risks <- split(risks_geo, by = geo_sup)
      no_risks <- setdiff(names(l_donors), names(l_risks))
      if (length(no_risks) > 0) l_donors <- l_donors[-which(names(l_donors) %in% no_risks)]

      l_donors <- lapply(l_donors, setkeyv, "ident")
      l_risks <- lapply(l_risks, setkeyv, "ident")

      oplan <- future::plan(future::multisession, workers = n_cores)
      on.exit(future::plan(oplan))

      opts <- furrr::furrr_options(
        globals = TRUE,
        packages = "TRSwapping",
        seed = TRUE
      )
      res[[j]] <- furrr::future_map2_dfr(
        l_donors,
        l_risks,
        draw_donors_one_geo_multi_sim,
        l_similar = l_similar,
        geo_level = geo,
        geo_level_sup = geo_sup,
        .options = opts
      )

      future::plan(future::sequential)

    }


    remaining_risks <- remaining_risks[!J(c(res[[j]]$orig, res[[j]]$dest)),]
    remaining_donors <- remaining_donors[!J(c(res[[j]]$orig, res[[j]]$dest)),]
    j = j + 1

    gc()
  }

  return(data.table::rbindlist(res))

}





