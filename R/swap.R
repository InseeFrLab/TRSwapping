#' Title
#'
#' @param data data.table
#' @param var_risk name of a boolean var
#' @param var_scope_risk name of a character/num var
#' @param var_ident name of a char or num var
#' @param l_similar list of character vector
#' @param geo_levels character vector
#'
#' @return list with two objects: swap = data.table, check = list
#' @export
#'
#' @examples
#' set.seed(123)
#' n = 1e3
#' data <- create_data_example(n)
#' res_swap <- swap(
#'   data,
#'   "is_risky",
#'   "scope_risk",
#'   "ident",
#'   l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")),
#'   geo_levels = "geo"
#' )
swap <- function(
    data,
    var_risk,
    var_scope_risk,
    var_ident,
    l_similar,
    geo_levels
){

  vars_similar <- unique(unlist(l_similar))

  data_prep <- prepare_data(
    data,
    var_risk,
    var_scope_risk,
    var_ident,
    vars_similar,
    geo_levels
  )
  risks <- data_prep$risks
  donors <- data_prep$donors

  rm(data_prep); gc(verbose = FALSE);

  donors_drawn <- draw_donors_multi_geo_multi_sim(
    donors,
    risks,
    l_similar,
    geo_levels
  )
  check_res <- check_swap(risks, donors, donors_drawn)

  return(list(swap = donors_drawn, check = check_res))
}
