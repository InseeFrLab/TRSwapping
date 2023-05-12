#' Title
#'
#' @param data data.table
#' @param var_risk name of a boolean var
#' @param var_scope_risk name of a character/num var
#' @param var_ident name of a char or num var
#' @param vars_similar names of all the similar vars (char or numeric variables)
#' @param vars_geo names of all the geo vars (char or num vars)
#'
#' @return list of two data.tables: donors and risks
#' @export
#'
#' @examples
#' set.seed(123)
#' n = 1e3
#' data <- create_data_example(n)
#' data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), "geo")
prepare_data <- function(
    data,
    var_risk,
    var_scope_risk,
    var_ident,
    vars_similar,
    vars_geo
){

  dt <- data.table::copy(data)
  setnames(dt, c(var_ident), "ident")
  setnames(dt, c(var_scope_risk), "scope_risk")
  setnames(dt, c(var_risk), "is_risky")

  # TODO convert char to num

  donors <- dt[, .SD, .SDcols = c("ident", vars_similar, vars_geo)]
  risks <- dt[ is_risky == TRUE, .SD, .SDcols = c("ident", vars_similar, vars_geo, "scope_risk")]

  return(
    list(donors = donors, risks = risks)
  )

}
