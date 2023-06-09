% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swap.R
\name{swap}
\alias{swap}
\title{Title}
\usage{
swap(
  data,
  var_risk,
  var_scope_risk,
  var_ident,
  l_similar,
  geo_levels,
  parallel = FALSE,
  n_cores = 2
)
}
\arguments{
\item{data}{data.table}

\item{var_risk}{name of a boolean var}

\item{var_scope_risk}{name of a character/num var}

\item{var_ident}{name of a char or num var}

\item{l_similar}{list of character vector}

\item{geo_levels}{character vector}

\item{parallel}{logical}

\item{n_cores}{integer}
}
\value{
list with two objects: swap = data.table, check = list
}
\description{
Title
}
\examples{
set.seed(123)
n = 1e3
data <- create_data_example(n)
res_swap <- swap(
  data,
  "is_risky",
  "scope_risk",
  "ident",
  l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")),
  geo_levels = "geo"
)
# Exemple 2
n = 1e5
data <- create_data_example(n, add_geo = TRUE)
res_swap <- swap(
  data,
  var_risk = "is_risky",
  var_scope_risk = "scope_risk",
  var_ident = "ident",
  l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")),
  geo_levels = c("geo", "geo2")
)
}
