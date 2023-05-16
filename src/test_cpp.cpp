#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

arma::uvec remove_idx_in_place(
    arma::uvec& idx,
    const arma::uvec& idx_to_remove
){

  int n = idx_to_remove.n_elem;
  for(int i = 0; i < n; i++ ){
    arma::uvec rems = find( idx == idx_to_remove( i ) );
    idx.shed_rows( rems );
  }
  return(idx);
}

arma::uvec rbind_idx_in_place(
    arma::uvec& idx,
    int n,
    arma::uvec& new_idx
){
  idx.rows(n, n + new_idx.n_elem -1) = new_idx;

  return(idx);
}

// [[Rcpp::export]]
arma::vec rbind_ident_in_place(
    arma::vec& ident,
    int n,
    const arma::vec& new_ident
){

  ident.rows(n, n + new_ident.n_elem -1) = new_ident;

  return(ident);
}


// [[Rcpp::export]]
List test_drawC(
    const arma::mat& st_mat,
    const arma::mat& r_mat,
    const arma::mat& d_mat
){

  int st_n = st_mat.n_rows;
  int r_n = r_mat.n_rows;
  int d_n = d_mat.n_rows;

  const arma::uvec col_sim_idx = { 0 };
  const arma::uvec col_geo_idx = { 1 };
  const arma::uvec col_geo_sup_idx = { 2 };
  const arma::uvec col_ident_idx = { 3 };

  arma::vec rd_ident_drawn(r_n*2);
  int rd_ident_n = 0;

  arma::uvec r_idx_drawn;
  arma::uvec d_idx_drawn;

  List res;
  int i = 0;

  while( (i < st_n) & (r_n > 0) & (d_n > 0)){

    int sim = as_scalar(st_mat.submat(i, 0, i, 0));
    int g = as_scalar(st_mat.submat(i, 1, i, 1));
    int g_sup = as_scalar(st_mat.submat(i, 2, i, 2));

    arma::uvec select_risks = find(
      (r_mat.cols(col_sim_idx) == sim) &&
        (r_mat.cols(col_geo_idx) == g)
    );
    remove_idx_in_place(select_risks, r_idx_drawn);

    arma::vec concerned_risks = r_mat.submat(select_risks, col_ident_idx);

    int nr = concerned_risks.n_elem;

    if(nr > 0){

      arma::uvec select_donors = find(
        (d_mat.cols(col_sim_idx) == sim) &&
          (d_mat.cols(col_geo_idx) != g) &&
          (d_mat.cols(col_geo_sup_idx) == g_sup)
      );
      remove_idx_in_place(select_donors, d_idx_drawn);

      arma::vec concerned_donors = d_mat.submat(select_donors, col_ident_idx);

      int nd = concerned_donors.n_elem;

      if(nd > 0){

        // Tirage des risques et donneurs
        int n_draw = std::min(nr, nd);

        IntegerVector draw_risk = (Rcpp::sample(nr, n_draw, false)-1);
        arma::uvec idx_draw_risk = as<arma::uvec>(wrap(draw_risk));

        IntegerVector draw_donor = (Rcpp::sample(nd, n_draw, false)-1);
        arma::uvec idx_draw_donor = as<arma::uvec>(wrap(draw_donor));

        arma::vec orig = concerned_risks(idx_draw_risk);
        arma::vec dest = concerned_donors(idx_draw_donor);

        res.push_back(
          DataFrame::create(
            Named("orig") = orig,
            Named("dest") = dest
          )
        );

        // Maj de la liste des identifiants tirés rd_ident_drawn
        rbind_ident_in_place(rd_ident_drawn, rd_ident_n, orig);
        rd_ident_n = rd_ident_n + orig.n_elem;
        rbind_ident_in_place(rd_ident_drawn, rd_ident_n, dest);
        rd_ident_n = rd_ident_n + dest.n_elem;


      }
    }


    i++;
  }

  return(res);

}

/*** R
library(data.table)
set.seed(123)
n = 1e3
data <- create_data_example(n, add_geo = TRUE)
data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), c("geo","geo2"))
risks_geo <- data_prep$risks
donors_geo <- data_prep$donors
stats_risks <- summary_risk(donors_geo, risks_geo, similar = c("edu", "sex"), geo_level = "geo", geo_level_sup = "geo2")
similar = c("edu", "sex"); geo_level = "geo"; geo_level_sup = "geo2"
risks_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
donors_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
stats_risks[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]

risks_geo_c <- risks_geo[, .SD, .SDcols = c("sim", geo_level, geo_level_sup, "ident")]
setnames(risks_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

donors_geo_c <- donors_geo[, .SD, .SDcols = c("sim", geo_level, geo_level_sup, "ident")]
setnames(donors_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

stats_risks_c <- stats_risks[, .SD, .SDcols = c("sim", geo_level, geo_level_sup)]
setnames(stats_risks_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

m_stats <- as.matrix(stats_risks_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup")])
m_risks <- as.matrix(risks_geo_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup","ident")])
m_donors <- as.matrix(donors_geo_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup","ident")])


test_drawC(m_stats, m_risks, m_donors)
*/
