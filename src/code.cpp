#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

// [[Rcpp::export]]
arma::uvec remove_idx_in_place(
    arma::uvec& idx,
    const arma::uvec& idx_to_remove
){

  int n = idx_to_remove.n_elem;
  arma::uvec rems;
  for(int i = 0; i < n; i++ ){
    rems = arma::find( idx == idx_to_remove( i ) );
    idx.shed_rows( rems );
  }
  return(idx);
}

// [[Rcpp::export]]
arma::vec concerned_select(
    const arma::mat& mat,
    const arma::uvec idx_drawn,
    int idx_len,
    int sim,
    int g,
    int g_sup,
    bool is_risk = true
){
  arma::uvec col_ident_idx = { 3 };
  arma::uvec select_idx;
  if(is_risk){
    select_idx = arma::find(
      (mat.col(0) == sim) &&
        (mat.col(1) == g)
    );
  }else{
    select_idx = arma::find(
      (mat.col(0) == sim) &&
        (mat.col(1) != g) &&
        (mat.col(2) == g_sup)
    );
  }
  if(idx_len > 0){
    remove_idx_in_place(select_idx, idx_drawn.rows(0, idx_len - 1));
  }
  arma::vec concerned = mat.submat(select_idx, col_ident_idx);

  return(concerned);
}

// [[Rcpp::export]]
arma::uvec rbind_idx_in_place(
    arma::uvec& idx,
    int n,
    const arma::uvec& new_idx
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
arma::uvec find_idx(
    const arma::vec& ident,
    const arma::vec& ident_drawn
){


  int n = ident_drawn.n_elem;
  arma::uvec idx(n, arma::fill::zeros);

  int j = 0;
  for(int i = 0; i < n; i++ ){
    arma::uvec add_idx = find( ident == ident_drawn(i) );
    if(add_idx.n_elem > 0){
      idx(j) = add_idx(0);
      j++;
    }
  }
  return(idx.rows(0,j-1));
}

// [[Rcpp::export]]
List drawC(
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

  arma::uvec r_idx_drawn(r_n); //r_n = max
  arma::uvec d_idx_drawn(r_n*2); //r_n*2 = max

  int r_idx_len = 0;
  int d_idx_len = 0;

  List res;

  int i = 0;

  while( (i < st_n) & (r_n > 0) & (d_n > 0)){

    int sim = st_mat(i, 0);
    int g = st_mat(i, 1);
    int g_sup = st_mat(i, 2);

    // arma::uvec select_risks = arma::find(
    //   (r_mat.cols(col_sim_idx) == sim) &&
    //     (r_mat.cols(col_geo_idx) == g)
    // );
    // if(r_idx_len > 0){
    //   remove_idx_in_place(select_risks, r_idx_drawn.rows(0, r_idx_len - 1));
    // }
    //
    // arma::vec concerned_risks = r_mat.submat(select_risks, col_ident_idx);
    arma::vec concerned_risks = concerned_select(r_mat, r_idx_drawn, r_idx_len, sim, g, g_sup);

    int nr = concerned_risks.n_elem; //127ms

    if(nr > 0){


      // arma::vec concerned_donors;
      // int l=0;
      // for(int k = 0; k < d_mat.n_rows; k++){
      //   if((d_mat(k, 0) == sim) && (d_mat(k, 1) != g) && (d_mat(k, 2) == g_sup)){//670ms
      //     // concerned_donors(l) = d_mat(k, 3);
      //     l++;
      //   }
      // }

      // arma::uvec select_donors = arma::find(
      //   (d_mat.cols(col_sim_idx) == sim) &&
      //     (d_mat.cols(col_geo_idx) != g) &&
      //     (d_mat.cols(col_geo_sup_idx) == g_sup)
      // ); //1.41s
      //   if(d_idx_len > 0){
      //     remove_idx_in_place(select_donors, d_idx_drawn.rows(0, d_idx_len - 1));
      //   }
      //
      //   arma::vec concerned_donors = d_mat.submat(select_donors, col_ident_idx);
      arma::vec concerned_donors = concerned_select(d_mat, d_idx_drawn, d_idx_len, sim, g, g_sup, false);

      int nd = concerned_donors.n_elem;//1.44s

      if(nd > 0){

        // Tirage des risques et donneurs
        int n_draw = std::min(nr, nd);

        arma::uvec idx_draw_risk = arma::randperm(nr, n_draw);
        arma::uvec idx_draw_donor = arma::randperm(nd, n_draw); //1.46

        arma::vec orig = concerned_risks(idx_draw_risk);
        arma::vec dest = concerned_donors(idx_draw_donor);//1.54s

        res.push_back(
          DataFrame::create(
            Named("orig") = orig,
            Named("dest") = dest
          )
        ); //1.62s

        // Maj de la liste des identifiants tirés rd_ident_drawn
        rbind_ident_in_place(rd_ident_drawn, rd_ident_n, orig);
        rd_ident_n = rd_ident_n + orig.n_elem;
        rbind_ident_in_place(rd_ident_drawn, rd_ident_n, dest);
        rd_ident_n = rd_ident_n + dest.n_elem;

        // Maj des index
        arma::uvec new_r_idx = find_idx(r_mat.cols(col_ident_idx), arma::join_cols(orig, dest));
        arma::uvec new_d_idx = find_idx(d_mat.cols(col_ident_idx), arma::join_cols(orig, dest));

        rbind_idx_in_place(r_idx_drawn, r_idx_len, new_r_idx);
        rbind_idx_in_place(d_idx_drawn, d_idx_len, new_d_idx);

        r_idx_len +=  new_r_idx.n_elem;
        r_n -= new_r_idx.n_elem;

        d_idx_len += new_d_idx.n_elem;
        d_n -= new_d_idx.n_elem;

      }
    }

    i++;
  }

  return(res);

}
//
// /*** R
// library(data.table)
// set.seed(123)
// n = 1e5
// data <- create_data_example(n, add_geo = TRUE)
// data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), c("geo","geo2"))
// risks_geo <- data_prep$risks
// donors_geo <- data_prep$donors
// stats_risks <- summary_risk(donors_geo, risks_geo, similar = c("edu", "sex"), geo_level = "geo", geo_level_sup = "geo2")
// similar = c("edu", "sex"); geo_level = "geo"; geo_level_sup = "geo2"
// risks_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
// donors_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
// stats_risks[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
//
// risks_geo_c <- risks_geo[, .SD, .SDcols = c("sim", geo_level, geo_level_sup, "ident")]
// setnames(risks_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))
//
// donors_geo_c <- donors_geo[, .SD, .SDcols = c("sim", geo_level, geo_level_sup, "ident")]
// setnames(donors_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))
//
// stats_risks_c <- stats_risks[, .SD, .SDcols = c("sim", geo_level, geo_level_sup)]
// setnames(stats_risks_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))
//
// m_stats <- as.matrix(stats_risks_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup")])
// m_risks <- as.matrix(risks_geo_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup","ident")])
// m_donors <- as.matrix(donors_geo_c[, lapply(.SD, as.integer), .SDcols=c("sim", "geo", "geo_sup","ident")])
//
// microbenchmark::microbenchmark(
//   res <- drawC(m_stats, m_risks, m_donors), times = 1
// )
// resdt <- rbindlist(res)
// length(unique(c(resdt$orig, resdt$dest))) == length(c(resdt$orig, resdt$dest))
//
// setdiff(resdt$orig, risks_geo_c$ident)
// setdiff(resdt$dest, donors_geo_c$ident)
// all(setdiff(resdt$orig, setdiff(donors_geo_c$ident, risks_geo_c$ident)) == resdt$orig)
//
// appar = merge(
//   merge(
//     resdt,
//     donors_geo_c[,.(ident, sim, geo, geo_sup)],
//     by.x = c("orig"),
//     by.y = "ident",
//     all.x = TRUE,
//     all.y = FALSE
//   ),
//   donors_geo_c[,.(ident, sim, geo, geo_sup)],
//   by.x = c("dest"),
//   by.y = "ident",
//   all.x = TRUE,
//   all.y = FALSE,
//   suffixes = c("","_dest")
// )
//
// appar[sim != sim_dest,]
// appar[geo_sup != geo_sup_dest,]
// appar[geo == geo_dest,]
//
// # rbind_ident_in_place(c(1:10,rep(0,10)), 0, 20:24)
// */
//


