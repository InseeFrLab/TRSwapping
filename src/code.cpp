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


// arma::uvec new_idx_drawn(
//     const arma::mat& mat,
//     arma::vec& new_ident_drawn
// ){
//
//   arma::uvec idx(new_ident_drawn.n_elem);
//   int j=0;
//
//   for(int i = 0; i < new_ident_drawn.n_elem; i++){
//     arma::uvec adds = find(mat.col(3) == new_ident_drawn(i));
//     if(adds.n_elem > 0){
//       idx(j) = adds(0);
//       j++;
//     }
//   }
//   return(idx.rows(0,j-1));
// }

arma::uvec rbind_idx(
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
int length_inter(
    const arma::mat& mat,
    const arma::uvec& col_idx,
    const arma::vec& vec
){
  arma::vec col_mat = mat.cols(col_idx);
  arma::vec inter = arma::intersect(col_mat, vec);
  return( mat.n_rows - inter.n_elem );
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

        //TODO: MAJ des index r_idx et d_idx
        // Maj de la liste des identifiants tirés rd_ident_drawn
        rbind_ident_in_place(rd_ident_drawn, rd_ident_n, orig);
        rd_ident_n = rd_ident_n + orig.n_elem;
        rbind_ident_in_place(rd_ident_drawn, rd_ident_n, dest);
        rd_ident_n = rd_ident_n + dest.n_elem;

      }
    }

    // Maj des nbs de risques et donneurs restants
    r_n = length_inter(r_mat, col_ident_idx, rd_ident_drawn);
    d_n = length_inter(d_mat, col_ident_idx, rd_ident_drawn);
    i++;
  }

  return(res);
}



