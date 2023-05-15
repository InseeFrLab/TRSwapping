#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List drawC(

    IntegerVector  st_sim,
    IntegerVector st_geo,
    IntegerVector st_geo_sup,

    IntegerVector r_ident,
    IntegerVector r_sim,
    IntegerVector r_geo,
    IntegerVector r_geo_sup,

    IntegerVector d_ident,
    IntegerVector d_sim,
    IntegerVector d_geo,
    IntegerVector d_geo_sup
){

  int st_n = st_sim.size();
  int r_n = r_ident.size();
  int d_n = d_ident.size();

  IntegerMatrix st_mat(st_n,3);
  st_mat(_,0) = st_sim;
  st_mat(_,1) = st_geo;
  st_mat(_,2) = st_geo_sup;

  IntegerMatrix r_mat(r_n,4);
  r_mat(_,0) = r_sim;
  r_mat(_,1) = r_geo;
  r_mat(_,2) = r_geo_sup;
  r_mat(_,3) = r_ident;

  IntegerMatrix d_mat(d_n,4);
  d_mat(_,0) = d_sim;
  d_mat(_,1) = d_geo;
  d_mat(_,2) = d_geo_sup;
  d_mat(_,3) = d_ident;

  List res;

  int i = 0;

  while( (i < st_n) & (r_n > 0) & (d_n > 0)){

    int sim = st_sim[i];
    int g = st_geo[i];
    int g_sup = st_geo_sup[i];

    LogicalVector select_risks = (r_sim == sim) & (r_geo == g);
    IntegerVector concerned_risks = r_ident[select_risks];

    int nr = concerned_risks.size();

    if(nr > 0){
      LogicalVector select_donors = (d_sim == sim) & (d_geo != g) & (d_geo_sup == g_sup);
      IntegerVector concerned_donors = d_ident[select_donors];

      int nd = concerned_donors.size();

      if(nd > 0){

        int n_draw = std::min(nr, nd);

        IntegerVector draw_risk = sample(nr, n_draw, false);
        IntegerVector draw_donor = sample(nd, n_draw, false);

        IntegerVector orig = concerned_risks[draw_risk-1];
        IntegerVector dest = concerned_donors[draw_donor-1];

        res.push_back(
          DataFrame::create(
            Named("orig") = orig,
            Named("dest") = dest
          )
        );

        IntegerVector orig_dest (orig.size() + dest.size());
        for(int k = 0; k < orig.size(); ++k) {
          orig_dest[k] = orig[k];
        }
        for(int k = 0; k < dest.size(); ++k) {
          orig_dest[k+orig.size()] = dest[k];
        }

        LogicalVector r_to_erase = in(r_ident,orig_dest);
        LogicalVector d_to_erase = in(d_ident,orig_dest);

        IntegerMatrix r_old = r_mat;
        IntegerMatrix r_mat(sum(!r_to_erase), r_old.ncol());

        for (int r = 0, s = 0; r < r_old.nrow(); r++) {
          if (!r_to_erase[r]) {
            r_mat(s, _) = r_old(r, _);
            s++;
          }
        }

        IntegerMatrix d_old = d_mat;
        IntegerMatrix d_mat(sum(!d_to_erase), d_old.ncol());

        for (int r = 0, s = 0; r < d_old.nrow(); r++) {
          if (!d_to_erase[r]) {
            d_mat(s, _) = d_old(r, _);
            s++;
          }
        }
      }
    }

    r_n = r_mat.nrow();
    d_n = d_mat.nrow();
    i++;
  }

  return(res);
}
