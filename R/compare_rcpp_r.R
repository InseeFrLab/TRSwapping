library(Rcpp)
library(RcppArmadillo)

cppFunction('List draw(List l) {
  CharacterVector spec = as<CharacterVector>(l["Species"]);
  NumericVector sep = as<NumericVector>(l["Sepal.Length"]);
  int n = spec.size();
  DataFrame res;
  NumericVector sel;
  for(int i = 0; i < n; ++i) {
    if(spec[i] == "virginica"){
      sel.push_back(sep[i]);
    }
  }
  int mini = std::min(n,12);
  res["sel"] = sel;
  res["Spec"] = "virginica";
  IntegerVector a = {1,2,3,4,5,6,7,8};
  IntegerVector b = {3,7,12};
  LogicalVector c = in(a,b);
  IntegerVector ex = IntegerVector::create(0,2);
  NumericMatrix m(2,2);
  m(_,0) = ex;
  m(_,1) = ex;
  res["mat"] = m;
  res["bool"] = c;
  return res;
}')
# LogicalVector t = sel == ex;

draw(iris)
as.data.table(draw(iris))

microbenchmark::microbenchmark(
  draw(iris),
  iris[iris$Species == "virginica","Sepal.Length"]
)


irisdt <- as.data.table(iris)
useDF(irisdt)


cppFunction('List draw(

  IntegerVector st_sim,
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

  while(i < st_n & r_n > 0 & d_n > 0){

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
}')

library(data.table)
set.seed(123)
n = 1e6
data <- create_data_example(n, add_geo = TRUE)
data_prep <- prepare_data(data, "is_risky", "scope_risk", "ident", c("edu", "sex", "age"), c("geo","geo2"))
risks <- data_prep$risks
donors <- data_prep$donors
stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo", geo_level_sup = "geo2")

tic()
donors_drawn <- draw_donors_one_geo_one_sim(donors, risks, stats_risks, similar = c("edu", "sex"), geo_level = "geo")
toc()

risks_geo <- data.table::copy(risks)
donors_geo <- data.table::copy(donors)

risks_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
donors_geo[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]
stats_risks[, sim := as.integer(do.call(paste, c(.SD, sep=""))), .SDcols= c(similar)]

risks_geo_c <- risks_geo[, .SD, .SDcols = c("ident", "sim", geo_level, geo_level_sup, "scope_risk")]
setnames(risks_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

donors_geo_c <- donors_geo[, .SD, .SDcols = c("ident", "sim", geo_level, geo_level_sup)]
setnames(donors_geo_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))

stats_risks_c <- stats_risks[, .SD, .SDcols = c("sim", geo_level, geo_level_sup)]
setnames(stats_risks_c, c(geo_level, geo_level_sup), c("geo", "geo_sup"))


tic()
st_sim = stats_risks_c$sim
st_geo = stats_risks_c$geo
st_geo_sup = stats_risks_c$geo_sup

r_ident = risks_geo_c$ident
r_sim = risks_geo_c$sim
r_geo = risks_geo_c$geo
r_geo_sup = risks_geo_c$geo_sup

d_ident = donors_geo_c$ident
d_sim = donors_geo_c$sim
d_geo = donors_geo_c$geo
d_geo_sup = donors_geo_c$geo_sup

resC <- draw(
  st_sim,
  st_geo,
  st_geo_sup,

  r_ident,
  r_sim,
  r_geo,
  r_geo_sup,

  d_ident,
  d_sim,
  d_geo,
  d_geo_sup
)
resC <- rbindlist(resC)
toc()

length(c(resC$orig, resC$dest)) == length(unique(c(resC$orig, resC$dest)))




