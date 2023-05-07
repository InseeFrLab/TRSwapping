library(data.table)
library(tictoc)

devtools::document()
# devtools::build()
devtools::load_all()

set.seed(123)
n = 1e6
donors <- create_data_example(n)
risks <- donors[sample(1:n, ceiling(n*0.07)),]
stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")

profvis::profvis({
  donors_drawn <- draw_donors(donors, risks, stats_risks, similar = c("edu", "sex"), geo_level = "geo")
})

#
# str(donors_drawn$drawn_donors)
# str(risks)
#
# str(donors_drawn$remain_risks)
#
#
#
# i = i+1
#
# # stats_risks <- summary_risk(remaining_donors, remaining_risks, similar = c("edu", "sex"), geo_level = "geo")
#
# g = stats_risks[[geo_level]][i]
#
# stats <- stats_risks[i,]
# setkeyv(stats, c(similar, geo_level))
# setkeyv(remaining_risks, c(similar, geo_level))
#
# concerned_risks1 <- stats_risks[remaining_risks]
# concerned_risks2 <- merge(stats_risks, remaining_risks, by = c(similar, geo_level))
