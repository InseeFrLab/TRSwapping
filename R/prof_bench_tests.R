# library(data.table)
#
# # devtools::document()
# # devtools::load_all("./", export_all = FALSE)
#
# set.seed(123)
# n = 1e3
# donors <- create_data_example(n)
# risks <- donors[sample(1:n, ceiling(n*0.07)),]
# # stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex","age"), geo_level = "geo")
# donors_geo = data.table::copy(donors); risks_geo = data.table::copy(risks)
#
# # donors_drawn <- draw_donors_one_geo_one_sim(donors_geo, risks_geo, stats_risks, similar = c("edu", "sex", "age"), geo_level = "geo")
# #
# # res <- donors_drawn$match
# # length(c(res$orig, res$dest)) == length(unique(c(res$orig, res$dest)))
# #
# # res_ss_na <- res[!is.na(res$orig),]
# # length(c(res_ss_na$orig, res_ss_na$dest)) == length(unique(c(res_ss_na$orig, res_ss_na$dest)))
#
#
# donors_drawn <- draw_donors_one_geo_multi_sim(
#   donors_geo, risks_geo, l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")), geo_level = "geo"
# )
#
# res <- rbindlist(donors_drawn)
# length(c(res$orig, res$dest)) == length(unique(c(res$orig, res$dest)))
#
#
# profvis::profvis({
#   donors_drawn <- draw_donors_one_geo_multi_sim(
#     donors_geo, risks_geo, l_similar = list(c("edu", "sex", "age"), c("age", "sex"), c("sex")), geo_level = "geo"
#   )
# })
#
# # system.time({
# #
# #   set.seed(123)
# #   n = 1e6
# #   donors <- create_data_example(n)
# #   risks <- donors[sample(1:n, ceiling(n*0.07)),]
# #   stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")
# #   donors_geo = data.table::copy(donors); risks_geo = data.table::copy(risks)
# #
# #
# #   ns <- nrow(stats_risks)
# #   cases <- 1:ns
# #   res <- list() #as.list(rep(NULL, ns))
# #
# #   # donors_geo <- data.table::copy(donors_geo)
# #   # remaining_risks <- data.table::copy(risks)
# #
# #   i = 1
# #
# #   while(nrow(risks_geo) > 0 & nrow(donors_geo) > 0 & i <= ns){
# #
# #     setkeyv(risks_geo, c(similar, geo_level, geo_level_sup))
# #     setkeyv(donors_geo, c(similar, geo_level_sup))
# #
# #     stats <- copy(stats_risks[i,])
# #     setkeyv(stats, c(similar, geo_level, geo_level_sup))
# #
# #     g <- stats[[geo_level]][1]
# #     g_sup <- if(is.null(geo_level_sup)) NULL else stats[[geo_level_sup]][1]
# #
# #     concerned_risks <- risks_geo[stats]
# #     # merge(stats_risks[i,], risks_geo, by = c(similar, geo_level))
# #     nr <- nrow(concerned_risks)
# #
# #     stats[,c(geo_level):=NULL]
# #     concerned_donors <- donors_geo[stats]
# #     setkeyv(concerned_donors, geo_level)
# #     concerned_donors <- concerned_donors[!J(g),]
# #     nd <- nrow(concerned_donors)
# #     setkey(concerned_donors, NULL)
# #
# #     if(nr == 0){
# #       res[[i]] <- NULL
# #     }else{
# #       if(nd == 0){
# #         res[[i]] <- NULL
# #       }else{
# #         n_draw <- min(nr, nd)
# #
# #         orig <- sample(concerned_risks$ident, n_draw)
# #         dest <- sample(concerned_donors$ident, n_draw)
# #
# #         res[[i]] <- data.table( orig = orig, dest = dest )
# #
# #         setkey(donors_geo, ident)
# #         donors_geo <- donors_geo[ !J(c(dest,orig)), ]
# #         setkey(risks_geo, ident)
# #         risks_geo <- risks_geo[ !J(c(dest,orig)), ]
# #       }
# #     }
# #
# #
# #     i <- i + 1
# #   }
# #
# # })
# #
# # system.time({
# #
# #   set.seed(123)
# #   n = 1e6
# #   donors <- create_data_example(n)
# #   risks <- donors[sample(1:n, ceiling(n*0.07)),]
# #   stats_risks <- summary_risk(donors, risks, similar = c("edu", "sex"), geo_level = "geo")
# #   donors_geo = data.table::copy(donors); risks_geo = data.table::copy(risks)
# #
# #
# #   ns <- nrow(stats_risks)
# #   cases <- 1:ns
# #   res <- list() #as.list(rep(NULL, ns))
# #
# #   # donors_geo <- data.table::copy(donors_geo)
# #   # remaining_risks <- data.table::copy(risks)
# #
# #   setkeyv(risks_geo, c(similar, geo_level, geo_level_sup))
# #   setkeyv(donors_geo, c(similar, geo_level_sup))
# #
# #   i = 1
# #
# #   while(nrow(risks_geo) > 0 & nrow(donors_geo) > 0 & i <= ns){
# #
# #
# #
# #     stats <- copy(stats_risks[i,])
# #     setkeyv(stats, c(similar, geo_level, geo_level_sup))
# #
# #     g <- stats[[geo_level]][1]
# #     g_sup <- if(is.null(geo_level_sup)) NULL else stats[[geo_level_sup]][1]
# #
# #     concerned_risks <- risks_geo[stats]
# #     # merge(stats_risks[i,], risks_geo, by = c(similar, geo_level))
# #     nr <- nrow(concerned_risks)
# #
# #     stats[,c(geo_level):=NULL]
# #     concerned_donors <- donors_geo[stats]
# #     setkeyv(concerned_donors, geo_level)
# #     concerned_donors <- concerned_donors[!J(g),]
# #     nd <- nrow(concerned_donors)
# #     setkey(concerned_donors, NULL)
# #
# #     if(nr == 0){
# #       res[[i]] <- NULL
# #     }else{
# #       if(nd == 0){
# #         res[[i]] <- NULL
# #       }else{
# #         n_draw <- min(nr, nd)
# #
# #         orig <- sample(concerned_risks$ident, n_draw)
# #         dest <- sample(concerned_donors$ident, n_draw)
# #
# #         res[[i]] <- data.table( orig = orig, dest = dest )
# #
# #         donors_geo <- donors_geo[ ! ident %in% c(dest,orig), ]
# #         risks_geo <- risks_geo[ ! ident %in% c(dest,orig), ]
# #       }
# #     }
# #
# #
# #     i <- i + 1
# #   }
# #
# # })
# #
# #
# # donors_drawn <- draw_donors_one_geo_one_sim(donors, risks, stats_risks, similar = c("edu", "sex"), geo_level = "geo")
