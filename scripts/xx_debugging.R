

map_df(flood_out_list,2) -> test_map

#maybe add something to drop those with errors? but it is dropping a lot of them unnecessarily
# need to drop shoot what is causing the error, something with counting the seasons per year
#not sure which id is throwing the issue though

missing <- rice_id$full_id[!(rice_id$full_id %in% test_map$full_id)]

rlang::last_trace()

all_missing <- filter(flood_smooth, full_id %in%missing) |>
  group_by(full_id) |>
  group_split()


for(i in 100:110) {
  wrap_rice(all_missing[[i]])
  print(paste(all_missing[[i]]$full_id[1], "completed (i=", i, ")"))
}

err_id <- as.data.frame((all_missing[[105]]))

deriv1 <- deriv_tseries(err_id, print_plot = T)
stat1 <- estimate_season_stats(deriv1, print_plot = T)
avg1 <- calc_rice_avg(stat1)
