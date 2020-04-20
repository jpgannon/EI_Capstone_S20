daily_data <- well_df %>% 
  group_by(year(well_df$date), month(well_df$date), day(well_df$date))
