# library(httr)
# library(osmdata)
# library(data.table)
library(plotly)
library(dplyr)

solar_locs = RSocrata::read.socrata("https://data.calgary.ca/resource/csgq-e555.json")

solar_lat = c()
solar_lng = c()
solar_names = c()
solar_prod_all = data.frame()

for (i in 1:length(solar_locs)) {
  
  lat = solar_locs[[i]]$latitude
  lng = solar_locs[[i]]$longitude

  solar_name = solar_locs[[i]]$name

  solar_names = append(solar_names, solar_name)
  solar_lat = append(solar_lat, lat)
  solar_lng = append(solar_lng, lng)
  
  temp_df = RSocrata::read.socrata(paste0("https://data.calgary.ca/resource/ytdn-2qsp.json?name=", solar_name))
  
  if (nrow(temp_df > 0)) {
    solar_prod_all = plyr::rbind.fill(solar_prod_all, temp_df)
  }
}

solar_coords_df = data.frame(solar_names, solar_lat, solar_lng)

solar_df = dplyr::inner_join(solar_coords_df, solar_prod_all, by=c("solar_names" = "name"))

solar_df$kwh = as.numeric(solar_df$kwh)

solar_df_grouped = solar_df %>%
  mutate(week = lubridate::week(date)) %>%
  group_by(week, solar_lat, solar_lng, solar_names) %>%
  summarize(max_kwh = max(kwh))

# readr::write_csv(solar_df_grouped, "./data/data_files/solar_df_grouped.csv")
# readr::write_csv(solar_df, "./data/data_files/solar_df.csv")
