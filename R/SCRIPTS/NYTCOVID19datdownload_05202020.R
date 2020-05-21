## Data Download


# Downloading New York Times COVID-19 data for 05/20/2020
# dat <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
# write_csv(dat, "./R/DATA-RAW/COVID19-05202020.csv")
dat <- read_csv("./R/DATA-RAW/COVID19-05202020.csv") %>%
  filter(date == "2020-05-20")



# Total pop is variable H7Z001
dpdat <- read.xlsx("./R/DATA-PROCESSED/Summary_File.xlsx", sheet = 1)
dpdat2 <- dpdat %>%
  mutate(fips = paste0(STFIPS, CTYFIPS)) %>%
  dplyr::select(fips, name_dp, H7Z001_dp, H7Z001_sf)

combined <- left_join(dpdat2, dat) %>%
  mutate(caserat_dp = cases / H7Z001_dp,
         caserat_sf = cases / H7Z001_sf,
         fatrat_dp = deaths / H7Z001_dp,
         fatrat_sf = deaths / H7Z001_sf)

plot(combined$caserat_dp, combined$caserat_sf)
