## Data Download


# Downloading New York Times COVID-19 data for 05/20/2020
dat <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

write_csv(dat, "./R/DATA-RAW/COVID19-05202020.csv")