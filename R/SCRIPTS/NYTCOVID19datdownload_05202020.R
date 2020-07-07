## Data Download


# Downloading New York Times COVID-19 data for 05/20/2020
# dat <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
# write_csv(dat, "./R/DATA-RAW/COVID19-05202020.csv")
dat <- read_csv("./R/DATA-RAW/COVID19-05202020.csv") %>%
  filter(date == "2020-05-20")



# Total pop is variable H7Z001
dpdat <- read.xlsx("./R/DATA-PROCESSED/Summary_File.xlsx", sheet = 1)
dpdat2 <- dpdat %>%
  mutate(fips = paste0(STFIPS, CTYFIPS),
  # dplyr::select(fips, name_dp, 
  #               H76022_dp, H76022_sf,
  #               H76023_dp, H76023_sf,
  #               H76024_dp, H76024_sf,
  #               H76025_dp, H76025_sf,
  #               H76046_dp, H76046_sf,
  #               H76047_dp, H76047_sf,
  #               H76048_dp, H76048_sf,
  #               H76049_dp, H76049_sf,
  #               ) %>%
  over70m_dp = H76022_dp +H76023_dp +H76024_dp +H76025_dp,
  over70f_dp = H76046_dp + H76047_dp +H76048_dp +H76049_dp,
  over70m_sf = H76022_sf +H76023_sf +H76024_sf +H76025_sf,
  over70f_sf =  H76046_sf + H76047_sf +H76048_sf +H76049_sf,
  perdiff_m = over70m_sf / over70m_dp,
  fatalitiesm_dp = (over70m_sf * 0.6 * 0.12) / over70m_dp ,
  fatalitiesm_sf = (over70m_sf * 0.6 * 0.12) / over70m_sf,
  ratrate = fatalitiesm_dp / fatalitiesm_sf) %>%
  dplyr::select(fips, name_dp, over70m_dp, over70m_sf,over70f_dp, over70f_sf,
                perdiff_m, fatalitiesm_dp,fatalitiesm_sf, ratrate)

dpdat2 <- dpdat %>%
  mutate(fips = paste0(STFIPS, CTYFIPS),
         oth_dp = (H7Z006_dp + H7Z007_dp +H7Z008_dp +H7Z009_dp),
         oth_sf = (H7Z006_sf + H7Z007_sf +H7Z008_sf +H7Z009_sf)) %>%
  dplyr::select(fips, STFIPS, name_dp, 
                white_dp = H7Z003_dp, white_sf = H7Z003_sf,
                black_dp = H7Z004_dp, black_sf = H7Z004_sf, 
                hsp_dp = H7Z010_dp, hsp_sf = H7Z010_sf,
                oth_dp, oth_sf, 
                asian_dp = H7Z006_dp, asian_sf = H7Z006_sf,
                nathawai_dp = H7Z007_dp,nathawai_sf = H7Z007_sf,
                someother_dp = H7Z008_dp, someother_sf = H7Z008_sf,
                multi_dp = H7Z009_dp, multi_sf = H7Z009_sf) %>%
  mutate(nonwhite_dp = black_dp + hsp_dp + oth_dp + asian_dp + nathawai_dp + someother_dp + multi_dp,
                nonwhite_sf = black_sf + hsp_sf + oth_sf + asian_sf + nathawai_sf + someother_sf + multi_sf) %>%
  # group_by(fips, name_dp) %>%
  pivot_longer(cols = white_dp:nonwhite_sf, names_to = "race", values_to ="pop") %>%
  separate(race, into= c("race", "type"), sep = "_") %>%
  pivot_wider(names_from = type, values_from = "pop") %>%
  mutate(cov_sf = (sf *0.05) / sf,
         cov_dp = ((sf *0.05) / dp),
         ratio = (cov_dp / cov_sf)-1) %>%
  na.omit 

# omitting all NaN and Inf values
dpdat2[mapply(is.infinite, dpdat2)] <- NA 
dpdat2 <- na.omit(dpdat2)
# 
# combined <- left_join(dpdat2, dat) %>%
#   mutate(caserat_dp = cases / H7Z001_dp,
#          caserat_sf = cases / H7Z001_sf,
#          fatrat_dp = deaths / H7Z001_dp,
#          fatrat_sf = deaths / H7Z001_sf)

plot(log(dpdat2$ratio), log(dpdat2$sf))
plot(log(dpdat2$sf), log(dpdat2$ratio))

ggplot(data = dpdat2, aes(cov_dp, cov_sf)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  facet_wrap(~ race) +
  labs(x = "Log(SF Population)",
       y = "Log(COVID-DP / COVID-SF)")

ggplot(data = dpdat2, aes(sf, abs(ratio)*100)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  scale_x_log10(labels=comma) +
  coord_cartesian(ylim= c(0, 500), expand = TRUE) +
  facet_wrap(~ race) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  # scale_y_continuous(limits = c(0,2.6), expand = c(0, 0)) +
  labs(
    x = "2010 U.S. Census Summary File ",
    y = "Absolute % Error"
    # title = "Absolute Percent Errors using Differential Privacy for County-level COVID-19 mortality\nrates by sex/age"
    # caption = "Showing only those with Percent Errors less than 500%."
  )

d2 <- dpdat2

quantfunc <-function(popsize, vals){
  return(quantile(abs(d2$ratio[which(d2$sf < popsize)]), 0.5))
}
meanfunc <- function(popsize){
  return(mean(abs(d2$ratio[which(d2$sf < popsize)]), na.omit = T))
}
rangefunc <- function(popsize){
  return(
    paste0(
      min(abs(d2$ratio[which(d2$sf <= popsize)]))," - ",
      max(abs(d2$ratio[which(d2$sf <= popsize)]))
    )
  )
}
calcsize <- function(agegrp1, popsize){
  nrow(d2[which(d2$sf <= popsize),])
}

quantfunc("80+", a, 1.3)
a <- data.frame(sizes = 
                  # seq(1000,10000, 1000)
                  c(1000, 2500,5000, 10000, 20000, 1000000)
) %>%
  group_by(sizes) %>%
  mutate(
    MAPE = quantfunc(sizes, 1.3),
    MEAN = meanfunc(sizes),
    RANGE = rangefunc(sizes),
    # without30percent =  quantfunc("80+", sizes, 1.3),
    n = calcsize("80+", sizes)
  )

nrow(d2)

##  TABLE DATA

a <- a %>%
  mutate(
    percentage = n / nrow(d2),
  )
