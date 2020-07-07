source("./R/SCRIPTS/001-DataLoad.R")

d2 <- filter(dpdat3, 
             # agegrp == agegrp1,
             # sf <= popsize,
             sex %in% c("Male", "Female")) 
quantfunc <-function(popsize, vals){
  return(quantile(abs(d2$ratio_rates[which(d2$sf < popsize)]), 0.5))
}
meanfunc <- function(popsize){
  return(mean(abs(d2$ratio_rates[which(d2$sf < popsize)])))
}
rangefunc <- function(popsize){
  return(
    paste0(
      min(abs(d2$ratio_rates[which(d2$sf <= popsize)]))," - ",
      max(abs(d2$ratio_rates[which(d2$sf <= popsize)]))
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

length(d2$ratio_rates[which(d2$mortrat_dp > 1)])

mean(abs(d2$ratio_rates[which(d2$sf<25000)]))
length(d2$ratio_rates[which(d2$sf<25000)])

mean(abs(d2$ratio_rates[which(d2$sf<50000)]))
length(d2$ratio_rates[which(d2$sf<50000)])

mean(abs(d2$ratio_rates[which(d2$sf<100000)]))
length(d2$ratio_rates[which(d2$sf<100000)])

mean(abs(d2$ratio_rates[which(d2$sf<500000)]))
length(d2$ratio_rates[which(d2$sf<500000)])

mean(abs(d2$ratio_rates))
length(d2$ratio_rates)


         
1-ecdf(abs(d2$ratio_rates[which(d2$sf <1000)]))(0.25)

ggplot(d2, aes(x =sf, y = mortrat_dp)) +
  geom_point() +
  theme_bw() +
  geom_hline(yintercept = 1) +
  # facet_grid(sex ~ agegrp) +
  scale_x_log10(labels=comma) +
  # yscale("log2", .format = TRUE)
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  labs(
    x = "2010 U.S. Census Summary File ",
    y = "COVID-19 Mortality Rate using DP as the denominator"
    # title = "Absolute Percent Errors using Differential Privacy for County-level COVID-19 mortality\nrates by sex/age"
    # caption = "Showing only those with Percent Errors less than 500%."
  )