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
a <- data.frame(sizes = seq(1000,10000, 1000)) %>%
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

