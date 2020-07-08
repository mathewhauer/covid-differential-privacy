source("./R/SCRIPTS/001-DataLoad.R")


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

d2 <- filter(dpage, sex %in% c("Male", "Female")) 

age <- data.frame(sizes = c(1000, 2500,5000, 10000, 20000, 10000000)) %>%
  group_by(sizes) %>%
  mutate(
    MAPE = quantfunc(sizes, 1.3),
    MEAN = meanfunc(sizes),
    RANGE = rangefunc(sizes),
    n = calcsize("80+", sizes)
  )


##  TABLE DATA

table_age <- age %>%
  mutate(
    percentage = n / nrow(d2),
  )

d2 <- filter(dprace, race !="nonwhite") 

race <- data.frame(sizes = c(1000, 2500,5000, 10000, 20000, 10000000)) %>%
  group_by(sizes) %>%
  mutate(
    MAPE = quantfunc(sizes, 1.3),
    MEAN = meanfunc(sizes),
    RANGE = rangefunc(sizes),
    n = calcsize("80+", sizes)
  )

table_race <- race %>%
  mutate(
    percentage = n / nrow(d2),
  )

