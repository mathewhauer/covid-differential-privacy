###------Table1-----
## @knitr Table1

# Helper function to calculate the Median for all sf sizes below `popsize`
quantfunc <-function(popsize){
  return(quantile(abs(d2$ratio_rates[which(d2$sf < popsize)]), 0.5))
}

# Helper function to calculate the mean for all sf sizes below `popsize`
meanfunc <- function(popsize){
  return(mean(abs(d2$ratio_rates[which(d2$sf < popsize)])))
}

# Helper function to calculate the # of rows for all sf sizes below `popsize`.
calcsize <- function(popsize){
  nrow(d2[which(d2$sf <= popsize),])
}

# Creating the age dataset, selecting only Male and Female and not Total
d2 <- filter(dpage, sex %in% c("Male", "Female")) 

# Creating the dataframe to do the calculations
age <- data.frame(sizes = c(1000, 2500,5000, 10000, 20000, 10000000)) %>%
  group_by(sizes) %>%
  mutate(
    MEDIAN = quantfunc(sizes),
    MEAN = meanfunc(sizes),
    n = calcsize(sizes)
  )


##  TABLE DATA

table_age <- age %>%
  mutate(
    percentage = n / nrow(d2),
  )

# Creating the race dataset, deselecting the created "nonwhite" variable.
d2 <- filter(dprace, race !="nonwhite") 

race <- data.frame(sizes = c(1000, 2500,5000, 10000, 20000, 10000000)) %>%
  group_by(sizes) %>%
  mutate(
    MEDIAN = quantfunc(sizes),
    MEAN = meanfunc(sizes),
    n = calcsize(sizes)
  )

table_race <- race %>%
  mutate(
    percentage = n / nrow(d2),
  )

table_age
table_race


