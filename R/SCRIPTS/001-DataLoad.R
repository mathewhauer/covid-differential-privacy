###------DataLoad-----
## @knitr DataLoad

# 
# source("./R/SCRIPTS/000-Libraries.R")

if(!file.exists("../R/DATA-PROCESSED/nhgis_ppdd_20200527_county.zip")){
  download.file("http://assets.nhgis.org/differential-privacy/v20200527/nhgis_ppdd_20200527_county.zip",
                destfile = "../R/DATA-PROCESSED/nhgis_ppdd_20200527_county.zip")
  unzip("../R/DATA-PROCESSED/nhgis_ppdd_20200527_county.zip", exdir = "../R/DATA-PROCESSED")}


# Getting the differential privacy data into R
# dpdat <- read.xlsx("../R/DATA-PROCESSED/Summary_File.xlsx", sheet = 1)
dpdat <- read_csv("../R/DATA-PROCESSED/nhgis_ppdd_20200527_county.csv") %>%
  mutate(STFIPS = substr(gisjoin, 2,3),
         CTYFIPS = substr(gisjoin, 5,7),
         fips = paste0(STFIPS, CTYFIPS))
# The male and female variables by age. Creating this for later.
vars_male <- paste0("H",seq(76002,76025, 1))
vars_female   <-  paste0("H",seq(76027,76049, 1))           

# These are the age-sex specific mortality rates from the CDC. https://covid.cdc.gov/covid-data-tracker/#demographics
# They come from the US's age/sex specific COVID case and mortality data.
# Data were gathered on 10/15/2020 and were last updated on Oct 15 2020 12:52PM
# The data for these calculations are located at ./R/DATA-PROCESSED/CDC_COVID-10152020.xlsx
vars_agegrp <- tribble(
  ~var,	~sex,	~agegrp, ~fatrate,
  'H76003', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76004', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76005', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76006', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76007', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76008', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76009', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76010', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76011', 	'Male', 	'0-29', 	0.000637871654343519,
  'H76012', 	'Male', 	'30-39', 	0.00293167618861254,
  'H76013', 	'Male', 	'30-39', 	0.00293167618861254,
  'H76014', 	'Male', 	'40-49', 	0.00788569561861518,
  'H76015', 	'Male', 	'40-49', 	0.00788569561861518,
  'H76016', 	'Male', 	'50-64', 	0.0266711724169897,
  'H76017', 	'Male', 	'50-64', 	0.0266711724169897,
  'H76018', 	'Male', 	'50-64', 	0.0266711724169897,
  'H76019', 	'Male', 	'50-64', 	0.0266711724169897,
  'H76020', 	'Male', 	'65-74', 	0.0897207828864004,
  'H76021', 	'Male', 	'65-74', 	0.0897207828864004,
  'H76022', 	'Male', 	'65-74', 	0.0897207828864004,
  'H76023', 	'Male', 	'75-84', 	0.197094387074319,
  'H76024', 	'Male', 	'75-84', 	0.197094387074319,
  'H76025', 	'Male', 	'85+', 	0.342884309292267,
  'H76027', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76028', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76029', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76030', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76031', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76032', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76033', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76034', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76035', 	'Female', 	'0-29', 	0.000331531322701834,
  'H76036', 	'Female', 	'30-39', 	0.00129709261767221,
  'H76037', 	'Female', 	'30-39', 	0.00129709261767221,
  'H76038', 	'Female', 	'40-49', 	0.00330552006412572,
  'H76039', 	'Female', 	'40-49', 	0.00330552006412572,
  'H76040', 	'Female', 	'50-64', 	0.0134138523151203,
  'H76041', 	'Female', 	'50-64', 	0.0134138523151203,
  'H76042', 	'Female', 	'50-64', 	0.0134138523151203,
  'H76043', 	'Female', 	'50-64', 	0.0134138523151203,
  'H76044', 	'Female', 	'65-74', 	0.0570852075438223,
  'H76045', 	'Female', 	'65-74', 	0.0570852075438223,
  'H76046', 	'Female', 	'65-74', 	0.0570852075438223,
  'H76047', 	'Female', 	'75-84', 	0.13462619248955,
  'H76048', 	'Female', 	'75-84', 	0.13462619248955,
  'H76049', 	'Female', 	'85+', 	0.248673081639328,
  
  
  
)



# Going to do some transformations. This will convert wide-format to long-format.
dpdat2 <- dpdat %>%
  # mutate(fips = paste0(STFIPS, CTYFIPS)) %>%
  # mutate(STFIPS = substr(gisjoin, 2,3),
  #        CTYFIPS = substr(gisjoin, 5,7),
  #        fips = paste0(STFIPS, CTYFIPS)) %>%
  dplyr::select(fips, STFIPS, name, 
                H76002_dp:H76049_dp, H76002_sf:H76049_sf) %>%
  pivot_longer(cols = H76002_dp:H76049_sf, names_to = "race", values_to ="pop") %>%
  separate(race, into= c("var", "type"), sep = "_") %>%
  pivot_wider(names_from = type, values_from = "pop") %>%
# Then we join this data with the Italy data from Dowd et al, group by sex and age group, and aggregate
# into 10-year age groups.
  left_join(., vars_agegrp) %>%
  na.omit() %>%
  group_by(fips, STFIPS, name, sex, agegrp, fatrate) %>%
  dplyr::summarise(dp = sum(dp),
                   sf = sum(sf))
 
# Now we need to calculate a sex group for total. The fatality rates are manually entered from Dowd et al.
dpdatatot <- dpdat2 %>%
  group_by(fips, STFIPS, name, agegrp) %>%
  dplyr::summarise(dp = sum(dp),
                   sf = sum(sf)) %>%
  mutate(sex = "Total",
    fatrate = case_when(
    agegrp == "0-29" ~ 0.000478965,
    agegrp == "30-39" ~ 0.002106211,
    agegrp == "40-49" ~ 0.005536066,
    agegrp == "50-64" ~ 0.019962968,
    agegrp == "65-74" ~ 0.073500902,
    agegrp == "75-84" ~ 0.163174157,
    agegrp == "85+" ~ 0.279291731

  ))

# Joining the sex-specific and total data files. Then creating some variables.
dpage <- rbind(dpdat2, dpdatatot) %>%
   mutate(#fatrate = fatrate/100,
 deaths_sf = sf * fatrate * 0.70, 
 deaths_dp = dp * fatrate * 0.70,
 mortrat_dp = deaths_sf / dp, # These two calcs use the deaths from SF only since SF is the true population we would expect to die rather than the noise infused population.
 mortrat_sf = deaths_sf / sf,
 ratio_rates = (mortrat_dp / mortrat_sf)-1, # The ratio of DP fatality rates to SF fatality rates
 ratio_deaths = (deaths_dp / deaths_sf)-1,
 sf2 = log(sf),
 highlight = ifelse(mortrat_dp>1,"1","0") # This is used in Figure 1 to highlight mortality rates above 1.0
  )  
# omitting all NaN and Inf values
dpage[mapply(is.infinite, dpage)] <- NA 
dpage <- na.omit(dpage)

covidrace <- tribble(
  ~race,	~fatrat,
  'hsp',	0.0239969030325239,
  'natam',	0.027901543142795,
  'asian',	0.0612299359531905,
  'black',	0.0488483806163221,
  'nathawai',	0.0213393870601589,
  'white',	0.0490353738492005,
  'multi',	0.0371029727591274,
  'someother',	0.0371029727591274,
  'nonwhite',	0.0352370236565606,
  
  
)

# Creating the Race/Ethnicity dataset from the DP data.
dprace <- dpdat %>%
  # mutate(fips = paste0(STFIPS, CTYFIPS)) %>%
  # Renaming the variables to be more intuitive
  dplyr::select(fips, STFIPS, name, 
                white_dp = H7Z003_dp, 
                white_sf = H7Z003_sf,
                black_dp = H7Z004_dp, 
                black_sf = H7Z004_sf, 
                hsp_dp = H7Z010_dp, 
                hsp_sf = H7Z010_sf,
                natam_dp = H7Z005_dp, 
                natam_sf = H7Z005_sf, 
                asian_dp = H7Z006_dp, 
                asian_sf = H7Z006_sf,
                nathawai_dp = H7Z007_dp,
                nathawai_sf = H7Z007_sf,
                someother_dp = H7Z008_dp, 
                someother_sf = H7Z008_sf,
                multi_dp = H7Z009_dp, 
                multi_sf = H7Z009_sf) %>%
  # Creating a nonwhite category
  mutate(nonwhite_dp = black_dp + hsp_dp + natam_dp +asian_dp + nathawai_dp + someother_dp + multi_dp,
         nonwhite_sf = black_sf + hsp_sf + natam_sf +asian_sf + nathawai_sf + someother_sf + multi_sf) %>%
  # group_by(fips, name_dp) %>%
  pivot_longer(cols = white_dp:nonwhite_sf, names_to = "race", values_to ="pop") %>%
  separate(race, into= c("race", "type"), sep = "_") %>%
  pivot_wider(names_from = type, values_from = "pop") %>%
  left_join(., covidrace) %>%
  # Assuming a 5% mortality rate by race.
  mutate(cov_sf = (sf *fatrat * 0.70) / sf,
         cov_dp = ((sf *fatrat * 0.70) / dp),
         ratio_rates = (cov_dp / cov_sf)-1,
         highlight = ifelse((sf *fatrat)>dp,"1","0")) %>%
  na.omit 

# omitting all NaN and Inf values
dprace[mapply(is.infinite, dprace)] <- NA 
dprace <- na.omit(dprace)

dprace$race <- recode_factor(dprace$race, 
                             "black" = "Black Alone",
                             "natam" = "Am Ind and Alsk Nat Alone",
                             "asian" = "Asian Alone",
                             "nathawai" = "Nat Haw and Oth Pac Isld Alone",
                             "someother" = "Some other race Alone",
                             "multi" = "Two or more races",
                             "hsp" = "Hispanic",
                             "white" = "White Alone",
                             "nonwhite" = "Non-White"
)