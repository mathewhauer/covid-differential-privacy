###------DataLoad-----
## @knitr DataLoad

# 
# source("./R/SCRIPTS/000-Libraries.R")

# Getting the differential privacy data into R
dpdat <- read.xlsx("../R/DATA-PROCESSED/Summary_File.xlsx", sheet = 1)

# The male and female variables by age. Creating this for later.
vars_male <- paste0("H",seq(76002,76025, 1))
vars_female   <-  paste0("H",seq(76027,76049, 1))           

# These are the age-sex specific mortality rates from Dowd et al. in PNAS.
# They come from Italy's age-sex specific data.
vars_agegrp <- tribble(
  ~var,	~sex,	~agegrp, ~fatrate,
  'H76003', 	'Male', 	'00-09', 	0,
  'H76004', 	'Male', 	'00-09', 	0,
  'H76005', 	'Male', 	'10-19', 	0,
  'H76006', 	'Male', 	'10-19', 	0,
  'H76007', 	'Male', 	'10-19', 	0,
  'H76008', 	'Male', 	'20-29', 	0.1,
  'H76009', 	'Male', 	'20-29', 	0.1,
  'H76010', 	'Male', 	'20-29', 	0.1,
  'H76011', 	'Male', 	'20-29', 	0.1,
  'H76012', 	'Male', 	'30-39', 	0.6,
  'H76013', 	'Male', 	'30-39', 	0.6,
  'H76014', 	'Male', 	'40-49', 	1.1,
  'H76015', 	'Male', 	'40-49', 	1.1,
  'H76016', 	'Male', 	'50-59', 	2.9,
  'H76017', 	'Male', 	'50-59', 	2.9,
  'H76018', 	'Male', 	'60-69', 	8.6,
  'H76019', 	'Male', 	'60-69', 	8.6,
  'H76020', 	'Male', 	'60-69', 	8.6,
  'H76021', 	'Male', 	'60-69', 	8.6,
  'H76022', 	'Male', 	'70-79', 	23.1,
  'H76023', 	'Male', 	'70-79', 	23.1,
  'H76024', 	'Male', 	'80+', 	34.4,
  'H76025', 	'Male', 	'80+', 	34.4,
  'H76027', 	'Female', 	'00-09', 	0,
  'H76028', 	'Female', 	'00-09', 	0,
  'H76029', 	'Female', 	'10-19', 	0,
  'H76030', 	'Female', 	'10-19', 	0,
  'H76031', 	'Female', 	'10-19', 	0,
  'H76032', 	'Female', 	'20-29', 	0,
  'H76033', 	'Female', 	'20-29', 	0,
  'H76034', 	'Female', 	'20-29', 	0,
  'H76035', 	'Female', 	'20-29', 	0,
  'H76036', 	'Female', 	'30-39', 	0.1,
  'H76037', 	'Female', 	'30-39', 	0.1,
  'H76038', 	'Female', 	'40-49', 	0.4,
  'H76039', 	'Female', 	'40-49', 	0.4,
  'H76040', 	'Female', 	'50-59', 	0.9,
  'H76041', 	'Female', 	'50-59', 	0.9,
  'H76042', 	'Female', 	'60-69', 	4.2,
  'H76043', 	'Female', 	'60-69', 	4.2,
  'H76044', 	'Female', 	'60-69', 	4.2,
  'H76045', 	'Female', 	'60-69', 	4.2,
  'H76046', 	'Female', 	'70-79', 	13.9,
  'H76047', 	'Female', 	'70-79', 	13.9,
  'H76048', 	'Female', 	'80+', 	21.2,
  'H76049', 	'Female', 	'80+', 	21.2,
)

# This is Dowd et al's data.
d<- read.xlsx("https://www.pnas.org/highwire/filestream/922167/field_highwire_adjunct_files/1/pnas.2004911117.sd01.xlsx", sheet = 1)

# Going to do some transformations. This will convert wide-format to long-format.
dpdat2 <- dpdat %>%
  mutate(fips = paste0(STFIPS, CTYFIPS)) %>%
  dplyr::select(fips, STFIPS, name_dp, 
                H76002_dp:H76049_dp, H76002_sf:H76049_sf) %>%
  pivot_longer(cols = H76002_dp:H76049_sf, names_to = "race", values_to ="pop") %>%
  separate(race, into= c("var", "type"), sep = "_") %>%
  pivot_wider(names_from = type, values_from = "pop") %>%
# Then we join this data with the Italy data from Dowd et al, group by sex and age group, and aggregate
# into 10-year age groups.
  left_join(., vars_agegrp) %>%
  group_by(fips, STFIPS, name_dp, sex, agegrp, fatrate) %>%
  dplyr::summarise(dp = sum(dp),
                   sf = sum(sf))
 
# Now we need to calculate a sex group for total. The fatality rates are manually entered from Dowd et al.
dpdatatot <- dpdat2 %>%
  group_by(fips, STFIPS, name_dp, agegrp) %>%
  dplyr::summarise(dp = sum(dp),
                   sf = sum(sf)) %>%
  mutate(sex = "Total",
    fatrate = case_when(
    agegrp == "00-09" ~ 0.0,
    agegrp == "10-19" ~ 0.0,
    agegrp == "20-29" ~ 0.1,
    agegrp == "30-39" ~ 0.3,
    agegrp == "40-49" ~ 0.7,
    agegrp == "50-59" ~ 2.0,
    agegrp == "60-69" ~ 7.1,
    agegrp == "70-79" ~ 19.8,
    agegrp == "80+" ~ 27.7
  ))

# Joining the sex-specific and total data files. Then creating some variables.
dpage <- rbind(dpdat2, dpdatatot) %>%
   mutate(fatrate = fatrate/100,
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


# Creating the Race/Ethnicity dataset from the DP data.
dprace <- dpdat %>%
  mutate(fips = paste0(STFIPS, CTYFIPS)) %>%
  # Renaming the variables to be more intuitive
  dplyr::select(fips, STFIPS, name_dp, 
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
  # Assuming a 5% mortality rate by race.
  mutate(cov_sf = (sf *0.05 * 0.70) / sf,
         cov_dp = ((sf *0.05 * 0.70) / dp),
         ratio_rates = (cov_dp / cov_sf)-1,
         highlight = ifelse((sf *0.05)>dp,"1","0")) %>%
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