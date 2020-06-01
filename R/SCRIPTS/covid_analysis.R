
# Getting the differential privacy data into R
dpdat <- read.xlsx("./R/DATA-PROCESSED/Summary_File.xlsx", sheet = 1)

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
dpdat3 <- rbind(dpdat2, dpdatatot) %>%
   mutate(fatrate = fatrate/100,
 deaths_sf = sf * fatrate, 
 deaths_dp = dp * fatrate,
 mortrat_dp = deaths_sf / dp, # These two calcs use the deaths from SF only since SF is the true population we would expect to die rather than the noise infused population.
 mortrat_sf = deaths_sf / sf,
 ratio_rates = mortrat_dp / mortrat_sf, # The ratio of DP fatality rates to SF fatality rates
 ratio_deaths = deaths_dp / deaths_sf
  ) %>%
  na.omit %>% # omitting all NaN and Inf values
  filter(ratio_rates <=2.5) # filtering for DP fatality rates less than 150% above SF.

# deaths <- dpdat3 %>%
#   group_by(fips, STFIPS, name_dp) %>%
#   dplyr::summarise(dp = sum(dp),
#                    sf = sum(sf),
#                    deaths_dp = sum(deaths_dp),
#                    deaths_sf = sum(deaths_sf)) %>%
#   mutate( mortrat_dp = deaths_sf / dp,
#           mortrat_sf = deaths_sf / sf,
#           ratio_rates = (mortrat_dp / mortrat_sf)-1,
#           ratio_deaths = (deaths_dp / deaths_sf)-1,
# GEOID = fips)
# 
# classes <- classIntervals(deaths$ratio_deaths, n = 5, style = "jenks")
# classes$brks
# classes2 <- c("-0.6", "-0.05",
#               "0.05", "1.0", "2.0", "16.5")
# deaths <- deaths %>%
#   mutate(percent_class = cut(ratio_deaths, classes2, include.lowest = T))
# 
# # a<- get_acs(geography = "county",
# #             variables = "B19013_001",
# #             geometry = TRUE, shift_geo = TRUE)
# 
# counties <- left_join(a, deaths) %>%
#   na.omit
# 
# ggplot(subset(counties, !is.na(percent_class)), aes(
#   geometry = geometry,
#                      fill = percent_class)) + 
#   geom_sf(color = NA) + 
#   scale_fill_brewer(palette = "PuBu",
#                     name = "No qualifications (%)") +
#   # coord_sf(crs = 26911) + 
#   scale_fill_viridis_d(option = "magma")
# 
# ggplot(data = filter(deaths,ratio_rates <= 2.5), aes(log(sf), ratio_rates)) +
#   geom_point() +
#   geom_smooth() +
#   theme_bw() +
#   labs(x = "Log(SF Population)",
#        y = "Ratio(COVID-DP / COVID-SF)",
#        title = "Impact of Differential Privacy on County-level COVID-19 mortality rates",
#        caption = "Y-axis shows the ratio of aggregated age-sex specific mort rates using DP or SF in the denominator. 
#        Values below 1.0 suggest a DP-calculated mortality rate less than SF")



ggplot(data = filter(dpdat3,ratio_rates <= 2.5), aes(log(sf), ratio_rates)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  facet_grid(sex ~ agegrp) +
  scale_y_continuous(limits = c(0,2.6), expand = c(0, 0)) +
  labs(x = "Log(SF Population)",
       y = "Ratio(COVID-DP / COVID-SF)",
       title = "Impact of Differential Privacy on County-level COVID-19 mortality rates by sex/age",
       caption = "Showing only those with Ratios below 2.5
       Y-axis shows the ratio of using DP or SF in the denominator using age-sex specific mortality rates from Italy applied to SF. 
       Values below 1.0 suggest a DP-calculated mortality rate less than SF.")
