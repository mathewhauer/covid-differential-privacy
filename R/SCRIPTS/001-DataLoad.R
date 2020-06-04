
source("./R/SCRIPTS/000-Libraries.R")

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
 ratio_rates = (mortrat_dp / mortrat_sf)-1, # The ratio of DP fatality rates to SF fatality rates
 ratio_deaths = (deaths_dp / deaths_sf)-1,
 sf2 = log(sf)
  )  
# omitting all NaN and Inf values
dpdat3[mapply(is.infinite, dpdat3)] <- NA 
dpdat3 <- na.omit(dpdat3)



# classes <- classIntervals(dpdat3$ratio_deaths, n = 5, style = "jenks")
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


# 
# top<- ggplot(data = dpdat3, aes(sf, abs(ratio_rates)*100)) +
#   geom_point() +
#   geom_smooth(se = FALSE) +
#   theme_bw() +
#   facet_grid(sex ~ agegrp) +
#   coord_cartesian(ylim= c(0, 500), expand = TRUE) +
#   scale_x_log10(labels=comma) +
#   theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
#   # scale_y_continuous(limits = c(0,2.6), expand = c(0, 0)) +
#   labs(
#     x = "True Population",
#        y = "Absolute % Error",
#        title = "Absolute Percent Errors using Differential Privacy for County-level COVID-19 mortality\nrates by sex/age"
#        # caption = "Showing only those with Percent Errors less than 500%."
#        )
# 
# bot <- ggplot(data=dpdat3, aes(sf, group = agegrp)) + 
#   facet_grid(. ~ agegrp, scales="free_x") +
#   # geom_density() +
#   # geom_vline(aes(xintercept=median)) +
#   # geom_segment(aes(x = 0, y = 0.5, xend = quantile(sf, 0.5),  yend = 0.5)) +
#   # geom_segment(aes(x = quantile(sf, 0.5), y = 0.5, xend = quantile(sf, 0.5),  yend = -Inf)) +
#   # geom_ribbon(aes(ymin=0, ymax=a_mean$median, fill=quantile(sf, 0.5))) +
# geom_line(aes(y = 1 - ..y..), stat='ecdf') +
#   geom_ribbon(data=temp.below, aes(x=sf, ymin=0,ymax=ecdf), alpha=0.5) +
#   # geom_ribbon(aes(ymin=0, ymax=1-..y.., fill=quantile(sf, prob=0.5)))
#   # stat_ecdf(aes( ymin=0,ymax=0.5),geom="ribbon", alpha=0.5) +
#   # stat_ecdf(geom = "step", aes(1-..y..)) +
#   # facet_grid(. ~ agegrp) +
#   scale_x_log10(labels=comma,
#                 expand = c(0,0)
#                 # breaks =setbreaks
#                 ) +
#   coord_cartesian(xlim = c(10,1000000)) +
#   theme_bw() +
#   scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
#   theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),
#         strip.text = element_blank(),
#         rect = element_blank(),
#         panel.border = element_rect(colour = "black")) +
#   labs(x = "True Population",
#        y = "")
# 
# plot_grid(top, bot, align = "v", axis = "lr",ncol=1,
#           rel_heights = c(2.5,1),
#           labels = "auto")
# 
#   d2 <- filter(dpdat3, agegrp == agegrp1,
#                sex %in% c("Male", "Female"))
#  
# d2 <- filter(dpdat3, 
#                # agegrp == agegrp1,
#                # sf <= popsize,
#                sex %in% c("Male", "Female")) 
# quantfunc <-function(popsize, vals){
#   return(quantile(abs(d2$ratio_rates[which(d2$sf <= popsize)]), 0.5))
# }
# meanfunc <- function(popsize){
#   return(mean(abs(d2$ratio_rates[which(d2$sf <= popsize)])))
# }
# rangefunc <- function(popsize){
#   return(
#     paste0(
#       min(abs(d2$ratio_rates[which(d2$sf <= popsize)]))," - ",
#       max(abs(d2$ratio_rates[which(d2$sf <= popsize)]))
#       )
#   )
# }
# calcsize <- function(agegrp1, popsize){
#   nrow(d2[which(d2$sf <= popsize),])
# }
# 
# quantfunc("80+", a, 1.3)
# a <- data.frame(sizes = seq(1000,10000, 1000)) %>%
#   group_by(sizes) %>%
#   mutate(
#     MAPE = quantfunc(sizes, 1.3),
#     MEAN = meanfunc(sizes),
#     RANGE = rangefunc(sizes),
#     # without30percent =  quantfunc("80+", sizes, 1.3),
#          n = calcsize("80+", sizes)
#     )
# 
# nrow(d2)
# a <- a %>%
#   mutate(
#     percentage = n / nrow(d2),
#          )
# 
# length(d2$ratio_rates[which(d2$mortrat_dp > 1)])
# ggplot(data=a, aes(x=sizes, y = MAPE)) + 
#   geom_line() +
#   theme_bw() +
#   geom_vline(xintercept = 512) +
#   geom_vline(xintercept = 1222) +
#   labs(x = "Median Absolute Percent Error",
#        y = "Population Size")
# 
# plot(log(d2$deaths_dp), log(d2$deaths_sf))