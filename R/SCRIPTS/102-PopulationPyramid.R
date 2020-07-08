###------PopPyramid-----
## @knitr PopPyramid

source("./R/SCRIPTS/001-DataLoad.R")

fipslist <- read_csv(file="https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", col_names = FALSE) %>%
  mutate(GEOID = paste0(X2, X3)) %>%
  dplyr::rename(state = X1,
                STATEID = X2,
                CNTYID = X3,
                NAME = X4) %>%
  filter(!STATEID %in% c("60", "66", "69", "72", "74", "78")) %>% # filtering out the outerlying areas.
  group_by(STATEID, state) %>%
  dplyr::summarise(STFIPS = STATEID) %>%
  unique()


proj_sexage1 <- dpdat2 %>%
  mutate(dp2 = ifelse(sex=="Male", -1*dp, dp),
         sf2 = ifelse(sex=="Male", -1*sf,sf),
         AGE2 = case_when(
           agegrp ==  "00-09" ~1,
           agegrp ==  "10-19" ~2,
           agegrp ==  "20-29" ~3,
           agegrp ==  "30-39" ~4,
           agegrp ==  "40-49" ~5,
           agegrp ==  "50-59" ~6,
           agegrp ==  "60-69" ~7,
           agegrp ==  "70-79" ~8,
           agegrp ==  "80+" ~9
         )
) %>%
  na.omit

scalelabs <- c("00-09", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
               "80+")

poppyramids<- proj_sexage1[which(proj_sexage1$fips %in% c("01001","48317", "50013",
                                                  "16029", "38087", "48011")),] %>%
  filter(sex != "Total") %>%
  group_by(fips, sex) %>%
  mutate(sf3 = sf2 / sum(sf2),
         dp3 = dp2 / sum(dp2),
         sf3 = ifelse(sex=="Male", -1*sf3, sf3),
         dp3 = ifelse(sex=="Male", -1*dp3, dp3)) %>%
  left_join(., fipslist) %>%
  mutate(County = paste0(name_dp,", ", state)) %>%
  I()

pyramids <- ggplot(poppyramids,aes(x= AGE2)) +
  geom_col(aes(y=sf3, fill = "Summary File"), color = "black") +
      geom_segment(aes(x=AGE2, 
                   xend=AGE2, 
                   y=0, 
                   yend=dp3,
                   color = "Differential Privacy")) +
  
  geom_point(aes(y = dp3, color = "Differential Privacy")) +
  scale_fill_manual(name = "", values = c("Summary File" = "dark grey")) +
  scale_color_manual(name = "", values = c("Differential Privacy" = "red")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=seq(-.4,.4,.2),labels=percent(abs(seq(-.40,.40,.2)))) +
  scale_x_continuous(breaks = seq(1,9,1),
    sec.axis =dup_axis(),
                     labels = paste0(scalelabs),
    expand = c(0,0.1)) +
  coord_flip() +
  facet_wrap(. ~ County) +
  annotate("text", x=1, y =.3, label ="Female") +
  annotate("text", x=1, y =-.4, label ="Male") +
  labs(y = "% of Population",
       x = "")
  
ggsave("./MANUSCRIPT/FIGURES/fig-pyramids.png", pyramids, width = 11, height = 7)

