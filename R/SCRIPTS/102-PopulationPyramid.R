###------PopPyramid-----
## @knitr PopPyramid

source("./R/SCRIPTS/001-DataLoad.R")

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
# proj_sexage1$AGE2 <- factor(proj_sexage1$AGE2, levels = proj_sexage1$AGE2, labels = proj_sexage1$AGE2)

scalelabs <- c("00-09", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79",
               "80+")

collegecampuses<- proj_sexage1[which(proj_sexage1$fips %in% c("01001","48317", "50013",
                                                  "16029", "38087", "48011")),] %>%
  filter(sex != "Total")

ggplot(collegecampuses,aes(x= AGE2)) +
  geom_col(aes(y=sf2), color="black") +
      geom_segment(aes(x=AGE2, 
                   xend=AGE2, 
                   y=0, 
                   yend=dp2,
                   color = "2020")) +
  # scale_color_manual(name = "", values = c("2020" = "red")) +
  # scale_fill_manual(name = "", values = c("2100" = NA)) +
  geom_point(aes(y = dp2), color = "red") +
  theme_bw() +
  theme(legend.position = "none") +
    # legend.position = c(0.9, 0.3),
    #     legend.background = element_rect(fill=alpha('white', 0)))+
  # scale_y_continuous(limits = c(-16000000, 16000000),
  #   breaks = seq(-15000000, 15000000, 5000000), 
  #                    labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "m")) +
  scale_x_continuous(breaks = seq(1,9,1),
    sec.axis =dup_axis(),
                     labels = paste0(scalelabs),
    expand = c(0,0.1)) +
  coord_flip() +
  facet_wrap(. ~ fips, scales = "free") +
  # annotate("text", x=1, y =100, label ="Female") +
  # annotate("text", x=1, y =-100, label ="Male") +
  labs(y = "Population",
       x = "")
  


