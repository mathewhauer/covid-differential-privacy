source("./R/SCRIPTS/001-DataLoad.R")

fig1 <- ggplot(data = dpdat3, aes(sf, abs(ratio_rates)*100)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  theme_bw() +
  facet_grid(sex ~ agegrp) +
  coord_cartesian(ylim= c(0, 500), expand = TRUE) +
  scale_x_log10(labels=comma) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1)) +
  # scale_y_continuous(limits = c(0,2.6), expand = c(0, 0)) +
  labs(
    x = "2010 U.S. Census Summary File ",
    y = "Absolute % Error"
    # title = "Absolute Percent Errors using Differential Privacy for County-level COVID-19 mortality\nrates by sex/age"
    # caption = "Showing only those with Percent Errors less than 500%."
  )

ggsave("./MANUSCRIPT/FIGURES/figure1.png", fig1, width = 11, height = 7)
