###------FigureMultipanel-----
## @knitr FigureMultipanel

# source("./R/SCRIPTS/001-DataLoad.R")

fig1 <- ggplot(data = dpage, aes(sf, abs(ratio_rates)*100)) +
  geom_point(aes(col = highlight)) +
  scale_color_manual(values=c("black", "red")) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  facet_grid(sex ~ agegrp) +
  coord_cartesian(ylim= c(0, 500), expand = TRUE) +
  scale_x_log10(labels=comma) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),
        legend.position = "none") +
  labs(
    x = "2010 U.S. Census Summary File ",
    y = "Absolute % Error")

fig1

# ggsave("./MANUSCRIPT/FIGURES/figure1.png", fig1, width = 11, height = 7)


fig2 <- ggplot(data = dprace, aes(sf, abs(ratio_rates)*100)) +
  geom_point(aes(col = highlight)) +
  scale_color_manual(values=c("black", "red")) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  facet_wrap(~ race) +
  coord_cartesian(ylim= c(0, 500), expand = TRUE) +
  scale_x_log10(labels=comma) +
  # scale_y_continuous(breaks = log2(c(0, 1, 2, 3)), labels = comma) +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1),
        legend.position = "none") +
  labs(
    x = "2010 U.S. Census Summary File ",
    y = "Absolute % Error")

# ggsave("./MANUSCRIPT/FIGURES/fig-race.png", fig2, width = 11, height = 7)