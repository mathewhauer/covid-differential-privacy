###------FigureRaceMultipanel-----
## @knitr FigureRaceMultipanel

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

fig2
# ggsave("./MANUSCRIPT/FIGURES/fig-race.png", fig2, width = 11, height = 7)

fig3 <- plot_grid(fig1, fig2, ncol=2, labels = "auto")
# fig3
# ggsave("./MANUSCRIPT/FIGURES/fig-all.pdf", fig3, width = 11, height = 4)
# ggsave("./MANUSCRIPT/FIGURES/fig-all.png", fig3, width = 11, height = 4)

# fig3 <- plot_grid(fig1, fig2, ncol=1, labels = "auto")
# ggsave("./MANUSCRIPT/FIGURES/fig-all-stacked.png", fig3, height=9, width = 7)