my_theme <- function() {
  theme_bw() +
    theme(text = element_text(family = 'serif'),
          strip.text = element_text(face = 'bold', size = 14),
          strip.background = element_rect(size = 1.2),
          panel.grid = element_blank(),
          axis.title = element_text(size = 14),
          plot.title = element_text(face = 'bold', size = 16),
          plot.subtitle = element_text(size = 13),
          plot.caption = element_text(hjust = 0, size = 10))
}