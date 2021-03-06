# Figura 4 - Impacto da regra dos 3s no intervalo entre lances -
# Histogramas do intervalo entre lances de cobertura,
# antes e após a Regra dos 3s

library(tidyverse)
source('code/helpers/my_theme.R')
theme_set(my_theme())

# Carregando dados de lances --------------------------------------------------
df_bid_inc_unnested <- readRDS('data/comprasnet_bid_data_increments.rds')

# Excluindo "outliers" --------------------------------------------------------
cut <- quantile(df_bid_inc_unnested$intervalo_menor, 0.975)

df_intervalo <- df_bid_inc_unnested %>%
  filter(intervalo_menor < cut)

# Histograma ------------------------------------------------------------------
ggplot(df_intervalo) +
  geom_histogram(aes(x = intervalo_menor, y = ..density..),
                 alpha = 0.6, binwidth = 0.25, col = 'white',
                 fill = 'gray', size = .1, position = 'identity') +
  scale_x_continuous(breaks = c(3, seq(10, 40, by = 10))) +
  scale_y_continuous(
    labels = function(x) formatC(x, big.mark = '.', decimal.mark = ',')
    ) +
  coord_cartesian(xlim = c(0, 45)) +
  labs(
    x = 'Intervalo entre lances (segundos)',
    y = 'Densidade',
    title = 'Efeitos da regra dos 3s no intervalo entre lances',
    subtitle = 'Histogramas do intervalo entre lances de menor valor',
    caption = 'Nota: Os histogramas possuem uma longa cauda à direita, não exibida para melhor visualização.'
    ) +
  theme(text = element_text(family = 'serif'),
        strip.text = element_text(face = 'bold', size = 14),
        strip.background = element_rect(size = 1.2),
        panel.grid = element_blank(),
        axis.title = element_text(size = 14),
        plot.title = element_text(face = 'bold', size = 16),
        plot.subtitle = element_text(size = 13),
        plot.caption = element_text(hjust = 0, size = 10)) +
  facet_wrap(~ regime_juridico_3s, ncol = 1, nrow = 2)

ggsave('results/histograms/histograma_intervalo_3s.png', width = 9, height = 7)
