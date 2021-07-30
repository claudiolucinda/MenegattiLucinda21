# Figura 3 - Impacto da regra dos 20s no intervalo entre lances -
# Histogramas do intervalo entre de um mesmo fornecedor,
# antes e após a Regra dos 20s

library(tidyverse)
source('code/helpers/my_theme.R')
theme_set(my_theme())

# Carregando dados de lances --------------------------------------------------

df_intervalo_mesmo_fornecedor <-
  readRDS('data/comprasnet_bid_data_same_bidder.rds')

# Eliminando "outliers"  (2,5% maiores intervalos) ----------------------------
cut <- quantile(df_intervalo_mesmo_fornecedor$intervalo_proprio, 0.975)

df_int <- df_intervalo_mesmo_fornecedor %>%
  filter(intervalo_proprio < cut)


# Histograma ------------------------------------------------------------------
ggplot(df_int) +
  geom_histogram(aes(x = intervalo_proprio, y = ..density..),
                 binwidth = 0.5, alpha = 0.6, col = 'white',
                 fill = 'gray', size = .1, position = 'identity') +
  scale_x_continuous(breaks = seq(0, 120, by = 20)) +
  scale_y_continuous(
    labels = function(x) formatC(x, big.mark = '.', decimal.mark = ',')
    ) +
  coord_cartesian(xlim = c(0, 120)) +
  labs(
    x = 'Intervalo entre lances (segundos)',
    y = 'Densidade',
    title = 'Efeitos da regra dos 20s no intervalo entre lances',
    subtitle = 'Histogramas do intervalo entre lances sequenciais de um mesmo fornecedor',
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
  facet_wrap(~ regime_juridico_20s, ncol = 1, nrow = 2)

ggsave('results/histograms/histograma_intervalo_20s.png',
       width = 9, height = 7)
