library(tidyverse)
source('code/helpers/my_theme.R')


# Carregando séries -----------------------------------------------------------
coffee_price_controls <-
  'data/control_variables_futures_and_arabica_prices.rds' %>% 
  readRDS()


# Notas sobre os dados --------------------------------------------------------

# Preço em reais de contratos futuros KC1:COM e saca de café arábica

# Os valores deflacionados estão em reais de Dez/2015 (IPCA).

# Série original de KC1:COM foi linearmente interpolada para preenchimento de
# missing observations - na série original não havia preços para as datas
# de todos os pregões considerados nos modelos de DD.
# O gráfico abaixo e modelos de DD usam a série interpolada (e deflacionada).

# Considerando os modelos DD restritos a SP: cerca de 5,4% das observações
# (ie, leilões) foram realizadas em datas para as quais não havia dados de
# contratos futuros. Portanto, esses modelos consideraram dados interpolados
# de contratos futuros em 5,4% das observações.

# Considerando os modelos DD para todo o Brasil: cerca de 6% das observações
# (ie, leilões) foram realizadas em datas para as quais não havia dados de
# contratos futuros. Portanto, esses modelos consideraram dados interpolados
# de contratos futuros em 6% das observações.

# Dados originais também disponíveis no DF para referência.
# Fontes: Bloomberg (futuros) e CEPEA/USP (saca de café arábica).
# Com as séries originais, é possível visualizar a distribuição dos
# missing values (e consequentemente, da interpolação) para a série KC1:COM
# ao longo de todas as datas, rodando as linhas abaixo:

coffee_price_controls %>%
  # Apenas periodo considerado nos modelos de DD
  filter(date >= '2011-03-01', date <= '2015-12-31') %>% 
  naniar::vis_miss()

# Portanto, realizou-se interpolação linear para apenas 1,8% das DATAS.
# O número é baixo, de forma que opção pela interpolação linear parece razoável.
# Os percentuais maiores indicados acima (5,4% e 6%) refletem a realização 
# de diversos leilões em pelo menos algumas dessas datas para as quais
# os dados de futuros foram interpolados.


# Colocando os dados no formato necessário ------------------------------------
df_plot <- coffee_price_controls %>% 
  select(date,
         futuro_defl = futures_KC1COM_defl_dez2015_ipca,
         arab_defl = arabica_defl_dez2015_ipca) %>% 
  filter(date >= '2011-03-01',
         date < '2016-01-01') %>% 
  pivot_longer(cols = -date,
               names_to = 'covariate', values_to = 'price') %>% 
  mutate(covariate = ifelse(covariate == 'arab_defl',
                            'Saca de café arábica', 'KC1:COM') %>% 
           fct_relevel('Saca de café arábica', 'KC1:COM'))

# Criando o gráfico -----------------------------------------------------------
theme_set(my_theme())

plot <- df_plot %>% 
  ggplot() +
  geom_point(aes(x = date, y = price, shape = covariate),
             alpha = .08) +
  geom_smooth(aes(x = date, y = price, linetype = covariate),
              se = FALSE, col = 'black', method = 'gam',
              formula = y ~ s(x, bs = "cs")) +
  labs(
    x = 'Data',
    y = 'Preço em reais'
  ) +
  theme(legend.title = element_blank(),
        legend.position = c(.8, .875)) ; plot

# Salvando gráfico em arquivo png ---------------------------------------------
ggsave(
  plot = plot, width = 5.5, height = 4.5,
  filename = 'results/plot-coffee-futures-and-arabica-prices-over-time.png'
  )