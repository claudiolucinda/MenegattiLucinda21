library(tidyverse)
library(PregoesBR)
source('code/helpers/geom_flat_violin.R')
source('code/helpers/my_theme.R')

theme_set(my_theme())

data_20s <- as.Date('2012-01-17')
data_3s <- as.Date('2014-01-02')

# Loading data ----------------------------------------------------------------

bec <- readRDS('data/dd_sp.rds') %>% 
  filter(comprasnet == 0)

cnet <- readRDS('data/dd_brasil.rds') %>% 
  filter(comprasnet == 1)

cnet_sp <- readRDS('data/dd_sp.rds') %>% 
  filter(comprasnet == 1)

# Data wrangling --------------------------------------------------------------
data_list <- list(bec, cnet, cnet_sp) %>%
  map(
    .f = ~ .x %>% 
      mutate_if(is.factor, as.character) %>%
      mutate(regime_juridico = case_when(
        abertura_lances < data_20s ~ 'Sem intervalo mínimo',
        abertura_lances >= data_20s & abertura_lances < data_3s ~ 'Regra 20s',
        abertura_lances >= data_3s ~ 'Regra 20s + Regra 3s'
        ) %>% fct_relevel('Sem intervalo mínimo',
                          'Regra 20s',
                          'Regra 20s + Regra 3s'))
  ) %>% set_names(c('bec', 'cnet', 'cnet_sp'))

df_stacked <-  data_list$bec %>%
  mutate(Grupo = "Controle") %>%
  bind_rows(data_list$cnet %>% mutate(Grupo = "Tratamento")) %>%
  bind_rows(data_list$cnet_sp %>% mutate(Grupo = "Tratamento - SP"))

# Quantidade: Summary statistics ----------------------------------------------
summary_table_quantidade <- df_stacked %>%
  group_by(Grupo) %>%
  summarise(N = n(),
            Média = mean(kg_fornecidos, na.rm = TRUE),
            Mediana = median(kg_fornecidos, na.rm = TRUE),
            SD = sd(kg_fornecidos, na.rm = TRUE)) ; summary_table_quantidade

# Quantidade -------------------------------------------------------------------

# Violinplot
df_stacked %>% 
  ggplot(aes(x = Grupo, y = kg_fornecidos)) +
  geom_flat_violin() + 
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  scale_x_discrete(labels = c('BEC', 'Comprasnet', 'Comprasnet SP')) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000),
                labels = c('10', '100', '1 mil', '10 mil', '100 mil')) +
  geom_text(data = summary_table_quantidade,
            aes(x = Grupo, y = Mediana, label = Mediana),
            size = 3, nudge_x = 0.17, family = 'serif') +
  labs(
    x = 'Grupo',
    y = 'Quilogramas fornecidos (escala logarítmica)',
    title = 'Distribuição da quantidade negociada nos pregões',
    subtitle = 'Comparação entre grupos de tratamento e controle',
    caption = 'Notas:
    1) Os valores no gráfico representam as medianas de cada grupo;
    2) Pregões utilizados nas especificações principais, realizados entre mar./2011 e dez./2015.'
  )

Contr = df_stacked$kg_fornecidos[df_stacked$Grupo=="Controle"]
Treat = df_stacked$kg_fornecidos[df_stacked$Grupo=="Tratamento"]
TreatSP= df_stacked$kg_fornecidos[df_stacked$Grupo=="Tratamento - SP"]

ks.test(Contr, Treat)

# ggsave('results/violinplot-quantity.png', width = 6, height = 6)

# Valor total -----------------------------------------------------------------

# Summary statistics
summary_table_valor <- df_stacked %>%
  group_by(Grupo) %>%
  summarise(N = n(),
            Média = mean(kg_fornecidos * win_bid_kg, na.rm = TRUE),
            Mediana = median(kg_fornecidos * win_bid_kg, na.rm = TRUE),
            SD = sd(kg_fornecidos * win_bid_kg,
                    na.rm = TRUE)) ; summary_table_valor

# Violinplot
df_stacked %>% 
  ggplot(aes(x = Grupo, y = kg_fornecidos * win_bid_kg/ 1e+3)) +
  geom_flat_violin() + 
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  scale_x_discrete(labels = c('BEC', 'Comprasnet', 'Comprasnet SP')) +
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000),
                labels = c('0,1', '1', '10', '100', '1000')) +
  geom_text(data = summary_table_valor,
            aes(x = Grupo, y = Mediana / 1e+3,
                label = round(Mediana/1e+3, digits = 2) %>% 
                  str_replace('\\.', ',')),
            size = 3, nudge_x = 0.17, family = 'serif') +
  labs(
    x = 'Grupo',
    y = 'Valor total (milhares de reais, escala logarítmica)',
    title = 'Distribuição do valor negociado nos pregões',
    subtitle = 'Comparação entre grupos de tratamento e controle',
    caption = 'Notas:
    1) Os valores no gráfico representam as medianas de cada grupo;
    2) Pregões utilizados nas especificações principais, realizados entre mar./2011 e dez./2015.'
  )

Contr = df_stacked$win_bid_kg[df_stacked$Grupo=="Controle"]
Treat = df_stacked$win_bid_kg[df_stacked$Grupo=="Tratamento"]
TreatSP= df_stacked$win_bid_kg[df_stacked$Grupo=="Tratamento - SP"]

ks.test(Contr, Treat)


# ggsave('plots/violinplot-total-auction-value.png', width = 6, height = 6)


# Quantidade por regime juridico -----------------------------------------------
# Nota para Lucinda: Dados para Figura 3 do artigo <<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Summary Statistics
summary_table_quantidade_regime <- df_stacked %>%
  group_by(Grupo, regime_juridico) %>%
  summarise(N = n(),
            Média = mean(kg_fornecidos, na.rm = TRUE),
            Mediana = median(kg_fornecidos, na.rm = TRUE),
            Máximo = max(kg_fornecidos, na.rm = TRUE),
            SD = sd(kg_fornecidos,
                    na.rm = TRUE)) ; summary_table_quantidade_regime

# Violinplot

# Nota para Lucinda <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Figura 3 do artigo

# Note que o gráfico abaixo é marginalmente diferente daquela apresentado 
# na dissertação (e na versão atual do artigo).

# Isso porque o gráfico da dissertação considerava uma amostra ligeiramente
# diferente daquela utilizada nos modelos de DD. Mais especificamente:

# 1) o gráfico da dissertação considerava algumas poucas observações
# (31 leilões BEC) para as quais temos os dados necessários para construir
# o gráfico abaixo, mas que não podem ser usadas no DD em razão de
# missing values na variável de número de bidders; e

# 2) no código utilizado para gerar o gráfico original (dissertação),
# removi 1,25% das observações em cada extremo da distribuição de preços,
# em cada um dos seguintes subconjuntos: leilões Comprasnet Brasil,
# leilões Comprasnet SP e leilões BEC (ie, tirei 2,5% dos leilões de cada).
# Pelo que consegui retomar agora, nos dados utilizados no DD, removi apenas
# 0,75% de cada lado e apenas nos leilões do Comprasnet (Brasil) - não fiz
# limpeza semelhante nos dados da BEC, tampouco nos leilões Comprasnet
# utilizados nos modelos restritos a SP, pois nesses conjuntos de leilões
# não havia observações com valores extremos. Talvez esse ponto não tenha
# sido explicado em detalhe na dissertação. Posso confirmar os valores e
# podemos conversar caso entenda necessario explicar melhor no artigo.

# Como as conclusões referentes ao gráfico se mantêm com essas
# alterações, me parece que seria melhor utilizar a versão atualizada (abaixo)
# para fins de consistência com os dados utilizados no DD.

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

df_stacked %>% 
  ggplot(aes(x = Grupo, y = kg_fornecidos)) +
  geom_flat_violin() + 
  geom_boxplot(width = 0.2, outlier.shape = NA) +
  scale_x_discrete(labels = c('BEC', 'Comprasnet', 'Comprasnet SP')) +
  scale_y_log10(breaks = c(10, 100, 1000, 10000, 100000),
                labels = c('10', '100', '1 mil', '10 mil', '100 mil')) +
  geom_text(data = summary_table_quantidade_regime,
            aes(x = Grupo, y = Mediana, label = Mediana),
            size = 2.5, nudge_x = 0.35, family = 'serif') +
  labs(
    x = 'Grupo',
    y = 'Quilogramas fornecidos (escala logarítmica)',
    title =
      'Distribuição da quantidade negociada nos pregões',
    subtitle = 
      'Comparação entre grupos de tratamento e controle, nos diferentes regimes jurídicos',
    caption = 'Notas:
    1) Os valores no gráfico representam as medianas de cada grupo;
    2) Pregões utilizados nas especificações principais, realizados entre mar./2011 e dez./2015.'
  ) +
  theme(axis.text.x = element_text(angle = 25, size = 9, hjust = 1),
        axis.title.x = element_blank()) +
  facet_grid(. ~ regime_juridico)

ggsave('results/violinplot-supplied-quantity-kg-faceted-by-rules.png',
       width = 7, height = 6)