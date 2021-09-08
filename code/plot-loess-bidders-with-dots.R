library(tidyverse)
library(lubridate)
source('code/helpers/trim_df.R')
source('code/helpers/my_theme.R')
source('code/helpers/formatar_numero.R')

theme_set(my_theme())

data_20s <- as.Date('2012-01-17')
data_3s <- as.Date('2014-01-02')

# Considerações sobre os dados e o período considerado ------------------------

# Nesse gráfico, considera-se um período um pouco mais extenso do que
# aquele considerado nos modelos DD - aqui começa 2 meses antes e termina 
# 2 meses depois (ou seja, aqui são considerados leilões de realizados entre
# 01/01/2011 e 01/02/2016). Motivo: minimizar "instabilidade" nos extremos
# da regressão não-paramétrica, em razão do pouco número de observações.

# Como o período é diferente do recorte utilizado na preparação das bases
# dd_sp.rds e dd_brasil.rds, aqui partimos de dados mais completos
# (porém menos limpos e organizados) dos leilões BEC e Comprasnet

# Carregando dados ------------------------------------------------------------

# Leilões BEC
bec <- readRDS('data/coffee_auctions_bec.rds') %>% 
  mutate(abertura_lances = as.Date(dt_inicio), Plataforma = 'BEC') %>%
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances,
         Plataforma, kg_fornecidos) %>% 
  filter(abertura_lances > '2011-01-01', abertura_lances < '2016-02-01') %>%
  # Removendo duas observações com valores claramente errados
  # id_item 801013801002015OC000450001 - win_bid_kg de R$ 49410
  # id_item 090173000012011OC002560001 - win_bid_kg de R$ 11535
  filter(win_bid_kg < 80)

# Leilões Comprasnet - Brasil
cnet <- readRDS('data/coffee_auctions_comprasnet.rds') %>% 
  mutate(abertura_lances = as.Date(abertura_lances)) %>% 
  select(id_item, abertura_lances, win_bid_kg, num_forn_lances,
         sigla_uf, kg_fornecidos) %>%
  filter(abertura_lances > '2011-01-01', abertura_lances < '2016-02-01')

# Leilões Comprasset - Apenas leilões de UASGs com sede em SP
cnet_sp <- filter(cnet, sigla_uf == 'SP')

# Assim como na preparação dos dados para uso nos modelos de DD, corta-se
# 0,75% das observações com valores mais extremos de win_bid_kg, apenas para
# os leilões da Comprasnet, na amostra completa (todo o Brasil). No caso da
# BEC e da Comprasnet restrita a SP esse tratamento não é necessário, pois
# não há observações com preços "estranhos" (na verdade, no caso da BEC há
# alguns, mas são poucos e tão extremos que é seguro excluí-los manualmente -
# conforme feito acima e na preparação para as bases utilizadas para DD).

cnet <- trim_df(cnet, 'win_bid_kg', perc = 1.5)

# Preparando dados ------------------------------------------------------------
plot_list <- map(
  .x = list(cnet, cnet_sp),
  .f = ~ select(.x, -sigla_uf) %>% 
    mutate(Plataforma = 'Comprasnet') %>% 
    bind_rows(bec) %>%
    mutate(Plataforma = fct_relevel(Plataforma, 'Comprasnet', 'BEC')) 
) %>% set_names(c('brasil', 'sp'))

df_plot <- bind_rows(
  # Variavel que sera utilizada para faceting
  plot_list$brasil %>% mutate(amostra = 'Amostra completa'),
  plot_list$sp %>% mutate(amostra = "Apenas Estado de São Paulo")
) %>% 
  mutate(
    Plataforma = factor(Plataforma, levels = c("Comprasnet", "BEC")),
    # Variavel que identifica regime juridico e plataforma (6 valores)
    grupo =
      case_when(
        Plataforma == 'Comprasnet' & abertura_lances < data_20s ~ 'cnet0',
        Plataforma == 'Comprasnet' & abertura_lances < data_3s ~ 'cnet1',
        Plataforma == 'Comprasnet' ~ 'cnet3',
        Plataforma == 'BEC' & abertura_lances < data_20s ~ 'bec0',
        Plataforma == 'BEC' & abertura_lances < data_3s ~ 'bec1',
        Plataforma == 'BEC' ~ 'bec3'
      )
  )

# Não-ponderado ---------------------------------------------------------------
p1 <- df_plot %>% 
  ggplot() +
  geom_point(aes(x = abertura_lances, y = num_forn_lances, shape=Plataforma),
             alpha = .08) +
  stat_smooth(aes(x = abertura_lances, y = num_forn_lances,
                  linetype = Plataforma),
              geom = 'smooth', se = TRUE, col = 'black',
              method = 'loess', span = 0.6, formula = y ~ x) +
  geom_vline(xintercept = c(data_20s, data_3s), linetype = 'dotted') +
  geom_label(data = tibble(abertura_lances = c(data_20s, data_3s),
                           label = c('Regra 20s', 'Regra 3s')),
             aes(x = abertura_lances, label = label),
             y = 9.75, size = 3, family = 'serif') +
  coord_cartesian(xlim = as.Date(c('2011-03-01', '2015-11-01'))) +
  scale_x_date(breaks = as.Date(str_c(2011:2017, '-01-01')),
               labels = 2011:2017) +
  scale_y_continuous(labels = formatar_numero) +
  labs(x = "Data do pregão",
       y = "Número de participantes",
       title = "Número de participantes",
       subtitle = "Comparação entre BEC e Comprasnet") +
  facet_wrap(~ amostra, nrow = 2) +
  theme(legend.position = 'bottom') ; p1

ggsave(plot = p1, width = 5, height = 6,
       filename = 'results/plot-loess-bidders-with-dots.png')

# Versao sem título
p2 <- p1 +
  theme(plot.title = element_blank(),
        plot.subtitle = element_blank())

# ggsave(plot = p2, width = 5, height = 6,
#        filename = 'results/plot-loess-bidders-no-title.png')
