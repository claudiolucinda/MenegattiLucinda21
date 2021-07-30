# Especificacoes para teste de robustez - Tabela 6 da dissertacao
# Inclusao de interacao grupo X tendencia linear

library(stargazer)
library(tidyverse)
library(lfe)
source('code/helpers/get_robust_std_errors.R')

# Abrindo bases ---------------------------------------------------------------
dd_brasil <- readRDS('data/dd_brasil.rds')
dd_sp <- readRDS('data/dd_sp.rds')

# Estimating DD with group-specific linear trends -----------------------------

# log win bid
trends_sp_log_win_bid <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_sp)

trends_brasil_log_win_bid <- 
  felm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
       data = dd_brasil)

# n bidders
trends_sp_n_bidders <- 
  felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + unidade_compradora + municipio + marca_vencedor_principais,
       data = dd_sp)

trends_brasil_n_bidders <- 
  felm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
         kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre | 
         bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
       data = dd_brasil)

# Salvando tabelas de resultados (erros-padrao simples) -----------------------
stargazer(trends_brasil_log_win_bid, trends_sp_log_win_bid,
          trends_brasil_n_bidders, trends_sp_n_bidders,
          type = 'text',
          out = 'results/trends/group-specific-trends.txt')

stargazer(trends_brasil_log_win_bid, trends_sp_log_win_bid,
          trends_brasil_n_bidders, trends_sp_n_bidders,
          out = 'results/trends/group-specific-trends-latex.tex',
          decimal.mark = ',', digit.separator = '.')


# HC1 SE ----------------------------------------------------------------------

# log win bid
trends_sp_hc1_price <- 
  lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre +
       bimestre + unidade_compradora + municipio + marca_vencedor_principais,
     data = dd_sp) %>% 
  get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
    )
  )

trends_brasil_hc1_price <- 
  lm(log_win_bid ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre + 
       bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
     data = dd_brasil) %>% 
  get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
    )
  )

# Bidders
trends_sp_hc1_bidders <- 
  lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre +
       bimestre + unidade_compradora + municipio + marca_vencedor_principais,
     data = dd_sp) %>% 
  get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
    )
  )

trends_brasil_hc1_bidders <- 
  lm(num_forn_lances ~ comprasnet + treat1 + treat2 + qualidade + 
       kg_por_unid + futuro_defl + arab_defl + comprasnet:trend_bimestre + 
       bimestre + sigla_uf:bimestre + municipio + unidade_compradora,
     data = dd_brasil) %>% 
  get_robust_std_errors(HC = 'HC1') %>% 
  filter(
    str_detect(
      coef,
      '(Intercept)|comprasnet$|trend_bimestre|treat|qualidade|kg_por_unid|defl'
    )
  )

# Salvando dados --------------------------------------------------------------
list('trends-price-sp' = trends_sp_hc1_price,
     'trends-price-br' = trends_brasil_hc1_price,
     'trends-bidders-sp' = trends_sp_hc1_bidders,
     'trends-bidders-br' = trends_brasil_hc1_bidders) %>% 
  saveRDS('results/trends/group-specific-trends-results-hc1-se.rds')
