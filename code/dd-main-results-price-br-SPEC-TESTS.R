#############################################
# R/& R Code
# Residual Specification Tests
# Claudio R. Lucinda
# 2021
# FEA/USP
##############################################

library(lfe)
library(stargazer)
library(tidyverse)
library(lmtest)
source('code/helpers/get_robust_std_errors.R')
source('code/helpers/spec_tests.R')
source('code/helpers/f_tests.R')


# Carregando base de dados para DD
dd_brasil <- readRDS('data/dd_brasil.rds')

# Especificacao basica
form <- 'log_win_bid ~ comprasnet + treat1 + treat2'

# Model fitting ---------------------------------------------------------------

# Criando dataframe para iterar entre especificacoes e armazenar resultados 
# Cada linha correspondera a uma especificacao
df_models_brasil <- tibble(
  # Definindo especificacoes
  formula = 
    c(form,
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora')),
  # Iterando entre especificacoes e armazenando resultados na coluna felm_models
  felm_models = map(
    .x = formula,
    .f = mod_diag, 
    .data = dd_brasil
  ))


df_fstats_brasil <- tibble(
  # Definindo especificacoes
  formula = 
    c(form,
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio'),
      str_c(form, ' | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + sigla_uf:bimestre + municipio + unidade_compradora')),
  # Iterando entre especificacoes e armazenando resultados na coluna felm_models
  felm_models = map(
    .x = formula,
    .f = f_stat, 
    .data = dd_brasil
  ))

R_rest=unlist(df_fstats_brasil[[2]][[4]][1])
R_irrest1=unlist(df_fstats_brasil[[2]][[5]][1])
R_irrest2=unlist(df_fstats_brasil[[2]][[6]][1])

p_rest=unlist(df_fstats_brasil[[2]][[4]][2])
p_irrest1=unlist(df_fstats_brasil[[2]][[5]][2])
p_irrest2=unlist(df_fstats_brasil[[2]][[6]][2])

n_rest=unlist(df_fstats_brasil[[2]][[4]][3])
n_irrest1=unlist(df_fstats_brasil[[2]][[5]][3])
n_irrest2=unlist(df_fstats_brasil[[2]][[6]][3])

F1=((R_irrest1-R_rest)/(p_irrest1-p_rest))/((1-R_irrest1)/(n_rest-p_irrest1))
F2=((R_irrest2-R_rest)/(p_irrest2-p_rest))/((1-R_irrest2)/(n_rest-p_irrest2))

pF1=pf(F1, (p_irrest1-p_rest), (n_rest-p_irrest1))
pF2=pf(F2, (p_irrest2-p_rest), (n_rest-p_irrest2))

save.image('results/dd-main-results/price/dd-price-br-SPEC-TESTS.RData')