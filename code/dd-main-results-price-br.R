library(lfe)
library(stargazer)
library(tidyverse)
source('code/helpers/get_robust_std_errors.R')

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
    .f = ~ felm(as.formula(.x), data = dd_brasil)
    ))

# Salvando tabelas de resultados (erros-padrao simples)
stargazer(df_models_brasil$felm_models, type = 'text',
          out = 'results/dd-main-results/price/dd-price-br.txt')

stargazer(df_models_brasil$felm_models,
          out = 'results/dd-main-results/price/dd-price-br-latex.tex',
          decimal.mark = ',', digit.separator = '.')


# Calculando erros-padrao robustos (HC1 SE) -----------------------------------

# Codigo para calcular HC1 SE nao funciona com lfe::felm
# Precisamos rodar regressoes usando lm
# Pode demorar bastante (menos eficiente com muitas dummies)

# Para rodar em paralelo no Windows, basta rodar as duas linhas a seguir
# e, dentro de "mutate", substituir "map" por "future_map"

# library(furrr)
# future::plan(multisession)

df_models_brasil <- df_models_brasil %>% 
  mutate(lm_models = map(
    .x = formula,
    .f = ~ str_replace(.x, '\\|', '+') %>%
      lm(data = dd_brasil)
  )) %>% 
  mutate(hc1_se = map(
    .x = lm_models,
    .f = ~ .x %>%
      get_robust_std_errors(HC = 'HC1')
  ))

# Salvando resultados completos em formato rds
df_models_brasil %>% 
  # Nao salvaremos lm_models em disco pois sao muito pesados
  select(-lm_models) %>% 
  saveRDS('results/dd-main-results/price/dd-price-br-full-results.rds')