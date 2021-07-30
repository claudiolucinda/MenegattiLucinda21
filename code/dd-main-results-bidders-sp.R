library(lfe)
library(stargazer)
library(tidyverse)
source('code/helpers/get_robust_std_errors.R')

# Carregando base de dados para DD
dd_sp <- readRDS('data/dd_sp.rds')

# Especificacao basica
form <- 'num_forn_lances ~ comprasnet + treat1 + treat2'

# Model fitting ---------------------------------------------------------------

# Criando dataframe para iterar entre especificacoes e armazenar resultados 
# Cada linha correspondera a uma especificacao
df_models_sp <- tibble(
  # Definindo especificacoes
  formula = 
    c(form,
      str_c(form, ' | bimestre + unidade_compradora + municipio'),
      str_c(form, ' | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid               | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais'),
      str_c(form, ' + qualidade + kg_por_unid + futuro_defl + arab_defl | bimestre + unidade_compradora + municipio + marca_vencedor_principais')
    ),
  # Iterando entre especificacoes e armazenando resultados na coluna felm_models
  felm_models = map(
    .x = formula,
    .f = ~ felm(as.formula(.x), data = dd_sp)
  ))

# Salvando tabelas de resultados (erros-padrao simples)
stargazer(df_models_sp$felm_models, type = 'text',
          out = 'results/dd-main-results/bidders/dd-bidders-sp.txt')

stargazer(df_models_sp$felm_models,
          out = 'results/dd-main-results/bidders/dd-bidders-sp-tex.tex',
          decimal.mark = ',', digit.separator = '.')

# Calculando erros-padrao robustos (HC1 SE) -----------------------------------
# Codigo para calcular HC1 SE nao funciona com lfe::felm
# Precisamos rodar regressoes usando lm
# Pode demorar bastante (menos eficiente com muitas dummies)
df_models_sp <- df_models_sp %>% 
  mutate(lm_models = map(
    .x = formula,
    .f = ~ str_replace(.x, '\\|', '+') %>%
      lm(data = dd_sp)
  )) %>% 
  mutate(hc1_se = map(
    .x = lm_models,
    .f = ~ .x %>%
      get_robust_std_errors(HC = 'HC1')
  ))

# Salvando resultados completos em formato rds
df_models_sp %>% 
  # Nao salvaremos lm_models em disco pois sao muito pesados
  select(-lm_models) %>% 
  saveRDS('results/dd-main-results/bidders/dd-bidders-sp-full-results.rds')
