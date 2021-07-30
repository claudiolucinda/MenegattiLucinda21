library(tidyverse)
source('code/helpers/get_robust_std_errors.R')

# Carregando base de dados
dd_sp <- readRDS('data/dd_sp.rds')

# Attaching DF para simplificar o codigo
attach(dd_sp)

# Definindo variaveis com as datas relevantes
data_20s <- as.Date('2012-01-17')
data_3s <- as.Date('2014-01-02')

# Construindo DF aninhado para rodar regressoes placebo -----------------------
# Cada linha representa uma data distinta para o tratamento (regra 3s)
# treat1, treat2 e treat_placebo sao as respectivas variaveis indicativas de tratamento 
df_placebo <-
  tibble(data_placebo = seq(-420, -30, by = 3) + data_3s) %>%
  mutate(
    treat1 = map(
      .x = data_placebo,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= data_20s & abertura_lances < .x,
        1, 0)
    ),
    treat_placebo = map(
      .x = data_placebo,
      .f = ~ ifelse(
        comprasnet == 1 & abertura_lances >= .x & abertura_lances < data_3s,
        1, 0)
    ),
    treat2 = map(
      .x = data_placebo,
      .f = ~ ifelse(comprasnet == 1 & abertura_lances >= data_3s, 1, 0)
    )
  )

# Iterando entre as colunas de df_placebo (datas tratamento placebo) ----------
for (i in 1:nrow(df_placebo)) {
  
  message(str_c('Fitting model ', i, '/', nrow(df_placebo)))
  
  # Rodando DD placebo
  # Usamos lm pois get_robust_std_errors nao funciona com lfe:felm
  model <- 
    lm(log_win_bid ~ df_placebo$treat1[[i]] + df_placebo$treat2[[i]] +
         df_placebo$treat_placebo[[i]] + qualidade + kg_por_unid + 
         futuro_defl + arab_defl + bimestre + unidade_compradora + 
         municipio + marca_vencedor_principais)
  
  message('Estimating HC1 standard errors')
  robust_se <- get_robust_std_errors(model)
  
  filename <- str_c('results/placebo/',
                    as.character(df_placebo$data_placebo[[i]]),
                    '.rds')
  
  message('Writing ', filename, '\n')
  
  saveRDS(robust_se, filename)
  
  rm(model, robust_se)
  
}
