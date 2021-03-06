library(tidyverse)
source('code/helpers/my_theme.R')
source('code/helpers/formatar_numero.R')

df_placebo <-
  tibble(data_placebo = seq(-420, -30, by = 3) + data_3s) %>% 
  mutate(model_list =
           map(.x = data_placebo,
               .f = ~ str_c('results/placebo/estimates/',
                            as.character(.x), '.rds') %>% readRDS()))

df_placebo <- df_placebo %>% 
  mutate(treat1_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[2]),
         treat2_est = map_dbl(.x = model_list,
                              .f = ~ .x$estimate[3]),
         treat_placebo_est = map_dbl(.x = model_list,
                                     .f = ~ .x$estimate[4]),
         treat1_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[2]),
         treat2_se = map_dbl(.x = model_list,
                             .f = ~ .x$std_error[3]),
         treat_placebo_se = map_dbl(.x = model_list,
                                    .f = ~ .x$std_error[4]))

# Computando intervalos de confiança 95%
df_placebo <- df_placebo %>%
  mutate(treat1_upper = treat1_est + 2*treat1_se,
         treat1_lower = treat1_est - 2*treat1_se,
         treat2_upper = treat2_est + 2*treat2_se,
         treat2_lower = treat2_est - 2*treat2_se,
         treat_placebo_upper = treat_placebo_est + 2*treat_placebo_se,
         treat_placebo_lower = treat_placebo_est - 2*treat_placebo_se)


# Plotting: efeito do tratamento placebo --------------------------------------
theme_set(my_theme())

ggplot(df_placebo, aes(x = data_placebo, group = 1)) +
  geom_line(aes(y = treat_placebo_est), color = 'black') +
  geom_ribbon(aes(ymin = treat_placebo_lower, ymax = treat_placebo_upper),
              fill = 'gray', alpha = 0.5) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date('2013-10-07'),
             col = 'black', alpha = 0.7, linetype = 'dotted') +
  geom_label(x = as.Date('2013-10-07'),
             y = 0.75, label = '07/10/2013:\nPublicação da\nRegra dos 3s',
             size = 3, family = 'serif', col = 'gray25') +
  scale_x_date(breaks = as.Date(c('2012-12-01',
                                  '2013-02-01', '2013-04-01',
                                  '2013-06-01', '2013-08-01',
                                  '2013-10-01', '2013-12-01')),
               labels = c('Dez/12', 'Fev/13', 'Abr/13', 'Jun/13',
                          'Ago/13', 'Out/13', 'Dez/13')) +
  scale_y_continuous(labels = formatar_numero) +
  labs(
    x = 'Data do Tratamento Placebo',
    y = 'Coeficiente do tratamento placebo',
    title = 'Efeito de Tratamento Placebo Anterior à Regra dos 3s',
    subtitle = 'Unidades compradoras de São Paulo'
  )

ggsave('results/placebo/plots/placebo-3s-price-sp.png', width = 6, height = 5)

# Plotting: dois painéis: Placebo + 3s ----------------------------------------
# Finalizando organização do dataframe para construir o gráfico
df_plot <- df_placebo %>%
  select(-model_list) %>% 
  gather(key = 'treat_var',
         value = 'coefficient',
         treat1_est:treat_placebo_lower) %>%
  mutate(grupo = str_remove(treat_var, 'treat|treat_') %>%
           str_remove('_est|_se|_lower|_upper') %>%
           str_replace('_placebo', 'Tratamento Placebo') %>%
           str_replace('2', 'Tratamento Regra 3s'))

# Gráfico
ggplot(mapping = aes(x = data_placebo, group = grupo)) +
  geom_line(data = df_plot %>% 
              filter(treat_var %in% c('treat2_est', 'treat_placebo_est')),
            mapping = aes(y = coefficient),
            color = 'black') +
  geom_ribbon(data = df_plot %>% 
                filter(treat_var %in% c('treat2_lower', 'treat2_upper')) %>% 
                spread(treat_var, coefficient),
              mapping = aes(ymin = treat2_lower, ymax = treat2_upper),
              fill = "gray", alpha = 0.5) +
  geom_ribbon(data = df_plot %>%
                filter(treat_var %in% c('treat_placebo_lower',
                                        'treat_placebo_upper')) %>% 
                spread(treat_var, coefficient),
              mapping = aes(ymin = treat_placebo_lower,
                            ymax = treat_placebo_upper),
              fill = "gray", alpha = 0.5) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ grupo, ncol = 2) +
  scale_y_continuous(labels = formatar_numero) +
  labs(
    x = 'Data do Tratamento Placebo',
    y = 'Coeficiente estimado',
    title = 'Teste Placebo - Efeito da Regra dos 3s - Amostra completa',
    subtitle = 'Introdução de um tratamento placebo anterior à regra dos 3s',
    caption = 'Notas:
    1) Resultados de modelos considerando datas alternativas para o tratamento placebo;
    2) Total de 60 placebos, de 22/04/2012 a 03/12/2013, intervalados em 10 dias;
    3) As áreas sombreadas representam intervalos de confiança de 95% (erros padrão HC1)'
  )

ggsave('results/placebo/plots/placebo-3s-price-sp-2paineis.png',
       width = 9, height = 6)
