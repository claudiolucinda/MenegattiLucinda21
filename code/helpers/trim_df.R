trim_df <- function(df, vars, perc = 5, tail = 'both') {
  
  if (class(perc) != "numeric") {
    stop("'perc' must be a number betweeen 0 and 100")
  } else if (perc <= 0 | perc >= 100) {
    stop("'perc' must be a number betweeen 0 and 100")
  }
  
  if (!tail %in% c('both', 'lower', 'higher')) {
    stop("'tail' must be one of the following: 'both', 'lower', 'higher'.")
  }
  
  if (!"data.frame" %in% class(df)) {
    stop("'df' must be a dataframe")
  }
  
  if (!"tbl" %in% class(df)) {
    df <- as_tibble(df)
  }
  
  
  
  
  # Caso 1 (default): remover outliers de ambos os lados da distribuicao
  if (tail == 'both') {
    
    # Como cortaremos de ambos os lados, dividimos por 2
    cut <- perc / 100 / 2
    
    # Construindo relacao de valores de corte para cada variavel
    df_quantiles <- data.frame(var = vars, upper = NA, lower = NA)
    for (i in 1:length(vars)) {
      column_vector <- df[ , vars[i]][[1]]
      df_quantiles[i, "upper"] <- quantile(column_vector, 1 - cut, na.rm = TRUE)
      df_quantiles[i, "lower"] <- quantile(column_vector, cut, na.rm = TRUE)
    }
    
    # Filtrando com base nos valores de corte que foram calculados usando todas as observacoes
    for (i in 1:length(vars)) {
      column_vector <- df[ , vars[i]][[1]]
      df <- df %>% filter(column_vector < df_quantiles[i, "upper"],
                          column_vector > df_quantiles[i, "lower"])
    }
    
    # Caso 2: cortar apenas de um lado da distribuicao
  } else {
    cut <- perc / 100
    
    # Construindo relacao de valores de corte para cada variavel
    df_quantiles <- data.frame(var = vars, lower = NA)
    for (i in 1:length(vars)) {
      column_vector <- df[ , vars[i]][[1]]
      df_quantiles[i, "upper"] <- quantile(column_vector, 1 - cut, na.rm = TRUE)
      df_quantiles[i, "lower"] <- quantile(column_vector, cut, na.rm = TRUE)
    }
    
    # Sub-caso 2.1: cortar apenas do lado esquerdo da distribuicao
    if (tail == 'lower') {
      
      # Filtrando
      for (i in 1:length(vars)) {
        column_vector <- df[ , vars[i]][[1]]
        df <- df %>% filter(column_vector > df_quantiles[i, "lower"])
      }
      
      # Sub-caso 2.2: cortar apenas do lado direito da distribuicao
    } else {
      
      # Filtrando com base nos valores de corte que foram calculados usando todas as observacoes
      for (i in 1:length(vars)) {
        column_vector <- df[ , vars[i]][[1]]
        df <- df %>% filter(column_vector < df_quantiles[i, "upper"])
      }
      
    }
  }
  
  return(df)
}
