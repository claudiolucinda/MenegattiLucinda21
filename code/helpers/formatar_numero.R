formatar_numero <-
  purrr::partial(formatC, digits = 2, decimal.mark = ',', big.mark = '.')