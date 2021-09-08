require(lfe)
require(tidyverse)
require(lmtest)

f_stat <- function(.x, .data) {
  mod01 = felm(as.formula(.x), data = .data)
  output=summary(mod01)
  coefs=output[['p']]
  fstat=output[['r2']]
  nobs=mod01[['N']]
  res=list(fstat, coefs, nobs)
  return(res)
  
}