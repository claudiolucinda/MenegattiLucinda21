require(lfe)
require(tidyverse)
require(lmtest)

mod_diag <- function(.x,.data) {
  mod01 = felm(as.formula(.x), data = .data)
  res01=residuals(mod01)
  pred01=mod01$fitted.values
  qqnorm(res01, ylab = 'Residuals')
  qqline(res01)
  Fig1 = recordPlot()
  Fig2 = hist(res01, xlab = 'Residuals')
  Fig3 = plot(pred01, res01)
  normtest = shapiro.test(res01)
  bptest01 = bptest(mod01)
  res = list(Fig1, Fig2, Fig3, normtest, bptest01)
  
  
}