# wrapper: run model selection on survival, growth, and flowering.
# models: simplex, plus annual precip, temp, snow depth, and january snow d. anom.
setwd("C:/cloud/Dropbox/isotria_idiv/")

source('analysis/regressions/surv.R')
source('analysis/regressions/grow.R')
source('analysis/regressions/flow.R')
