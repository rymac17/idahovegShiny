
library(rsconnect)

# necessary files
nf <- c('data/idaho.rds',
        'data/quads.rds',
        'app.R')

deployApp(appFiles=nf,
          appName='habitat-patches-of-idaho')
