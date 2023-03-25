# test
library(marginaleffects)
library(here)

# Load function ---------------------------------------------------------------------

source(here('scripts/ihs-trans.R'))

# Load data -------------------------------------------------------------------------

nswre <- haven::read_dta(here('data/nswre74.dta'))

nswre$re75_ihs <- asinh(nswre$re75)
nswre$re78_ihs <- asinh(nswre$re78)


# Replicate results from Bellemere and Wicham (2020), Table 3 -----------------------

## Column 2 ------------------------------------------------------------------------

m <- lm(re78 ~ treat + re75_ihs,
           nswre)
summary(m)

ihsElasticity(m,
              explanatory_var = 're75_ihs',
              type = 'linear-arcsinh',
              data = nswre)
# Est. from BW: 0.0256 (.0145)


## Column 3 --------------------------------------------------------------------------

m <- lm(re78_ihs ~ treat + re75,
           nswre)
summary(m)

ihsElasticity(m,
              explanatory_var = 're75',
              type = 'arcsinh-linear',
              data = nswre)
# Est. from BW: 0.155 (.0896)

ihsElasticity(m,
              explanatory_var = 'treat',
              type = 'arcsinh-linear-dummy-approx',
              data = nswre)
# Est. from BW: 1.638 (1.096)

ihsElasticity(m,
              explanatory_var = 'treat',
              type = 'arcsinh-linear-dummy-exact',
              data = nswre)
# Est. from BW: 1.876 (1.195)

## Column 4 ------------------------------------------------------------------------

m <- lm(re78_ihs ~ treat + re75_ihs,
           nswre)
summary(m)

ihsElasticity(m,
              explanatory_var = 're75_ihs',
              type = 'arcsinh-arcsinh',
              data = nswre)
# Est. from BW: 0.0705 (.0506)

ihsElasticity(m,
              explanatory_var = 'treat',
              type = 'arcsinh-linear-dummy-approx',
              data = nswre)
# Est. from BW: 1.578 (1.076)

ihsElasticity(m,
              explanatory_var = 'treat',
              type = 'arcsinh-linear-dummy-exact',
              data = nswre)
# Est. from BW: 1.813 (1.174)
