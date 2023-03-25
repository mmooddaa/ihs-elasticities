# test
library(fixest)
library(marginaleffects)
library(here)

nswre <- haven::read_dta(here('data/nswre74.dta'))

nswre$re75_ihs <- asinh(nswre$re75)
nswre$re78_ihs <- asinh(nswre$re78)

m <- feols(re78 ~ treat + re75,
      nswre)
m_ihs <- feols(re78 ~ treat + re75_ihs,
               nswre)
summary(m_ihs)

ihsElasticity(m_ihs,
              explanatory_var = 're75_ihs',
              type = 'linear-arcsinh',
              data = nswre)

m_ihs <- feols(re78_ihs ~ treat + re75_ihs,
               nswre)
summary(m_ihs)

r <- ihsElasticity(m_ihs,
              explanatory_var = 'treat',
              type = 'arcsinh-linear-dummy-exact',
              data = nswre)
r[ c('estimate', 'std.error')]

r <- ihsElasticity(m_ihs,
                   explanatory_var = 'treat',
                   type = 'arcsinh-linear-dummy-approx',
                   data = nswre)
r[ c('estimate', 'std.error')]


ihsElasticity(m_ihs,
              explanatory_var = 're75_ihs',
              type = 'arcsinh-arcsinh',
              conf_level = .9,
              data = nswre)



(2/3)*(4/5)

(2*4)/(3*5)

(2/5) * (4/3)
