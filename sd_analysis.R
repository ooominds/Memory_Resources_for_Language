
require(qgam)
require(itsadug)
require(brms)

options(show.signif.stars=FALSE)

load('sd_data.rda')

#############
### QGAMM ###
#############

qgam.model.simple <- mqgam(rollSD ~
    Type + Condition +
    TrialOrder.z +
    s(Item, bs='re') +
    s(TrialOrder.z, Participant, bs='fs', m=1),
    data=sd_data, qu=0.5, argGam=list(method='ML'))
qdo(qgam.model.simple, qu=0.5, summary)
# Parametric coefficients:
#                     Estimate Std. Error z value Pr(>|z|)
# (Intercept)         0.450625   0.020792  21.673  < 2e-16
# Typecase            0.050422   0.017071   2.954  0.00314
# Typeaspect          0.116857   0.017328   6.744 1.54e-11
# Typecollocation     0.149161   0.017453   8.547  < 2e-16
# ConditionConcurent -0.064547   0.010545  -6.121 9.30e-10
# TrialOrder.z        0.008742   0.007283   1.200  0.23001
# 
# Approximate significance of smooth terms:
#                               edf Ref.df Chi.sq p-value
# s(Item)                     25.29     92   37.1 0.00184
# s(TrialOrder.z,Participant) 80.36    404  467.5 < 2e-16
# 
# R-sq.(adj) =  0.142   Deviance explained = 15.5%
# -ML = 997.33  Scale est. = 1         n = 3519

#################
### Contrasts ###
#################

# examples for specific 2-way comparisons (long output)
qdo(qgam.model.simple, 0.5, wald_gam,
    comp=list(Type=c('aspect', 'case', 'collocation', 'subordination'),
        Condition=c('Single', 'Concurent')))

###########
### BRM ###
###########

# NOTE: It is expected that the estimates will be slightly different
#       for each specific chain/iteration run.
#       Using HPC or otherwise superior performing computers
#       is strongly advised.
summary(brm.model <- brm(bf(rollSD ~
    Type + Condition +
    TrialOrder.z +
    s(Item, bs='re') +
    s(TrialOrder.z, Participant, bs='fs', m=1),
    quantile=0.5),
    data=sd_data,
    family=asym_laplace(),
    chains=4, iter=4000, cores=4))
# Smooth Terms: 
#                                 Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(sItem_1)                        0.04      0.01     0.02     0.05 1.00     2387     1965
# sds(sTrialOrder.zParticipant_1)     0.31      0.05     0.21     0.42 1.00     1525     2913
# sds(sTrialOrder.zParticipant_2)     1.52      0.18     1.20     1.93 1.00     2121     3533
# 
# Population-Level Effects: 
#                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept              0.44      0.02     0.39     0.48 1.00     1487     2975
# Typecase               0.05      0.02     0.02     0.09 1.00     5681     5710
# Typeaspect             0.12      0.02     0.09     0.16 1.00     5729     5674
# Typecollocation        0.16      0.02     0.12     0.20 1.00     5978     5677
# ConditionConcurent    -0.06      0.01    -0.08    -0.04 1.00     9675     5765
# TrialOrder.z           0.01      0.01    -0.01     0.03 1.00     3455     4147
# 
# Family Specific Parameters: 
#          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma        0.12      0.00     0.12     0.12 1.00     8649     5391
# quantile     0.50      0.00     0.50     0.50 1.00     8000     8000


