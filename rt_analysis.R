
require(mgcv)
require(itsadug)
require(brms)

options(show.signif.stars=FALSE)

load('rt_data.rda')

############
### GAMM ###
############

summary(gam.model <- gam(RT.log ~
    (Type + Llama_F + SRT) * Condition +
    TrialOrder.z +
    s(Item, bs='re') +
    s(TrialOrder.z, Participant, bs='fs', m=1),
    data = rt_data))
# Parametric coefficients:
#                                     Estimate Std. Error t value Pr(>|t|)
# (Intercept)                         6.230628   0.133368  46.717  < 2e-16
# Typecase                            0.081628   0.093269   0.875  0.38153
# Typecollocation                     0.146439   0.093729   1.562  0.11829
# Typeaspect                          0.285338   0.093879   3.039  0.00239
# Llama_FAvg.High                    -0.342828   0.132001  -2.597  0.00944
# Llama_FHigh                        -0.271282   0.115745  -2.344  0.01915
# SRTAvg.Slow                        -0.002686   0.132217  -0.020  0.98380
# SRTAvg.Fast                         0.021533   0.136339   0.158  0.87452
# SRTFast                             0.068953   0.138269   0.499  0.61803
# ConditionConcurent                  0.293381   0.058133   5.047 4.73e-07
# TrialOrder.z                       -0.031314   0.015510  -2.019  0.04357
# Typecase:ConditionConcurent        -0.038944   0.053641  -0.726  0.46789
# Typecollocation:ConditionConcurent  0.041319   0.055308   0.747  0.45506
# Typeaspect:ConditionConcurent      -0.116024   0.055164  -2.103  0.03551
# Llama_FAvg.High:ConditionConcurent  0.155925   0.058841   2.650  0.00809
# Llama_FHigh:ConditionConcurent      0.152838   0.047829   3.195  0.00141
# SRTAvg.Slow:ConditionConcurent      0.097675   0.055138   1.771  0.07657
# SRTAvg.Fast:ConditionConcurent      0.121046   0.055585   2.178  0.02950
# SRTFast:ConditionConcurent         -0.153642   0.057549  -2.670  0.00763
# 
# Approximate significance of smooth terms:
#                               edf Ref.df     F p-value
# s(Item)                     83.15     92 7.822  <2e-16
# s(TrialOrder.z,Participant) 93.58    399 2.218  <2e-16
# 
# R-sq.(adj) =  0.363   Deviance explained = 39.7%
# GCV = 0.35729  Scale est. = 0.33818   n = 3660

#################
### Contrasts ###
#################

# example of combined comparison
wald_gam(gam.model,
    select=matrix(c(0,-0.33,-0.33,-0.33,0,0,0,0,0,0.25,0,0.25,0.25,0.25,0,0,0,0,0), nrow=1))
# [1] "Chi-square(1.000) = 2.068, p = 0.150 "

# examples for specific 2-way comparisons (long output)
wald_gam(gam.model,
    comp=list(Type=c('aspect', 'case', 'collocation', 'subordination'),
        Condition=c('Single', 'Concurent')))
wald_gam(gam.model,
    comp=list(Llama_F=c('Avg.Low', 'Avg.High', 'High'),
        Condition=c('Single', 'Concurent')))
wald_gam(gam.model,
    comp=list(SRT=c('Slow', 'Avg.Slow', 'Avg.Fast', 'Fast'),
        Condition=c('Single', 'Concurent')))

###########
### BRM ###
###########

# NOTE: It is expected that the estimates will be slightly different
#       for each specific chain/iteration run.
#       Using HPC or otherwise superior performing computers
#       is strongly advised.
summary(brm.model <- brm(RT.log ~
    (Type + Llama_F + SRT) * Condition +
    TrialOrder.z +
    (1|Item) +
    (1|Participant) +
    (0+TrialOrder.z|Participant),
    data = rt_data,
    chains=4, iter=4000, cores=4,
    control=list(adapt_delta=.95)))
# Group-Level Effects: 
# ~Item (Number of levels: 96) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.28      0.02     0.24     0.33 1.00     2364     3661
# 
# ~Participant (Number of levels: 45) 
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.30      0.04     0.24     0.38 1.00     2500     4609
# sd(TrialOrder.z)     0.08      0.01     0.05     0.11 1.00     3701     5233
# 
# Population-Level Effects: 
#                                    Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                              6.23      0.13     5.98     6.48 1.00     2263     3881
# Typecase                               0.08      0.09    -0.09     0.26 1.00     1938     3773
# Typecollocation                        0.14      0.09    -0.04     0.32 1.00     1528     2887
# Typeaspect                             0.28      0.09     0.11     0.46 1.00     1770     2971
# Llama_FAvg.High                       -0.34      0.13    -0.60    -0.09 1.00     2676     3939
# Llama_FHigh                           -0.27      0.11    -0.49    -0.04 1.00     2426     3830
# SRTAvg.Slow                           -0.00      0.13    -0.26     0.25 1.00     2400     3492
# SRTAvg.Fast                            0.02      0.13    -0.24     0.28 1.00     2458     3621
# SRTFast                                0.07      0.13    -0.19     0.33 1.00     2642     4118
# ConditionConcurent                     0.29      0.06     0.18     0.41 1.00     6017     5746
# TrialOrder.z                          -0.03      0.02    -0.06     0.00 1.00     5927     6121
# Typecase:ConditionConcurent           -0.04      0.05    -0.14     0.07 1.00     8867     6582
# Typecollocation:ConditionConcurent     0.04      0.06    -0.07     0.16 1.00     8935     6363
# Typeaspect:ConditionConcurent         -0.11      0.06    -0.22    -0.01 1.00     8486     6152
# Llama_FAvg.High:ConditionConcurent     0.15      0.06     0.04     0.27 1.00     8897     6831
# Llama_FHigh:ConditionConcurent         0.15      0.05     0.06     0.25 1.00     8855     6071
# SRTAvg.Slow:ConditionConcurent         0.10      0.05    -0.01     0.20 1.00     8932     7091
# SRTAvg.Fast:ConditionConcurent         0.12      0.06     0.01     0.23 1.00    10192     6788
# SRTFast:ConditionConcurent            -0.15      0.06    -0.27    -0.04 1.00     9131     6572
# 
# Family Specific Parameters: 
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.59      0.01     0.57     0.60 1.00    13670     5691


