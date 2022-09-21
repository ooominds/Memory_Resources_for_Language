
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
    s(Item, bs='re') +
    s(TrialOrder.z, Participant, bs='fs', m=1),
    data = rt_data,
    chains=4, iter=4000, cores=4,
    control=list(adapt_delta=.95)))
# Smooth Terms: 
#                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sds(ssound_1)                     0.28      0.02     0.24     0.33 1.00     2497     4211
# sds(szTrialOrderSubject_nr_1)     0.31      0.05     0.22     0.41 1.00     3389     5414
# sds(szTrialOrderSubject_nr_2)     2.07      0.26     1.62     2.63 1.00     3734     5053
# 
# Population-Level Effects: 
#                                          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                                    6.23      0.13     5.97     6.49 1.00     2556     3857
# typecase                                     0.09      0.09    -0.08     0.26 1.00     2003     4170
# typecollocation                              0.15      0.09    -0.03     0.33 1.00     2200     3934
# typeaspect                                   0.29      0.09     0.11     0.47 1.00     1923     3328
# LlamaFScoreF4Avg.High                       -0.34      0.13    -0.59    -0.09 1.00     3167     4330
# LlamaFScoreF4High                           -0.27      0.11    -0.49    -0.05 1.00     2875     4383
# SRTgroups4Avg.Slow                           0.02      0.13    -0.23     0.27 1.00     3088     4497
# SRTgroups4Avg.Fast                           0.03      0.13    -0.23     0.28 1.00     3063     4790
# SRTgroups4Fast                               0.06      0.13    -0.20     0.32 1.00     2910     4578
# conditionConcurent                           0.29      0.06     0.18     0.41 1.00     5890     5960
# zTrialOrder                                 -0.03      0.02    -0.06     0.00 1.00     5781     6706
# typecase:conditionConcurent                 -0.04      0.05    -0.14     0.07 1.00     8989     6435
# typecollocation:conditionConcurent           0.04      0.06    -0.07     0.15 1.00     9316     6781
# typeaspect:conditionConcurent               -0.12      0.05    -0.22    -0.01 1.00     9094     6636
# LlamaFScoreF4Avg.High:conditionConcurent     0.16      0.06     0.04     0.27 1.00    10391     6717
# LlamaFScoreF4High:conditionConcurent         0.15      0.05     0.06     0.25 1.00    10221     6416
# SRTgroups4Avg.Slow:conditionConcurent        0.10      0.05    -0.01     0.20 1.00     8058     6758
# SRTgroups4Avg.Fast:conditionConcurent        0.12      0.06     0.01     0.23 1.00    10699     6818
# SRTgroups4Fast:conditionConcurent           -0.15      0.06    -0.26    -0.04 1.00    10051     7011
# 
# Family Specific Parameters: 
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.58      0.01     0.57     0.60 1.00    13845     6310


