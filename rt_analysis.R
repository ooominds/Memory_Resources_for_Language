
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
#                               Estimate Std. Error t value Pr(>|t|)
# (Intercept)                    6.52281    0.12957  50.341  < 2e-16
# Typecase                      -0.20385    0.09397  -2.169  0.03013
# Typecollocation               -0.13901    0.09442  -1.472  0.14105
# Typesubordination             -0.28540    0.09391  -3.039  0.00239
# Llama_FAvg.High               -0.33901    0.12636  -2.683  0.00733
# Llama_FHigh                   -0.27499    0.11070  -2.484  0.01303
# SRTAvg.Slow                    0.01508    0.12643   0.119  0.90508
# SRTAvg.Fast                    0.02380    0.13039   0.183  0.85517
# SRTFast                        0.05687    0.13226   0.430  0.66724
# ConditionCT                    0.17708    0.06030   2.937  0.00334
# TrialOrder.z                  -0.03112    0.01556  -2.001  0.04552
# Typecase:ConditionCT           0.07754    0.05552   1.397  0.16264
# Typecollocation:ConditionCT    0.15781    0.05721   2.758  0.00584
# Typesubordination:ConditionCT  0.11645    0.05516   2.111  0.03484
# Llama_FAvg.High:ConditionCT    0.15609    0.05883   2.653  0.00801
# Llama_FHigh:ConditionCT        0.15279    0.04782   3.195  0.00141
# SRTAvg.Slow:ConditionCT        0.09750    0.05513   1.768  0.07707
# SRTAvg.Fast:ConditionCT        0.12076    0.05558   2.173  0.02986
# SRTFast:ConditionCT           -0.15362    0.05754  -2.670  0.00762
# 
# Approximate significance of smooth terms:
#                               edf Ref.df     F p-value
# s(Item)                     83.16     92 7.824  <2e-16
# s(TrialOrder.z,Participant) 93.45    399 2.213  <2e-16
# 
# R-sq.(adj) =  0.363   Deviance explained = 39.7%
# GCV = 0.35723  Scale est. = 0.33814   n = 3660

#################
### Contrasts ###
#################

# example of combined comparison
wald_gam(gam.model,
    select=matrix(c(0,-0.33,-0.33,-0.33,0,0,0,0,0,0.25,0,0.25,0.25,0.25,0,0,0,0,0), nrow=1))
# [1] "Chi-square(1.000) = 14.934, p = 1.11e-04 ***"

# examples for specific 2-way comparisons (long output)
wald_gam(gam.model,
    comp=list(Type=c('aspect', 'case', 'collocation', 'subordination'),
        Condition=c('ST', 'CT')))
wald_gam(gam.model,
    comp=list(Llama_F=c('Avg.Low', 'Avg.High', 'High'),
        Condition=c('ST', 'CT')))
wald_gam(gam.model,
    comp=list(SRT=c('Slow', 'Avg.Slow', 'Avg.Fast', 'Fast'),
        Condition=c('ST', 'CT')))

###########
### BRM ###
###########

# NOTE: It is expected that the estimates will be slightly different
#       for each specific chain/iteration run
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
# sd(Intercept)     0.28      0.02     0.24     0.33 1.00     2539     4560
# 
# ~Participant (Number of levels: 45) 
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.30      0.04     0.24     0.38 1.00     2430     4300
# sd(TrialOrder.z)     0.08      0.02     0.05     0.11 1.00     3996     5858
# 
# Population-Level Effects: 
#                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                         6.51      0.13     6.26     6.77 1.00     2047     2884
# Typecase                         -0.20      0.09    -0.37    -0.03 1.00     2126     3784
# Typecollocation                  -0.14      0.09    -0.32     0.04 1.00     2240     3742
# Typesubordination                -0.28      0.09    -0.46    -0.11 1.00     2138     3621
# Llama_FAvg.High                  -0.35      0.13    -0.61    -0.09 1.00     2526     4043
# Llama_FHigh                      -0.27      0.11    -0.49    -0.05 1.00     2485     4002
# SRTAvg.Slow                       0.00      0.13    -0.26     0.26 1.00     1992     3050
# SRTAvg.Fast                       0.03      0.14    -0.24     0.30 1.00     2092     3512
# SRTFast                           0.07      0.14    -0.20     0.35 1.00     2378     3271
# ConditionCT                       0.18      0.06     0.06     0.30 1.00     5511     5699
# TrialOrder.z                     -0.03      0.02    -0.06     0.00 1.00     6577     6360
# Typecase:ConditionCT              0.07      0.06    -0.03     0.18 1.00     8273     6533
# Typecollocation:ConditionCT       0.16      0.06     0.04     0.27 1.00     8373     6958
# Typesubordination:ConditionCT     0.11      0.06     0.01     0.22 1.00     8512     6992
# Llama_FAvg.High:ConditionCT       0.15      0.06     0.04     0.27 1.00     8964     6408
# Llama_FHigh:ConditionCT           0.15      0.05     0.06     0.25 1.00     9525     6148
# SRTAvg.Slow:ConditionCT           0.10      0.05    -0.01     0.20 1.00     8970     6425
# SRTAvg.Fast:ConditionCT           0.12      0.06     0.01     0.23 1.00    10028     6837
# SRTFast:ConditionCT              -0.15      0.06    -0.27    -0.04 1.00     9447     6958
# 
# Family Specific Parameters: 
#       Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma     0.59      0.01     0.57     0.60 1.00    15693     5419


