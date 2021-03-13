
require(qgam)
require(itsadug)
require(brms)

options(show.signif.stars=FALSE)

load('sd_data.rda')

#############
### QGAMM ###
#############

qgam.model <- mqgam(rollSD ~
    (Type + Llama_F + SRT) * Condition +
    TrialOrder.z +
    s(Item, bs='re') +
    s(TrialOrder.z, Participant, bs='fs', m=1),
    data=sd_data, qu=0.5)
qdo(qgam.model, qu=0.5, summary)
# Parametric coefficients:
#                                Estimate Std. Error z value Pr(>|z|)
# (Intercept)                    0.559558   0.045967  12.173  < 2e-16
# Typecase                      -0.068367   0.023411  -2.920 0.003497
# Typecollocation                0.042936   0.024425   1.758 0.078774
# Typesubordination             -0.096589   0.023348  -4.137 3.52e-05
# Llama_FAvg.High               -0.047659   0.049208  -0.969 0.332785
# Llama_FHigh                   -0.077012   0.043098  -1.787 0.073952
# SRTAvg.Slow                    0.087932   0.049335   1.782 0.074694
# SRTAvg.Fast                    0.071140   0.050668   1.404 0.160303
# SRTFast                        0.027534   0.051448   0.535 0.592523
# ConditionCT                   -0.062659   0.031377  -1.997 0.045827
# TrialOrder.z                   0.007760   0.007347   1.056 0.290832
# Typecase:ConditionCT           0.003554   0.029675   0.120 0.904665
# Typecollocation:ConditionCT   -0.026886   0.030541  -0.880 0.378688
# Typesubordination:ConditionCT -0.039865   0.029082  -1.371 0.170443
# Llama_FAvg.High:ConditionCT    0.007275   0.029443   0.247 0.804846
# Llama_FHigh:ConditionCT        0.096637   0.025310   3.818 0.000134
# SRTAvg.Slow:ConditionCT       -0.034728   0.028987  -1.198 0.230896
# SRTAvg.Fast:ConditionCT       -0.106863   0.029099  -3.672 0.000240
# SRTFast:ConditionCT            0.041969   0.030056   1.396 0.162607
# 
# Approximate significance of smooth terms:
#                               edf Ref.df Chi.sq p-value
# s(Item)                     27.59     92  40.52 0.00181
# s(TrialOrder.z,Participant) 78.36    399 447.20 < 2e-16
# 
# R-sq.(adj) =   0.15   Deviance explained = 16.6%
# -REML = 1015.2  Scale est. = 1         n = 3519

#################
### Contrasts ###
#################

# examples for specific 2-way comparisons (long output)
qdo(qgam.model, 0.5, wald_gam,
    comp=list(Type=c('aspect', 'case', 'collocation', 'subordination'),
        Condition=c('ST', 'CT')))
qdo(qgam.model, 0.5, wald_gam,
    comp=list(Llama_F=c('Avg.Low', 'Avg.High', 'High'),
        Condition=c('ST', 'CT')))
qdo(qgam.model, 0.5, wald_gam,
    comp=list(SRT=c('Slow', 'Avg.Slow', 'Avg.Fast', 'Fast'),
        Condition=c('ST', 'CT')))

###########
### BRM ###
###########

# note: it is expected that the estimates will be slightly different
#       for each specific chain/iteration run
summary(brm.model <- brm(bf(rollSD ~
    (Type + Llama_F + SRT) * Condition +
    TrialOrder.z +
    (1|Item) +
    (1|Participant) +
    (0+TrialOrder.z|Participant),
    quantile=0.5),
    data=sd_data,
    family=asym_laplace(),
    chains=4, iter=4000, cores=4,
    control=list(max_treedepth=20)))
# Group-Level Effects: 
# ~Item (Number of levels: 96) 
#               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     0.04      0.01     0.02     0.05 1.00     2636     3360
# 
# ~Participant (Number of levels: 45) 
#                  Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)        0.12      0.02     0.10     0.16 1.00     2078     3443
# sd(TrialOrder.z)     0.03      0.01     0.02     0.05 1.00     2786     3201
# 
# Population-Level Effects: 
#                               Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept                         0.53      0.05     0.43     0.63 1.00     1739     2947
# Typecase                         -0.06      0.02    -0.11    -0.01 1.00     3424     4212
# Typecollocation                   0.06      0.03     0.00     0.11 1.00     3537     4459
# Typesubordination                -0.10      0.02    -0.15    -0.05 1.00     3506     4423
# Llama_FAvg.High                  -0.04      0.06    -0.15     0.07 1.00     1924     3106
# Llama_FHigh                      -0.07      0.05    -0.17     0.02 1.00     1751     2838
# SRTAvg.Slow                       0.10      0.06    -0.02     0.21 1.00     1607     2528
# SRTAvg.Fast                       0.08      0.06    -0.03     0.19 1.00     1648     2773
# SRTFast                           0.05      0.06    -0.07     0.16 1.00     1592     2766
# ConditionCT                      -0.06      0.03    -0.12     0.01 1.00     3034     4050
# TrialOrder.z                      0.01      0.01    -0.01     0.02 1.00     4636     5299
# Typecase:ConditionCT              0.01      0.03    -0.05     0.07 1.00     4034     5029
# Typecollocation:ConditionCT      -0.04      0.03    -0.10     0.03 1.00     3833     4926
# Typesubordination:ConditionCT    -0.04      0.03    -0.10     0.01 1.00     3755     4461
# Llama_FAvg.High:ConditionCT       0.01      0.03    -0.05     0.07 1.00     4879     5294
# Llama_FHigh:ConditionCT           0.10      0.02     0.05     0.15 1.00     5135     5171
# SRTAvg.Slow:ConditionCT          -0.04      0.03    -0.10     0.02 1.00     4488     5373
# SRTAvg.Fast:ConditionCT          -0.11      0.03    -0.17    -0.05 1.00     4793     5325
# SRTFast:ConditionCT               0.04      0.03    -0.02     0.09 1.00     4304     5364
# 
# Family Specific Parameters: 
#          Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sigma        0.12      0.00     0.12     0.12 1.00     8568     5222
# quantile     0.50      0.00     0.50     0.50 1.00     8000     8000


