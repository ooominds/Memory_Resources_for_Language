
load('loglin_data.rda')

##################
### Log-Linear ###
##################

freqdat = data.frame(tabdat <- xtabs(~
    Match +
    Type +
    Condition,
    data=loglin_data))
ftable(tabdat)
#                     Condition  ST  CT
# Match Type                           
# 0     subordination             6   6
#       case                     15  21
#       collocation              64  81
#       aspect                   69  48
# 1     subordination           521 528
#       case                    515 513
#       collocation             468 451
#       aspect                  456 480

# NOTE: Models with good fit are non-significant!
#       Model comparisons are interpreted as usual (with p < 0.05 etc.)

# no effect; i.e., independence
ll.freqdat.indep = loglin(tabdat, list(c(1), c(2,3)), param=TRUE, fit=TRUE)
1 - pchisq(ll.freqdat.indep$lrt, ll.freqdat.indep$df)
# [1] 0

# only type has a direct effect
ll.freqdat.type = loglin(tabdat, list(c(1,2), c(2,3)), param=TRUE, fit=TRUE)
1 - pchisq(ll.freqdat.type$lrt, ll.freqdat.type$df)
# [1] 0.1030885

# only condition has a direct effect
ll.freqdat.condition = loglin(tabdat, list(c(1,3), c(2,3)), param=TRUE, fit=TRUE)
1 - pchisq(ll.freqdat.condition$lrt, ll.freqdat.condition$df)
# [1] 0

# both type and condition have direct effects
# (joint effect is meaningless; variable list is exausted)
ll.freqdat.both = loglin(tabdat, list(c(1,2), c(1,3), c(2,3)), param=TRUE, fit=TRUE)
1 - pchisq(ll.freqdat.both$lrt, ll.freqdat.both$df)
# [0] 0.05274842

# comparatively, ll.freqdat.type is simpler and better model;
# condition does not improve the fit significantly (but)
1 - pchisq(abs(ll.freqdat.type$lrt-ll.freqdat.both$lrt),
    abs(ll.freqdat.type$df-ll.freqdat.both$df))
# [1] 0.930331

# observed vs. expected (fitted)
(freqdat2 = data.frame(cbind(freqdat,
    'loglinFreq'=data.frame(ll.freqdat.type$fit)[,4])))
#    Match          Type Condition Freq loglinFreq
# 1      0 subordination        ST    6   5.960415
# 2      1 subordination        ST  521 521.039585
# 3      0          case        ST   15  17.932331
# 4      1          case        ST  515 512.067669
# 5      0   collocation        ST   64  72.500000
# 6      1   collocation        ST  468 459.500000
# 7      0        aspect        ST   69  58.333333
# 8      1        aspect        ST  456 466.666667
# 9      0 subordination        CT    6   6.039585
# 10     1 subordination        CT  528 527.960415
# 11     0          case        CT   21  18.067669
# 12     1          case        CT  513 515.932331
# 13     0   collocation        CT   81  72.500000
# 14     1   collocation        CT  451 459.500000
# 15     0        aspect        CT   48  58.666667
# 16     1        aspect        CT  480 469.333333

###############################################
### Checking the effects of Llama_F and SRT ###
###############################################

freqdatAll = data.frame(tabdatAll <- xtabs(~
    Match +
    Type +
    Condition +
    Llama_F +
    SRT,
    data=loglin_data))

# base model; type only
ll.freqdat.type.wIndDiff1 = loglin(tabdatAll, list(c(1,2), c(2,3,4,5)), param=TRUE, fit=TRUE)
1 - pchisq(ll.freqdat.type.wIndDiff1$lrt, ll.freqdat.type.wIndDiff1$df)
# [1] 0.4643417

# joint effect of type and Llama_F
ll.freqdat.type.wIndDiff2 = loglin(tabdatAll, list(c(1,2,4), c(2,3,4,5)), param=TRUE, fit=TRUE)
1 - pchisq(abs(ll.freqdat.type.wIndDiff1$lrt - ll.freqdat.type.wIndDiff2$lrt),
    abs(ll.freqdat.type.wIndDiff1$df - ll.freqdat.type.wIndDiff2$df))
# [1] 0.318864

# joint effect of type and SRT
ll.freqdat.type.wIndDiff3 = loglin(tabdatAll, list(c(1,2,5), c(2,3,4,5)), param=TRUE, fit=TRUE)
1 - pchisq(abs(ll.freqdat.type.wIndDiff1$lrt - ll.freqdat.type.wIndDiff3$lrt),
    abs(ll.freqdat.type.wIndDiff1$df - ll.freqdat.type.wIndDiff3$df))
# [1] 0.1660264


