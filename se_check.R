# Setup ==================================================================================
library(mlogit)
library(gmnl)
library(flexmix)

# Data ===================================================================================
data = read.csv("data.csv") 

# simple cbc analysis with 131 students with 14 tasks each
# 2 alternatives (chocolate bars) + outside option 
# 4 attributes:
#    brands: 1, 2, 3 (ref)
#    cocoa content: low, middle (ref), high
#    fairtrade label: 1 (yes), no (ref)
#    price (in Euro): 0.89, 0.99, 1.19, 1.49, 1.99
# 2 demographic variables:  
#    female: 1 (yes), 0 (no)
#    age > 25: 1 (yes), 0 (no)

# Build data for mlogit and gmnl 
data_ml = mlogit.data(data, choice = "choice", shape = "long", 
                      alt.var = "alt",  id.var = "id")

# Estimation: MNL + LC-MNL with 2 classes incl. two demographics as concomitant vars. ====
# GMNL -----------------------------------------------------------------------------------

# MNL 
mnl = gmnl(choice ~ none + brand_1 + brand_2 + ftlabel + 
             cocoa_low + cocoa_high +  price | 0, 
           data = data_ml)
summary(mnl)
#             Estimate Std. Error  z-value  Pr(>|z|)    
# none       -2.014359   0.128803 -15.6391 < 2.2e-16 ***
# brand_1     0.260213   0.043900   5.9274 3.077e-09 ***
# brand_2    -0.189690   0.045240  -4.1930 2.753e-05 ***
# ftlabel     0.233968   0.034825   6.7184 1.838e-11 ***
# cocoa_low   0.409129   0.053462   7.6527 1.976e-14 ***
# cocoa_high -0.260922   0.053805  -4.8494 1.238e-06 ***
# price      -1.157713   0.092683 -12.4911 < 2.2e-16 ***
#
# ll = -1815.8

# LC-MNL
lc2 = gmnl(choice ~ none + brand_1 + brand_2 + ftlabel + cocoa_low + cocoa_high + 
             price | 0 | 0 | 0 | 1 + female + age_26, data = data_ml, 
           model = "lc", Q = 2, panel = TRUE, print.level = 1)
summary(lc2)

#                    Estimate Std. Error  z-value  Pr(>|z|)    
# class.1.none       -4.357620   0.272641 -15.9830 < 2.2e-16 ***
# class.1.brand_1     0.277879   0.057517   4.8312 1.357e-06 ***
# class.1.brand_2    -0.185620   0.057058  -3.2532 0.0011411 ** 
# class.1.ftlabel     0.134659   0.046004   2.9272 0.0034208 ** 
# class.1.cocoa_low   0.025770   0.077163   0.3340 0.7384061    
# class.1.cocoa_high -0.065399   0.080842  -0.8090 0.4185324    
# class.1.price      -1.233694   0.120102 -10.2721 < 2.2e-16 ***
# class.2.none       -0.821643   0.220244  -3.7306 0.0001910 ***
# class.2.brand_1     0.279567   0.078753   3.5499 0.0003853 ***
# class.2.brand_2    -0.252096   0.085005  -2.9657 0.0030204 ** 
# class.2.ftlabel     0.456284   0.062988   7.2440 4.357e-13 ***
# class.2.cocoa_low   0.977982   0.093442  10.4662 < 2.2e-16 ***
# class.2.cocoa_high -0.568168   0.102628  -5.5362 3.091e-08 ***
# class.2.price      -1.164387   0.169539  -6.8680 6.513e-12 ***
# (class)2           -0.513881   0.077243  -6.6528 2.876e-11 ***
# female:class2       0.525977   0.099299   5.2969 1.178e-07 ***
# class2:age_26       0.272797   0.104634   2.6072 0.0091298 ** 
#
# ll = -1579.3

# flexmix --------------------------------------------------------------------------------
# MNL
flx1 = flexmix(choice ~ 0 + none + brand_1 + brand_2 + ftlabel + 
                 cocoa_low + cocoa_high +  price | id,
               model = FLXMRcondlogit(strata = ~obs), data = data,  k = 1)
summary(flx1)
#        prior size post>0 ratio
# Comp.1     1 5502   5502     1
# 
# 'log Lik.' -1815.777 (df=7)
# AIC: 3645.554   BIC: 3691.844 

flx1 = refit(flx1)
summary(flx1)
#             Estimate Std. Error  z value  Pr(>|z|)    
# none       -2.014359   0.128803 -15.6391 < 2.2e-16 ***
# brand_1     0.260213   0.043900   5.9274 3.077e-09 ***
# brand_2    -0.189690   0.045240  -4.1930 2.753e-05 ***
# ftlabel     0.233968   0.034825   6.7184 1.838e-11 ***
# cocoa_low   0.409129   0.053462   7.6527 1.968e-14 ***
# cocoa_high -0.260922   0.053805  -4.8494 1.238e-06 ***
# price      -1.157713   0.092683 -12.4911 < 2.2e-16 ***

# LC_MNL
set.seed(1)
flx2 = stepFlexmix(choice ~ 0 + none + brand_1 + brand_2 + ftlabel + 
                     cocoa_low + cocoa_high +  price | id,
                   model = FLXMRcondlogit(strata = ~ obs), 
                   concomitant = FLXPmultinom(formula = ~ 1 + female + age_26),
                   data = data, k = 2, nrep = 10, drop = TRUE)
summary(flx2)
#        prior size post>0 ratio
# Comp.1 0.551 3066   3654 0.839
# Comp.2 0.449 2436   3822 0.637
# 
# 'log Lik.' -1579.286 (df=17)
# AIC: 3192.571   BIC: 3304.99 

# LC-MNL 
flx2 = refit(flx2)
summary(flx2, which = "model")
# $Comp.1
#             Estimate Std. Error  z value  Pr(>|z|)    
# none       -4.355226   0.272411 -15.9877 < 2.2e-16 ***
# brand_1     0.277961   0.057517   4.8326 1.347e-06 ***
# brand_2    -0.185545   0.057053  -3.2521  0.001145 ** 
# ftlabel     0.134631   0.046001   2.9267  0.003426 ** 
# cocoa_low   0.025950   0.077160   0.3363  0.736634    
# cocoa_high -0.065471   0.080835  -0.8099  0.417976    
# price      -1.233194   0.120086 -10.2692 < 2.2e-16 ***
# 
# $Comp.2
#             Estimate Std. Error z value  Pr(>|z|)    
# none       -0.821817   0.220240 -3.7315 0.0001904 ***
# brand_1     0.279536   0.078760  3.5492 0.0003864 ***
# brand_2    -0.252101   0.085000 -2.9659 0.0030181 ** 
# ftlabel     0.456371   0.062985  7.2457 4.302e-13 ***
# cocoa_low   0.978092   0.093442 10.4673 < 2.2e-16 ***
# cocoa_high -0.568215   0.102630 -5.5365 3.085e-08 ***
# price      -1.164198   0.169526 -6.8674 6.540e-12 ***

summary(flx2, which = "concomitant")
#             Estimate Std. Error z value Pr(>|z|)  
# (Intercept) -0.51451    0.28903 -1.7801  0.07506 .
# female       0.52605    0.37156  1.4158  0.15683  
# age_26       0.27309    0.39152  0.6975  0.48548  


# Analysis of results ====================================================================

# the parameter estimates are virtually identical
plot(coef(lc2), flx2@coef, ylab = "coefs. gmnl", xlab = "coefs. flexmix")
abline(0, 1)

# however, we see some differences for the SEs
se_gmnl = sqrt(diag(vcov(lc2))) # gmnl
se_flexmix = sqrt(diag(flx2@vcov)) # flexmix

ratio = round(se_flexmix / se_gmnl, 2)
table(ratio)
#  1 3.74 
# 14    3 

ratio[ratio > 1]
# - SEs for the utility parameters are almost the same
# - SEs for the concomitant vars are about 3.742 times large in flexmix
# - 3.74^2 is approx. 14, which is the number of choice tasks per respondent
# - My hunch is, that gmnl incorrectly uses (in the case of panel data) the number of obs.
#   when computing the SE for class parameters instead of the number of respondents.
