library(mlogit)
library(gmnl)
library(data.table)
library(rstan)
library(bayesplot)

# setwd("~/Desktop/lcmnl/")

# Data ===================================================================================
data = fread("data.csv") 

# simple cbc analysis with 131 students with 14 tasks each
# 2 alternatives (chocolate bars) + outside option 
# 4 attributes:
#    brands: 1, 2, 3 (ref)
#    cocoa: low, middle (ref), high
#    fairtrade label: 1 (yes), no (ref)
#    price (in Euro): 0.89, 0.99, 1.19, 1.49, 1.99
# 2 demographic variables:  
#    female: 1 (yes), 0 (no)
#    age > 25: 1 (yes), 0 (no)

# Some benchmarks using mlogit and gmnl ==================================================
data_ml = mlogit.data(data, choice = "choice", shape = "long", 
                      alt.var = "alt",  id.var = "id")

# MNL ------------------------------------------------------------------------------------
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

# LC MNL (no demographics) ---------------------------------------------------------------
starting_values = function(S, D = 0, mnl_coef, disp = 0.5) {
  c(rep(mnl_coef, S), runif((S - 1) * (1 + D), -0.1, 0.1)) * 
    runif(S * length(mnl_coef) + (S - 1) * (1 + D), 1 - disp, 1 + disp)
}


S = 2
lc2 = gmnl(choice ~ none + brand_1 + brand_2 + ftlabel + cocoa_low + cocoa_high + 
             price | 0 | 0 | 0 | 1, data = data_ml, 
           model = "lc", Q = S, panel = TRUE, print.level = 1,
           start = starting_values(S, D = 0, mnl_coef = coef(mnl)))
summary(lc2)
#                     Estimate Std. Error  z-value  Pr(>|z|)    
# class.1.none       -0.827475   0.220489  -3.7529 0.0001748 ***
# class.1.brand_1     0.281722   0.078209   3.6022 0.0003156 ***
# class.1.brand_2    -0.256717   0.085181  -3.0138 0.0025801 ** 
# class.1.ftlabel     0.454690   0.063049   7.2117 5.524e-13 ***
# class.1.cocoa_low   0.974458   0.093991  10.3676 < 2.2e-16 ***
# class.1.cocoa_high -0.564673   0.101736  -5.5504 2.851e-08 ***
# class.1.price      -1.168288   0.169179  -6.9056 4.998e-12 ***
# class.2.none       -4.366493   0.274101 -15.9303 < 2.2e-16 ***
# class.2.brand_1     0.276319   0.057310   4.8215 1.425e-06 ***
# class.2.brand_2    -0.183262   0.057142  -3.2071 0.0013407 ** 
# class.2.ftlabel     0.134961   0.046031   2.9319 0.0033686 ** 
# class.2.cocoa_low   0.027367   0.076930   0.3557 0.7220396    
# class.2.cocoa_high -0.067419   0.080196  -0.8407 0.4005276    
# class.2.price      -1.231273   0.119807 -10.2772 < 2.2e-16 ***
# (class)2            0.199699   0.048741   4.0971 4.183e-05 ***
#   
# ll = -1580.4


# LC MNL (with demographics) -------------------------------------------------------------
lc2d = gmnl(choice ~ none + brand_1 + brand_2 + ftlabel + cocoa_low + cocoa_high + 
              price | 0 | 0 | 0 | 1 + female + age_26, data = data_ml, 
            model = "lc", Q = S, panel = TRUE, print.level = 1,
            start = starting_values(S, D = 2, mnl_coef = coef(mnl)))
summary(lc2d)

#                     Estimate Std. Error  z-value  Pr(>|z|)    
# class.1.none       -0.821604   0.220244  -3.7304 0.0001912 ***
# class.1.brand_1     0.279559   0.078754   3.5498 0.0003855 ***
# class.1.brand_2    -0.252082   0.085004  -2.9655 0.0030217 ** 
# class.1.ftlabel     0.456279   0.062988   7.2439 4.359e-13 ***
# class.1.cocoa_low   0.978009   0.093442  10.4665 < 2.2e-16 ***
# class.1.cocoa_high -0.568238   0.102629  -5.5368 3.081e-08 ***
# class.1.price      -1.164345   0.169539  -6.8677 6.523e-12 ***
# class.2.none       -4.357585   0.272632 -15.9834 < 2.2e-16 ***
# class.2.brand_1     0.277889   0.057518   4.8313 1.356e-06 ***
# class.2.brand_2    -0.185631   0.057058  -3.2534 0.0011405 ** 
# class.2.ftlabel     0.134666   0.046004   2.9273 0.0034195 ** 
# class.2.cocoa_low   0.025752   0.077164   0.3337 0.7385868    
# class.2.cocoa_high -0.065375   0.080844  -0.8087 0.4187108    
# class.2.price      -1.233753   0.120104 -10.2723 < 2.2e-16 ***
# (class)2            0.513920   0.077243   6.6533 2.866e-11 ***
# female:class2      -0.525981   0.099300  -5.2969 1.178e-07 ***
# class2:age_26      -0.272804   0.104634  -2.6072 0.0091283 ** 
#
# ll = -1579.3


# Stan ===================================================================================

# create model matrix
X = model.matrix(choice ~ 0 + none + brand_1 + brand_2 + ftlabel + 
                   cocoa_low + cocoa_high + price, data = data)

# MNL ------------------------------------------------------------------------------------
mnl_model = stan_model(file = "mnl.stan")

# build stan data list
index_n = data[, .(id = first(id), y = row[choice == 1], 
                   start_n = first(row), end_n = last(row)), by = obs]
data_stan = list(N = max(index_n$obs),
                 M = nrow(X),
                 K = ncol(X), 
                 X = X,
                 y = index_n$y,
                 start_n = index_n$start_n,
                 end_n = index_n$end_n,
                 sig_beta = 100)

# map ------------------------------------------------------------------------------------
mnl_map = optimizing(mnl_model, data = data_stan, hessian = TRUE, verbose = TRUE, 
                     algorithm = "BFGS", iter = 1000, 
                     init = 0, tol_grad = 1e-16, tol_rel_grad = 1e-16)

data.table(par = colnames(X),
           est = round(mnl_map$par[1:7], 4),
           se = round(sqrt(diag(solve(-mnl_map$hessian))), 4))
#           par     est     se
# 1:       none -2.0144 0.1288
# 2:    brand_1  0.2602 0.0439
# 3:    brand_2 -0.1897 0.0452
# 4:    ftlabel  0.2340 0.0348
# 5:  cocoa_low  0.4091 0.0535
# 6: cocoa_high -0.2609 0.0538
# 7:      price -1.1577 0.0927

mnl_map$value
# -1815.777

# full bayes -----------------------------------------------------------------------------
mnl_mcmc = sampling(mnl_model, data = data_stan, pars = "beta", 
                    init = 0, seed = 123, warmup = 200, iter = 1200, cores = 4)

print(mnl_mcmc, digits_summary = 4, probs = c(0.025, 0.5, 0.975),
      pars = c("beta", "lp__"))
#                 mean se_mean     sd       2.5%        50%      97.5% n_eff   Rhat
# beta[1,1]    -2.0185  0.0024 0.1327    -2.2810    -2.0178    -1.7681  3040 1.0017
# beta[2,1]     0.2600  0.0007 0.0439     0.1732     0.2603     0.3470  3850 1.0009
# beta[3,1]    -0.1897  0.0007 0.0457    -0.2805    -0.1905    -0.1000  3992 0.9999
# beta[4,1]     0.2338  0.0006 0.0350     0.1624     0.2333     0.3024  4008 0.9996
# beta[5,1]     0.4104  0.0009 0.0540     0.3047     0.4109     0.5148  3506 1.0011
# beta[6,1]    -0.2610  0.0009 0.0538    -0.3668    -0.2619    -0.1554  3651 1.0001
# beta[7,1]    -1.1608  0.0017 0.0942    -1.3491    -1.1618    -0.9776  2996 1.0020
# lp__      -1819.2878  0.0458 1.8428 -1823.5213 -1818.9791 -1816.5947  1616 1.0036


post_mnl_mcmc = extract(mnl_mcmc, inc_warmup = TRUE, permuted = FALSE)

mcmc_trace(post_mnl_mcmc, n_warmup = 200,
           pars = c("beta[1,1]", "beta[2,1]", "beta[7,1]", "lp__"))


# LC MNL (no demographcs) ----------------------------------------------------------------
lcmnl_model = stan_model(file = "lcmnl.stan")

# build individual indeces
index_i = data[,.(start_i = first(obs), end_i = last(obs)), by = id]
# ... and add to data list
data_stan$S = 2
data_stan$I = index_i[, length(id)]
data_stan$start_i = index_i$start_i
data_stan$end_i = index_i$end_i
data_stan$sig_gamma = 100
data_stan$Z = rep(1, data_stan$I) %x% matrix(c(1, 0), 2, 1)
data_stan$L = ncol(data_stan$Z)

# map ------------------------------------------------------------------------------------
lc2_map = optimizing(lcmnl_model, data = data_stan, hessian = TRUE, verbose = TRUE, 
                     algorithm = "BFGS", iter = 1000, 
                     init = 0, tol_grad = 1e-16, tol_rel_grad = 1e-16)
lc2_map$value
data.table(par = c(paste0("class.", rep(1:2, each = 7), ".", 
                          rep(colnames(X), 2)), "alpha"),
           est = round(lc2_map$par[1:15], 4),
           se = round(sqrt(diag(solve(-lc2_map$hessian))), 4))

#                    par     est     se
#  1:       class.1.none -0.8275 0.2205
#  2:    class.1.brand_1  0.2817 0.0782
#  3:    class.1.brand_2 -0.2567 0.0852
#  4:    class.1.ftlabel  0.4547 0.0630
#  5:  class.1.cocoa_low  0.9744 0.0940
#  6: class.1.cocoa_high -0.5647 0.1017
#  7:      class.1.price -1.1683 0.1692
#  8:       class.2.none -4.3663 0.2741
#  9:    class.2.brand_1  0.2763 0.0573
# 10:    class.2.brand_2 -0.1833 0.0571
# 11:    class.2.ftlabel  0.1350 0.0460
# 12:  class.2.cocoa_low  0.0274 0.0769
# 13: class.2.cocoa_high -0.0674 0.0802
# 14:      class.2.price -1.2312 0.1198
# 15:              alpha -0.1996 0.1823


# full bayes -----------------------------------------------------------------------------
lc2_mcmc = sampling(lcmnl_model, data = data_stan, pars = c("beta", "gamma"), 
                    init = 0, seed = 123, warmup = 200, iter = 1200, cores = 4)

print(lc2_mcmc, digits_summary = 4, probs = c(0.025, 0.5, 0.975),
      pars = c("beta", "gamma", "lp__"))

post_lc2_mcmc = extract(lc2_mcmc, inc_warmup = TRUE, permuted = FALSE)


mcmc_trace(post_lc2_mcmc, n_warmup = 200,
           pars = c("beta[1,1]", "beta[1,2]", "beta[7,1]", "beta[7,2]", 
                    "gamma[1]", "lp__"))


# LC MNL (with demographcs) --------------------------------------------------------------
data_stan$Z = as.matrix(data[task == 1 & alt == 1,.(asc = 1, female, age_26)]) %x% 
  matrix(c(1, 0), 2, 1)
data_stan$L = ncol(data_stan$Z)


# map ------------------------------------------------------------------------------------
lc2d_map = optimizing(lcmnl_model, data = data_stan, hessian = TRUE, verbose = TRUE, 
                      algorithm = "BFGS", iter = 1000, 
                      init = 0, tol_grad = 1e-16, tol_rel_grad = 1e-16)

data.table(par = c(paste0("class.", rep(1:2, each = 7), ".", 
                          rep(colnames(X), 2)), c("asc", "female", "age_26")),
           est = round(lc2d_map$par[1:17], 4),
           se = round(sqrt(diag(solve(-lc2d_map$hessian))), 4))

#                    par     est     se
#  1:       class.1.none -0.8216 0.2202
#  2:    class.1.brand_1  0.2796 0.0788
#  3:    class.1.brand_2 -0.2521 0.0850
#  4:    class.1.ftlabel  0.4563 0.0630
#  5:  class.1.cocoa_low  0.9780 0.0934
#  6: class.1.cocoa_high -0.5682 0.1026
#  7:      class.1.price -1.1643 0.1695
#  8:       class.2.none -4.3575 0.2726
#  9:    class.2.brand_1  0.2779 0.0575
# 10:    class.2.brand_2 -0.1856 0.0571
# 11:    class.2.ftlabel  0.1347 0.0460
# 12:  class.2.cocoa_low  0.0258 0.0772
# 13: class.2.cocoa_high -0.0654 0.0808
# 14:      class.2.price -1.2337 0.1201
# 15:                asc -0.5139 0.2890
# 16:             female  0.5259 0.3715
# 17:             age_26  0.2728 0.3915

lc2d_map$value
# -1579.287


# full bayes -----------------------------------------------------------------------------
lc2d_mcmc = sampling(lcmnl_model, data = data_stan, pars = c("beta", "gamma"), 
                     init = 0, seed = 123, warmup = 200, iter = 1200, cores = 4)

print(lc2d_mcmc, digits_summary = 4, probs = c(0.025, 0.5, 0.975),
      pars = c("beta", "gamma", "lp__"))

post_lc2d_mcmc = extract(lc2d_mcmc, inc_warmup = TRUE, permuted = FALSE)


mcmc_trace(post_lc2d_mcmc, n_warmup = 200,
           pars = c("beta[1,1]", "beta[1,2]", "beta[7,1]", "beta[7,2]", 
                    "gamma[1]", "gamma[2]", "gamma[3]", "lp__"))


# lcmnl (constrained) (with demos) ------------------------------------------

lcmnl_model_constrained = stan_model(file = "lcmnl-constrained.stan")

lc2d_constrained_map = optimizing(lcmnl_model_constrained, data = data_stan, hessian = TRUE, verbose = TRUE, 
                      algorithm = "BFGS", iter = 1000, 
                      init = 0, tol_grad = 1e-16, tol_rel_grad = 1e-16)

data.table(par = c(paste0("class.", rep(1:2, each = 7), ".", 
                          rep(colnames(X), 2)), c("asc", "female", "age_26")),
           est = round(lc2d_constrained_map$par[1:17], 4),
           se = round(sqrt(diag(solve(-lc2d_constrained_map$hessian))), 4))

# full bayes -----------------------------------------------------------------------------
lc2d_constrained_mcmc = sampling(lcmnl_model_constrained, data = data_stan, pars = c("beta", "gamma"), 
                     init = 0, seed = 123, warmup = 200, iter = 1200, cores = 4)

print(lc2d_constrained_mcmc, digits_summary = 4, probs = c(0.025, 0.5, 0.975),
      pars = c("beta", "gamma", "lp__"))

post_lc2d_constrained_mcmc = extract(lc2d_constrained_mcmc, inc_warmup = TRUE, permuted = FALSE)


mcmc_trace(post_lc2d_constrained_mcmc, n_warmup = 200,
           pars = c("beta[1,1]", "beta[1,2]", "beta[7,1]", "beta[7,2]", 
                    "gamma[1]", "gamma[2]", "gamma[3]", "lp__"))
