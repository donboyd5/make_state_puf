
# horserace among different methods of constructing state weights for a microdata file

# TODO: develop best possible starting weights using either:
#  a MNL function that includes some noise, on assumption we could get something like that
#  or a distance function that is more detailed


#****************************************************************************************************
#                Libraries and globals ####
#****************************************************************************************************
source(file.path(PROJHOME, "r/includes", "libraries.r"))
library("kableExtra")

# https://www.istat.it/en/methods-and-tools/methods-and-it-tools/process/processing-tools/regenesees/regenesees-package-source?email=donboyd5%40gmail.com&hash=e7c5097b204bdffe5fb4aa889eadefa4
# https://www.istat.it/en/methods-and-tools/methods-and-it-tools/process/processing-tools/regenesees/regenesees-package-source

# install.packages(path_to_file, repos = NULL, type="source")
# path <- "C:/Users/donbo/Downloads/ReGenesees_1.9.tar.gz"
# install.packages(path, repos = NULL, type="source")
library(ReGenesees)
??ReGenesees


# CalibrateSSB
library(CalibrateSSB)

source(file.path(PROJHOME, "r/includes", "functions_horserace.r"))




#****************************************************************************************************
#                Globals ####
#****************************************************************************************************
# probs <- c(0, .05, .1, .25, .5, .75, .9, .95, .99, 1) # for quantiles
probs <- c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1) # for quantiles


#****************************************************************************************************
#                functions ####
#****************************************************************************************************



#****************************************************************************************************
#                construct data that can be used to test approaches ####
#****************************************************************************************************
# create a subset of ACS person records. This way we will know true values for variables we target
# and true values of variables we do not target
# ONETIME: run 0b_get_and_save_ACS_person_records.r


#****************************************************************************************************
#               get a sample and define associated constraints ####
#****************************************************************************************************
system.time(persons <- readRDS("d:/temp/persons.rds"))
glimpse(persons)

set.seed(1234)
samp <- persons %>%
  filter(stabbr %in% c("AL", "CA", "NY"), agep>=15) %>%
  sample_n(1e6) %>%
  select(-sporder, -st)
glimpse(samp)

samp_clean <- samp %>%
  mutate(n=1 / pwgtp, pop=1,
         income=wagp + intp + pap + retp + ssip + ssp,
         incgroup=ntile(income, 10)) %>%
  mutate_at(vars(intp, pap, retp, ssip, ssp, wagp, income), list(n = ~ as.numeric(. !=0)))
glimpse(samp_clean)
summary(samp_clean)
count(samp_clean, stabbr)
count(samp_clean, incgroup)
ht(samp_clean)


#.. define constraints data: 1 per income group per state ----
constraints_true <- samp_clean %>%
  group_by(stabbr, incgroup) %>%
  summarise_at(vars(n, pop, income, intp, pap, retp, ssip, ssp, wagp, ends_with("_n")),
               list(~ sum(. * pwgtp))) %>%
  ungroup

# add a different amount of noise to each constraint
set.seed(1234)
constraints_noisy <- constraints_true %>%
  gather(variable, value, -stabbr, -incgroup, -n) %>%
  group_by(stabbr, incgroup, n, variable) %>%
  mutate(value=value * (1 + rnorm(n(), 0, .02))) %>%
  ungroup %>%
  spread(variable, value) %>%
  select(names(constraints_true)) # keep the same order

# decide which to use
constraints <- constraints_true
# constraints <- constraints_noisy

#.. define consdata and sampdata for a specific subset ----
# cvars has the names of the constraint variables
# consdata will have 1 record with all constraints for the subset
# sampdata will have all records in the subset, including constraint vars and identifying/other vars
#.. define constraint variables ---
(cvars <- setdiff(names(constraints), c("stabbr", "incgroup", "n")))

# look at a subset that we'll want to use
st <- "NY"
ygrp <- 3
constraints %>% filter(stabbr==st, incgroup==ygrp) %>% select(cvars)
samp_clean %>% filter(stabbr==st, incgroup==ygrp) %>% select(cvars) %>% ht

# multiplying 
consdata <- constraints %>% filter(stabbr==st, incgroup==ygrp) %>% select(cvars)
sampdata <- samp_clean %>% filter(incgroup==ygrp) %>% select(serialno, stabbr, pwgtp, cvars)
count(sampdata, stabbr)

# make sure we know whether consdata has noise
bind_rows(consdata,
          constraints_true %>% filter(stabbr==st, incgroup==ygrp) %>% select(cvars))

#****************************************************************************************************
#               check out a starting point ####
#****************************************************************************************************
wts0 <- sampdata$pwgtp * consdata$pop / sum(sampdata$pwgtp) # good starting point
# wts0a <- rep(consdata$pop / nrow(sampdata), nrow(sampdata)) # not bad
# wts0b <- rep(1, nrow(sampdata)) # bad

calc_constraints(wts0, sampdata, names(consdata))

# pct difference from constraint targets at a few possible starting points
pdiff(wts0, sampdata, consdata) %>% round(2)
# pdiff(wts0a, sampdata, consdata) %>% round(2)
# pdiff(wts0b, sampdata, consdata) %>% round(2)

# objective function at a few possible starting points
objfn(wts0, sampdata, consdata)
# objfn(wts0a, sampdata, consdata)
# objfn(wts0b, sampdata, consdata)
# we are ready to begin the horserace

#****************************************************************************************************
#               function for uniform optimization call ####
#****************************************************************************************************
uni_opt <- function(method, wts0, data, constraints, objfn=NULL, ...){
  # call any of the methods and get a uniform return value
  
  methods <- c("simann", "mma", "calibrate")
  if(!method %in% methods){
    err_msg1 <- paste0("STOP: Method ", method, " not supported. Method must be one of:")
    err_msg2 <- paste(methods, collapse=", ")
    print(err_msg1)
    print(err_msg2)
    return(NULL)
  }
  
  
  args <- list(...) # only gets the dots values; match.call() gets all names, but not the values
  if(hasArg(maxiter)) maxiter <- args$maxiter else maxiter <- 1000 # always define even if not needed
  
  if(method=="simann"){
    set.seed(seed)
    a <- proc.time()
    sa_result <- sim_ann(objfn, wts0, data, constraints, 
                      p=2, niter = maxiter, step = 0.1, 
                      phase_iter=500, max_recshare=.3, max_wtshare=1)
    b <- proc.time()
    
    result <- list()
    result$method <- method
    result$obj <- sa_result$best_obj
    result$weights <- sa_result$best_weights
    result$iter <- sa_result$iterations
    result$elapsed_seconds <- unname((b - a)[3])
    result$opt_object <- sa_result
    
  } else if(method=="mma"){
    a <- proc.time()
    mma_result <- mma_nloptr(objfn, wts0, data, constraints, niter=maxiter)
    b <- proc.time()
    
    result <- list()
    result$method <- method
    result$obj <- mma_result$value
    result$weights <- mma_result$par
    result$iter <- mma_result$iter
    result$elapsed_seconds <- unname((b - a)[3])
    result$opt_object <- mma_result
    
  } else if(method=="calibrate"){
    a <- proc.time()
    calib_result <- calibrate_reweight(wts0, data, constraints)
    b <- proc.time()
    
    result <- list()
    result$method <- method
    result$obj <- objfn(weights(calib_result), data, constraints)
    result$weights <- unname(weights(calib_result))
    result$iter <- 0
    result$elapsed_seconds <- unname((b - a)[3])
    result$opt_object <- NULL
    result$notes <- "(1) obj is NOT from the calibrate function, it is the general objfn; (2) result object not returned - too big"
  }
  return(result)
}

tmp <- uni_opt(wts0, sampdata, consdata, method="calibrate", objfn=objfn, maxiter=10)
str(tmp)


#****************************************************************************************************
#               simulated annealing ####
#****************************************************************************************************
pval <- 2
seed <- 12345
nx <- 1000

# do not allow multiple records replacement or weight reduction > 1
# set.seed(seed); system.time(sa_none <- sim_ann(objfn, wts0, sampdata, consdata,
#                                                p=pval, niter = nx, step = 0.1, 
#                                                phase_iter=0, max_recshare=0, max_wtshare=0))
# 
# # allow multiple records to be reduced but NOT weight redution > 1
# set.seed(seed); system.time(sa_recs <- sim_ann(objfn, wts0, sampdata, consdata,
#                                                p=pval, niter = nx, step = 0.1, 
#                                                phase_iter=500, max_recshare=.3, max_wtshare=0))
# allow multiple records to be reduced, and allow weight reduction > 1
a <- proc.time()
set.seed(seed)
sa_wfs <- sim_ann(objfn, wts0, sampdata, consdata, 
                  p=pval, niter = 10e3, step = 0.1, 
                  phase_iter=2000, max_recshare=.5, max_wtshare=1)
b <- proc.time()
b - a
# saveRDS(sa_wfs, "d:/temp/opt_runs/sa_wfs.rds")
sa_wfs <- readRDS("d:/temp/opt_runs/sa_wfs.rds")

# str(sa_none)
# str(sa_recs)
str(sa_wfs) 

# 10k 81 secs 30k 417 secs 50k 
# also, succeeded at 5296 iterations (obj < 1e-3), 72 secs
# niter 3k phase 1k recshare .5 wtshare 1: 
objfn(sa_wfs$best_weights, sampdata, consdata)
pdiff(sa_wfs$best_weights, sampdata, consdata) %>% round(2)
quantile(sa_wfs$best_weights, probs=probs)
sum(sa_wfs$best_weights==0)



#****************************************************************************************************
#               nloptr and mma ####
#****************************************************************************************************
# what about nloptr?
library("nloptr")

# note - MMA does not seem to solve without gradient - had to cancel
# this uses the gradient
system.time(mma_result <- mma_nloptr(objfn, wts0, sampdata, consdata, niter=1000))
str(mma_result)

pdiff(wts0, sampdata, consdata) %>% round(2)
pdiff(mma_result$par, sampdata, consdata) %>% round(2)

# this (below) converged after another 679 iterations (starting from the prior result) -- 1,679 total iterations
# the obj function value was the same as prior, at precision shown
# system.time(mma_long <- mma_nloptr(objfn, mma_result$par, sampdata, consdata, niter=3000))
# str(mma_long)


#****************************************************************************************************
#               calibate from survey package ####
#****************************************************************************************************
# https://www.r-bloggers.com/survey-raking-an-illustration/
# GOOD: https://cvxr.rbind.io/cvxr_examples/cvxr_survey_calibration/
# Primary Sampling Units (PSU)
# https://stats.stackexchange.com/questions/83089/sample-weighting-with-continuous-variables
# https://faculty.washington.edu/tlumley/survey-jsm-nup.pdf p.129

# here, we penalize deviations from initial weights; the initial weights must be pretty good
library("survey")

# wts_avg <- rep(sum(wts0) / nrow(sampdata), nrow(sampdata))
# wts_init <- wts_avg

calib_rw <- calibrate_reweight(wts0, sampdata, consdata) # FAST!! Also, the best result. Does it scale?
# calib_rw <- calibrate_reweight(wts_init, sampdata, consdata) # FAST!! Also, the best result. Does it scale?
str(calib_rw)

pdiff(wts0, sampdata, consdata) %>% round(2)
pdiff(weights(calib_rw), sampdata, consdata) %>% round(2)


#****************************************************************************************************
#               simulated annealing 2-stage - don't use ####
#****************************************************************************************************
set.seed(seed); system.time(sa_2stage <- sim_ann(objfn, weights(calib_rw), sampdata, consdata, 
                                              p=pval, niter = 1000, step = 0.1, 
                                              phase_iter=500, max_recshare=.3, max_wtshare=1))
str(sa_2stage) # somewhat better than before - and VERY different from before (see below)
str(sa_wfs)


#****************************************************************************************************
#               mma 2-stage - don't use ####
#****************************************************************************************************
system.time(mma_2stage <- mma_nloptr(objfn, calib_rwt$weights, sampdata, consdata, niter=1000))
str(mma_2stage) # no better!
str(mma_result)

tibble(row=1:length(calib_rwt$weights), calib=calib_rwt$weights, mma2=mma_2stage$par) %>%
  ggplot(aes(calib, mma2)) + geom_point()


#****************************************************************************************************
#               multinomial logit ####
#****************************************************************************************************
library("mlogit")
library("mnlogit") # Fast Estimation of Multinomial Logit Models:
??mnlogit
glimpse(samp_clean)
glimpse(constraints)

glimpse(sampdata)
glimpse(consdata)

# turn constraints into average returns
# 10 per state
avg_return <- function(df, vnames) {
  nvars <- paste0(vnames, "_n")
  # mutate(df, !!vnames := !!!vnames / !!!nvars)
  df[, vnames] <- df[, vnames] / df[, nvars]
  df[, nvars] <- NULL
  df %>% mutate_at(vars(vnames), ~replace(., is.na(.), 0))
}

#****************************************************************************************************
#               djb good 6/25/2019 - multinomial logit - use full info we have ####
#****************************************************************************************************
# make average returns and cbind them to sampdata
# then create a long file
avgvars <- c("income", "intp", "pap", "retp", "ssip", "ssp", "wagp")
constraints_avg <- avg_return(constraints %>% filter(incgroup==ygrp), avgvars)

glimpse(sampdata)
# enhance sampdata with wide version of avg returns then make long file
# cwide <- constraints_avg %>%
#   select(-incgroup, -n, -pop) %>%
#   gather(vname, value, -stabbr) %>%
#   unite(vname, stabbr, vname) %>%
#   spread(vname, value)
# creates stabbr_vname but we will need vname_stabbr for mnlogit

cwide <- constraints_avg %>%
  select(-incgroup, -n, -pop) %>%
  pivot_wider(names_from=stabbr,
              values_from=c(income, intp, pap,retp, ssip, ssp, wagp)) %>%
  setNames(paste0("cons.", names(.)))
cwide

sampdata_wide <- sampdata %>%
  cbind(cwide)
glimpse(sampdata_wide)
count(sampdata_wide, income)

ivarying <- c(which(str_detect(names(sampdata_wide), "_AL")),
              which(str_detect(names(sampdata_wide), "_CA")),
              which(str_detect(names(sampdata_wide), "_NY")))
sampdata_long <- mlogit.data(sampdata_wide %>% mutate(depvar=stabbr), shape = "wide", choice = "depvar", varying = ivarying, sep="_")
glimpse(sampdata_long)

ht(sampdata_wide)
ht(sampdata_long)

m2 <- mnlogit(depvar ~ 0 | income + wagp + intp + pap + retp + ssip + 
                wagp_n + intp_n + pap_n + retp_n + ssip_n, data=sampdata_long)
summary(m2)

m2$freq # actual frequencies
apply(fitted(m2, outcome = FALSE), 2, mean) # predicted frequencies

ht(m2$probabilities)

pvals_mnl <- as_tibble(m2$probabilities) %>% mutate(stabbr=sampdata_wide$stabbr)
calib_mnl <- calibrate_reweight(pvals_mnl[, "NY"] * sampdata$pwgtp, sampdata, consdata)
str(calib_mnl)

pdiff(pvals_mnl[["NY"]] * sampdata$pwgtp, sampdata, consdata)
pdiff(weights(calib_mnl), sampdata, consdata)

tgtwts <- samp_clean %>%
  filter(incgroup==3) %>%
  mutate(wtarg=ifelse(stabbr=="NY", pwgtp, 0))

stack_base <- readRDS("d:/temp/comp_results_true.rds")
glimpse(stack_base)

sa_wfs <- readRDS("d:/temp/opt_runs/sa_wfs.rds")
sa2_calib <- calibrate_reweight(sa_wfs$best_weights, sampdata, consdata)


stack_add <- tibble(row=1:length(wts0),
                    wmnl=pvals_mnl$NY * sampdata$pwgtp,
                    wmnl_calib=weights(calib_mnl),
                    wsimann2=sa_wfs$best_weights,
                    wsimann2_calib=weights(sa2_calib),
                    wtarg=tgtwts$wtarg) %>%
  gather(method, weight, -row) %>%
  group_by(method) %>%
  mutate(obj=objfn(weight, sampdata, consdata),
         iter=0, seconds=0)
stack <- bind_rows(stack_base, stack_add)
glimpse(stack)
count(stack, method)

stack %>%
  group_by(method) %>%
  summarise_at(vars(obj, iter, seconds), ~ first(.))

stack %>%
  group_by(method) %>%
  summarise(zero_pct=sum(weight==0) / n() * 100)

stack %>%
  group_by(method) %>%
  summarise(near_zero_pct=sum(round(weight, 3)==0.000) / n() * 100)
samp_clean %>%
  filter(incgroup==3) %>%
  group_by(stabbr) %>%
  summarise(wt=sum(pwgtp)) %>%
  mutate(wtpct=wt / sum(wt) * 100)
# glimpse(samp_clean)

stack %>%
  select(row, method, weight) %>%
  spread(method, weight) %>%
  mutate(wts0=wts0) %>%
  ungroup %>%
  select(-row) %>%
  cor()

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(calc_constraints(.$weight, sampdata, names(consdata)) %>%
       t %>% 
       as.data.frame) %>%
  bind_rows(consdata %>% mutate(method="target", obj=0, iter=0, seconds=0,
                                method=factor(method))) %>%
  ungroup %>%
  arrange(method) %>%
  kable(digits=c(1, 5, 0, 0, rep(1, ncol(.) - 4)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(pdiff(.$weight, sampdata, consdata)) %>%
  kable(digits=c(1, 5, 0, 0, rep(1, ncol(.) - 4)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(qtiledf(.$weight, probs=probs)) %>%
  kable(digits=c(1, 5, rep(0, 4), rep(1, ncol(.) - 6)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  ggplot(aes(weight)) +
  geom_histogram(fill="blue", bins=50) +
  geom_vline(xintercept = mean(wts0)) +
  scale_x_continuous(breaks=seq(0, 500, 5), limits=c(0, 80)) +
  facet_wrap(~method, ncol=2)

tmp <- stack %>%
  left_join(sampdata %>% select(income) %>% mutate(row=row_number())) %>%
  bind_rows(samp_clean %>% 
              filter(stabbr=="NY", incgroup==ygrp) %>% 
              select(weight=pwgtp, income) %>%
              mutate(method="target")) %>%
  mutate(incgrp=cut(income, 5)) %>%
  group_by(method, incgrp) %>%
  summarise(income=sum(income * weight))
# glimpse(tmp)

tmp %>% 
  gather(variable, value, income) %>%
  spread(method, value) %>%
  arrange(variable, incgrp) %>%
  mutate_at(vars(-incgrp, -variable, -target), list(pdiff = ~ . / target * 100 - 100)) %>%
  kable(digits=c(1), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = TRUE)



#****************************************************************************************************
#               multinomial logit - use just what we would have ####
#****************************************************************************************************

f(constraints, c("wagp", "intp", "ssip"))

conret <- constraints %>%
  mutate(stabbr=as.character(stabbr), row=row_number()) %>%
  do(f(., c("income", "intp", "retp", "ssip", "ssp", "wagp")))

nn1 <- multinom(stabbr ~ income + wagp + intp + retp + ssip + ssp,
                data=conret,
                maxit=1000)
nn1
summary(nn1)
pvals_nn1 <- as_tibble(fitted(nn1)) %>% mutate(stabbr=conret$stabbr)
ht(pvals_nn1)

predict(nn1, type="probs", newdata=conretl) %>% round(3)
glimpse(sampdata)

nn1_probs <- predict(nn1, type="probs", newdata=sampdata) %>% as_tibble # sampdata is all states, just our ygroup
ht(nn1_probs)
nrow(nn1_probs)

nn1_wts <- nn1_probs %>%
  mutate_all(funs(. * sampdata$pwgtp))
ht(nn1_wts)
sum(nn1_wts$NY)
sampdata %>% filter(stabbr=="NY") %>% summarise(nysum=sum(pwgtp))

calib_nn1 <- calibrate_reweight(nn1_wts$NY, sampdata, consdata)
pdiff(weights(calib_nn1), sampdata, consdata)


conretl <- conret %>%
  right_join(expand_grid(alt=unique(conret$stabbr), row=conret$row)) %>%
  mutate(state=(stabbr==alt)) %>%
  arrange(row, alt)
frm <- frm <- formula(state ~ 1 | intp + retp + ssip + ssp + wagp | 1)
mnl1 <- mnlogit(frm, conretl, choiceVar="alt")
mnl1
summary(mnl1)
pvals_mnl <- as_tibble(mnl1$probabilities) %>% mutate(stabbr=conret$stabbr)
ht(pvals_mnl)


#****************************************************************************************************
#               construct distances to the summary records and construct weights based on that ####
#****************************************************************************************************
# turn constraints into average returns
# 10 per state
f <- function(df, vnames) {
  nvars <- paste0(vnames, "_n")
  # mutate(df, !!vnames := !!!vnames / !!!nvars)
  df[, vnames] <- df[, vnames] / df[, nvars]
  df[, nvars] <- NULL
  df %>% mutate_at(vars(vnames), ~replace(., is.na(.), 0))
}
f(constraints, c("income", "intp", "ssip"))

nvars <-  c("income", "intp", "retp", "ssip", "ssp", "wagp")
conret <- constraints %>%
  filter(incgroup==3) %>%
  mutate(stabbr=as.character(stabbr), row=row_number()) %>%
  do(f(., nvars)) %>%
  select(-pap_n, -pop)  %>% 
  rename_all(funs(paste0(., "_cons")))
conret

head(sampdata)
sampdists <- sampdata %>%
  select(stabbr, pwgtp, pop, one_of(nvars)) %>%
  mutate(row=row_number()) %>%
  crossing(conret) %>%
  arrange(row, row_cons)
ht(sampdists)

dfun <- function(df, dvars){
  x <- df[, dvars]
  y <-  df[, paste0(dvars, "_cons")]
  x2 <- df[, dvars] / y
  
  x2 <- ifelse(y==0, 0, df[, dvars] / y)
  scale(x, center=TRUE, scale=apply(x,2,mean))
  (df[, dvars] - df[, paste0(dvars, "_cons")])^2 %>% 
    rowSums %>% 
    sqrt
}

scale2 <- function(vars) {
  svals <- scale(vars)[, ]
  svals[is.nan(svals)] <- 0
  svals
}

d <- scale2(sampdists %>% filter(row<=4) %>% select(income, intp))
str(d)

dvars <- c("income", "intp")
dvars <- nvars
tmp <- sampdists %>%
  mutate_at(c(dvars, paste0(dvars, "_cons")), ~scale2(.)) %>%
  mutate(dist=dfun(., dvars)) %>%
  select(stabbr, row, stabbr_cons, row_cons, pwgtp, dvars, paste0(dvars, "_cons"), dist)
tmp %>%
  filter(row < 6)
quantile(tmp$dist)

probs_dist <- tmp %>%
  select(stabbr, row, pwgtp, stabbr_cons, dist) %>%
  group_by(row) %>%
  mutate(prob=dist / sum(dist)) %>%
  select(-dist) %>%
  spread(stabbr_cons, prob) %>%
  arrange(row)
probs_dist %>% ht  

# END: construct distances to the summary records and construct weights based on that ####





# make a long version
set.seed(1234)
slong1 <- samp_clean %>%
  mutate(stabbr=as.character(stabbr)) %>%
  group_by(stabbr) %>%
  ungroup %>%
  mutate(row=row_number())
slong2 <- slong1 %>%
  right_join(expand_grid(alt=unique(slong1$stabbr), row=slong1$row)) %>%
  mutate(state=(stabbr==alt),
         sex=factor(sex),
         mar=factor(mar)) %>% 
  arrange(row, alt)
glimpse(slong2)
head(slong2, 9)
count(slong2, state)
quantile(slong2$row)

constraints %>% 
  rename_at(vars(-stabbr, -incgroup), 
            list(~paste0(., "_cons")))
slong3 <- slong2 %>%
  left_join(constraints %>% 
              mutate_at(vars(-stabbr, -incgroup, -pop),
                        list(~. / pop)) %>%
              rename_at(vars(-stabbr, -incgroup), 
                        list(~paste0(., "_cons"))))
glimpse(slong3)

frm <- formula(state ~ 1 | intp + retp + ssip + ssp + wagp | 1) # GOOD intp_cons
frm <- formula(state ~ 1 | intp + retp + ssip + ssp + wagp -1 | retp_cons + intp_cons - 1) # NOT GOOD
# frm <- formula(state ~ 0 | intp + retp + ssip + ssp + wagp + retp_cons + intp_cons | 0)
frm <- formula(state ~ 0 | intp + retp + ssip + ssp + wagp | 0) # GOOD
frm <- formula(state ~ 0 | intp + retp + ssip + ssp + wagp + retp_cons | 0) # GOOD
frm <- formula(state ~ 0 | intp + retp + ssip + ssp + wagp + retp_cons + intp_cons - 1 | 0) # multicollinear even if -1
frm <- formula(state ~ 0 | intp + retp + ssip + ssp + wagp + retp_cons + intp_cons - 1 | 0) # multicollinear even if -1
system.time(wfit <- mnlogit(frm, slong3 %>% filter(incgroup==3),
                            choiceVar="alt", ncores=3))
summary(wfit)

pvals <- wfit$probabilities
ht(pvals)
pvals %>%
  as_tibble() %>%
  gather(stabbr, prob) %>%
  group_by(stabbr) %>%
  do(qtiledf(.$prob))
slong1 %>% group_by(stabbr) %>% summarise(n=n()) %>% mutate(n / sum(n))

pvals %>%
  as_tibble() %>%
  summarise_all(mean)
quantile(slong1$pwgtp)

# alternative nnet djb -- looking pretty good ----
glimpse(slong3)
count(slong3, pop)
nn1 <- multinom(stabbr ~ wagp + intp + retp + ssip + ssp + pap + wagp_cons,
                data=slong3 %>% filter(state==TRUE, incgroup==3) %>% mutate(wagp_cons=wagp_cons * (1+rnorm(100e3, 0, .01))),
                maxit=1000)
nn1
pvals <- fitted(nn1)
ht(pvals)
nrow(pvals)

# tmp <- slong3 %>% filter(state==TRUE)

#.. 2nd stage calibrate from mnl ----
targsum <- sampdata %>% filter(stabbr=="NY") %>% summarise(wt=sum(pwgtp)) %>% .[["wt"]]
nn1sum <- sum(nn1_probs[, "NY"] * sampdata$pwgtp)

calib_mnl <- calibrate_reweight(pvals[, "NY"],
                                sampdata, consdata) # FAST!! Also, the best result. Does it scale?
calib_nn1 <- calibrate_reweight(nn1_probs[, "NY"] * sampdata$pwgtp * sampdata$pwgtp * targsum / nn1sum,
                                sampdata, consdata)

calib_dist <- calibrate_reweight(probs_dist[, "NY"] * sampdata$pwgtp * targsum / sum(probs_dist[, "NY"] * sampdata$pwgtp),
                                sampdata, consdata)


sum(sampdata$pwgtp)
sum(nn1_probs[, "NY"] * sampdata$pwgtp)
sum(pvals[, "NY"] * sampdata$pwgtp)

tgtwts <- samp_clean %>%
  filter(incgroup==3) %>%
  mutate(wtarg=ifelse(stabbr=="NY", pwgtp, 0))

stack <- readRDS("d:/temp/comp_results_true.rds")
glimpse(stack)

stack_add <- tibble(row=1:nrow(pvals),
                    wmnl=pvals[, "NY"] * sampdata$pwgtp,
                    wcalib_mnl=weights(calib_mnl),
                    wcalib_nn1=weights(calib_nn1),
                    wcalib_dist=weights(calib_dist),
                    wtarg=tgtwts$wtarg) %>%
  gather(method, weight, -row) %>%
  group_by(method) %>%
  mutate(obj=objfn(weight, sampdata, consdata),
         iter=0, seconds=0)
stack <- bind_rows(stack, stack_add)
glimpse(stack)
count(stack, method)

stack %>%
  group_by(method) %>%
  summarise_at(vars(obj, iter, seconds), ~ first(.))

stack %>%
  group_by(method) %>%
  summarise(zero_pct=sum(weight==0) / n() * 100)

stack %>%
  group_by(method) %>%
  summarise(near_zero_pct=sum(round(weight, 3)==0.000) / n() * 100)
samp_clean %>%
  filter(incgroup==3) %>%
  group_by(stabbr) %>%
  summarise(wt=sum(pwgtp)) %>%
  mutate(wtpct=wt / sum(wt) * 100)
glimpse(samp_clean)



stack %>%
  select(row, method, weight) %>%
  spread(method, weight) %>%
  mutate(wts0=wts0) %>%
  ungroup %>%
  select(-row) %>%
  cor()

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(calc_constraints(.$weight, sampdata, names(consdata)) %>%
       t %>% 
       as.data.frame) %>%
  bind_rows(consdata %>% mutate(method="target", obj=0, iter=0, seconds=0,
                                method=factor(method, levels=method_levs))) %>%
  ungroup %>%
  arrange(method) %>%
  kable(digits=c(1, 5, 0, 0, rep(1, ncol(.) - 4)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(pdiff(.$weight, sampdata, consdata)) %>%
  kable(digits=c(1, 5, 0, 0, rep(1, ncol(.) - 4)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(qtiledf(.$weight, probs=probs)) %>%
  kable(digits=c(1, 5, rep(0, 4), rep(1, ncol(.) - 6)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  ggplot(aes(weight)) +
  geom_histogram(fill="blue", bins=50) +
  geom_vline(xintercept = mean(wts0)) +
  scale_x_continuous(breaks=seq(0, 500, 5), limits=c(0, 80)) +
  facet_wrap(~method, ncol=2)

tmp <- stack %>%
  left_join(sampdata %>% select(income) %>% mutate(row=row_number())) %>%
  bind_rows(samp_clean %>% 
              filter(stabbr=="NY", incgroup==ygrp) %>% 
              select(weight=pwgtp, income) %>%
              mutate(method="target", method=factor(method, levels=method_levs))) %>%
  mutate(tax=tax(income),
         incgrp=cut(income, 5)) %>%
  group_by(method, incgrp) %>%
  summarise(income=sum(income * weight), tax=sum(tax * weight)) 
# glimpse(tmp)

tmp %>% 
  gather(variable, value, income, tax) %>%
  spread(method, value) %>%
  arrange(variable, incgrp) %>%
  mutate_at(vars(-incgrp, -variable, -target), list(pdiff = ~ . / target * 100 - 100)) %>%
  kable(digits=c(rep(0, 10), rep(1, 7)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = TRUE)



# create a small tax model and see how we do
tax <- function(income){
  rate <- .01 + min((income / 50e3)^2 / 100, .5)
  income * rate
}
tax(0)
tax(10e3)
tax(1000e3)
glimpse(sampdata)
glimpse(stack)


#..end 2nd stage ----



# frm <- formula(state ~ income + intp + retp + ssip + ssp + wagp) # NO GOOD
frm2 <- formula(state ~ 1 | income + intp + retp + ssip + ssp + wagp | 1) # GOOD
frm2 <- formula(state ~ 1 | intp + retp + ssip + ssp + wagp + mar + sex + wagp * sex + wagp*mar | 1)
# frm3 <- formula(state ~ 1 | 1 | income + intp + retp + ssip + ssp + wagp) # NO GOOD!!
#  + intp_n + pap_n + wagp_n
system.time(wfit <- mnlogit(frm2, slong2, choiceVar="alt", ncores=3))


data("Fish", package = 'mnlogit')
glimpse(Fish)
head(Fish, 8)
count(Fish, alt)
fm <- formula(mode ~ price | income | catch)
system.time(fit <- mnlogit(fm, Fish, ncores=2))
mnlogit(fm, Fish, "alt") # choiceVar is alt
class(fit)
print(fit, what = "eststat")
print(fit, what = "modsize")
?lrtest

f2 <- as.data.frame(Fish)
glimpse(f2)
count(f2, alt, mode) %>% spread(mode, n)
mnlogit(fm, f2, "alt") 
fm2 <- formula(mode ~ income) # bad
mnlogit(fm2, f2, "alt") 

# THIS (below) IS CORRECT (why does it do this?)
fm3 <- formula(mode ~ 1 | income | 1) # separate intercepts and slopes on income for each of 3 alternatives
fm3 <- formula(mode ~ 0 | income | 1) # separate slopes on 3 alternatives
fm3 <- formula(mode ~ 1 | income | 0) # separate slopes on 3 alternatives
fm3 <- formula(mode ~ 0 | income | 0) # separate slopes on 3 alternatives
fm3 <- formula(mode ~ 1 + price | income | 1) # separate slopes 3, intercepts 3, + price slope alternatives


fm3 <- formula(mode ~ income | 1 | 1) # error singular
fm3 <- formula(mode ~ income | 0 | 1) # error singular
fm3 <- formula(mode ~ income | 1 | 0) # error singular
fm3 <- formula(mode ~ income | 0 | 0) # error singular

fm3 <- formula(mode ~ 1 | 1 | income) # separate: 3 intercepts, 4 slopes but slope SEs are NA
fm3 <- formula(mode ~ 1 | 1 | income - 1) # bad
fm3 <- formula(mode ~ 0 | 1 | income - 1) # separate: 3 intercepts, 4 slopes but slope SEs are NA
fm3 <- formula(mode ~ 0 | 1 | income) # separate slopes on 4 alternatives
fm3 <- formula(mode ~ 1 | 0 | income) # separate slopes on 4 alternatives
fm3 <- formula(mode ~ 0 | 0 | income) # separate slopes on 4 alternatives

tst <- mnlogit(fm3, f2, "alt") 
summary(tst)

library(nnet)
mnnet <- multinom(mode ~ income, data=f2) # slope and intercept overall
mnnet

test <- multinom(mode ~ income + I(income^2), data=f2)
test
head(pp <- fitted(test))
head(pp, 20)

mnnet0 <- multinom(alt ~ income, data=f2 %>% filter(mode==TRUE)) # 3 slopes 3 intercepts
mnnet0
summary(mnnet0)
str(mnnet0)
test <- mnnet0
test
pp <- fitted(test)
head(pp, 20)


mnnet1 <- multinom(alt ~ income, data=f2) # huh?
mnnet1
multinom(alt ~ income, data=f2) 

mnnet2 <- multinom(alt ~ income, data=f2 %>% filter(mode==TRUE)) # intercept for each alt
mnnet2

mnnet3 <- multinom(alt ~ income - 1, data=f2 %>% filter(mode==TRUE)) # slope for each alt
mnnet3

str(mnnet)



fm4 <- formula(mode ~ 1 | income) # same model, riskier specification
mnlogit(fm4, f2, "alt") 

fm4 <- formula(mode ~ 1 | 1 | income)
mnlogit(fm3, f2, "alt") 


library(foreign)
library(nnet)
library(stargazer)
mydata <- read.dta("http://www.ats.ucla.edu/stat/data/hsb2.dta")





# now my data
glimpse(sampdata)
ns(sampdata)
ns(persons)
glimpse(samp_clean)



# make a long version
set.seed(1234)
slong1 <- samp_clean %>%
  mutate(stabbr=as.character(stabbr)) %>%
  group_by(stabbr) %>%
  sample_frac(.05) %>%
  ungroup %>%
  mutate(row=row_number())
slong2 <- slong1 %>%
  right_join(expand_grid(alt=unique(slong1$stabbr), row=slong1$row)) %>%
  mutate(state=(stabbr==alt),
         sex=factor(sex),
         mar=factor(mar)) %>% 
  arrange(row, alt)
glimpse(slong2)
head(slong2, 9)
count(slong2, state)
quantile(slong2$row)

# frm <- formula(state ~ income + intp + retp + ssip + ssp + wagp) # NO GOOD
frm2 <- formula(state ~ 1 | income + intp + retp + ssip + ssp + wagp | 1) # GOOD
frm2 <- formula(state ~ 1 | intp + retp + ssip + ssp + wagp + mar + sex + wagp * sex + wagp*mar | 1)
# frm3 <- formula(state ~ 1 | 1 | income + intp + retp + ssip + ssp + wagp) # NO GOOD!!
#  + intp_n + pap_n + wagp_n
system.time(wfit <- mnlogit(frm2, slong2, choiceVar="alt", ncores=3))
# print(wfit, what = "eststat")
# print(wfit, what = "modsize")
summary(wfit)
# print(wfit)
# str(wfit)

# fvals <- fitted(wfit, outcome=TRUE) # 1 per observation in the source (pre expansion) data
# quantile(fvals)
# vcov(wfit)
# logLik(wfit)
pvals <- wfit$probabilities
ht(pvals)
pvals %>%
  as_tibble() %>%
  gather(stabbr, prob) %>%
  group_by(stabbr) %>%
  do(qtiledf(.$prob))
slong1 %>% group_by(stabbr) %>% summarise(n=n()) %>% mutate(n / sum(n))

pvals %>%
  as_tibble() %>%
  summarise_all(mean)
quantile(slong1$pwgtp)

#****************************************************************************************************
#               OTA Fisher and Lin approach ####
#****************************************************************************************************




#****************************************************************************************************
#               compare results ####
#****************************************************************************************************
imax <- 1000
sa_wfs <- uni_opt(method="simann", wts0, sampdata, consdata, objfn=objfn, maxiter=10)
mma_rwt <- uni_opt(method="mma", wts0, sampdata, consdata, objfn=objfn, maxiter=10)
calib_rwt <- uni_opt(method="calibrate", wts0, sampdata, consdata, objfn=objfn, maxiter=10)
str(sa_wfs)
tmp <- list(sa_wfs, mma_rwt, calib_rwt)
str(tmp)
sapply(1:3, function(i) tmp[[i]]$method)
# obj.vals <- laply(1:n, function(i) optlist[[i]]$result$objective)

ldf <- function(method, wts0, data, constraints, objfn, maxiter){
  list_result <-  uni_opt(method=method, wts0=wts0, data=data, constraints=constraints, objfn=objfn, maxiter=maxiter)
  df_result <- tibble(row=1:length(wts0), method=method, obj=list_result$obj, iter=list_result$iter,
                      seconds=list_result$elapsed_seconds, weight=list_result$weights)
  return(df_result)
}
# ldf("simann", wts0, sampdata, consdata, objfn, 10)

# tmp <- llply(c("simann", "mma", "calibrate"), uni_opt, wts0, sampdata, consdata, maxiter=10)
# str(tmp)

system.time(comp_results <- ldply(c("simann", "mma", "calibrate"), ldf, wts0, sampdata, consdata, objfn, maxiter=1000))
glimpse(comp_results)
wts0df <- tibble(row=1:length(wts0), method="wts0", obj=objfn(wts0, sampdata, consdata), iter=0, seconds=0, weight=wts0)
stack <- bind_rows(wts0df, comp_results)
method_levs <- c("target", "wts0", "simann", "mma", "calibrate")
stack <- stack %>%
  mutate(method=factor(method, levels=method_levs))
ht(stack)
count(stack, method)
# saveRDS(stack, "d:/temp/comp_results_true.rds")
# saveRDS(stack, "d:/temp/comp_results_perturbed2pct.rds")

# obj value, iterations, time
stack %>%
  group_by(method) %>%
  summarise_at(vars(obj, iter, seconds), ~ first(.))

stack %>%
  group_by(method) %>%
  summarise(zero_pct=sum(weight==0) / n() * 100)

stack %>%
  select(row, method, weight) %>%
  spread(method, weight) %>%
  mutate(wts0=wts0) %>%
  ungroup %>%
  select(-row) %>%
  cor()

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(calc_constraints(.$weight, sampdata, names(consdata)) %>%
       t %>% 
       as.data.frame) %>%
  bind_rows(consdata %>% mutate(method="target", obj=0, iter=0, seconds=0,
                                method=factor(method, levels=method_levs))) %>%
  ungroup %>%
  arrange(method) %>%
  kable(digits=c(1, 5, 0, 0, rep(1, ncol(.) - 4)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(pdiff(.$weight, sampdata, consdata)) %>%
  kable(digits=c(1, 5, 0, 0, rep(1, ncol(.) - 4)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  group_by(method, obj, iter, seconds) %>%
  do(qtiledf(.$weight, probs=probs)) %>%
  kable(digits=c(1, 5, rep(0, 4), rep(1, ncol(.) - 6)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = FALSE)

stack %>%
  ggplot(aes(weight)) +
  geom_histogram(fill="blue", bins=50) +
  geom_vline(xintercept = mean(wts0)) +
  scale_x_continuous(breaks=seq(0, 500, 5), limits=c(0, 80)) +
  facet_wrap(~method, ncol=1)

# create a small tax model and see how we do
tax <- function(income){
  rate <- .01 + min((income / 50e3)^2 / 100, .5)
  income * rate
}
tax(0)
tax(10e3)
tax(1000e3)
glimpse(sampdata)
glimpse(stack)
tmp <- stack %>%
  left_join(sampdata %>% select(income) %>% mutate(row=row_number())) %>%
  bind_rows(samp_clean %>% 
              filter(stabbr=="NY", incgroup==ygrp) %>% 
              select(weight=pwgtp, income) %>%
              mutate(method="target", method=factor(method, levels=method_levs))) %>%
  mutate(tax=tax(income),
         incgrp=cut(income, 5)) %>%
  group_by(method, incgrp) %>%
  summarise(income=sum(income * weight), tax=sum(tax * weight)) 
glimpse(tmp)

tmp %>% 
  gather(variable, value, income, tax) %>%
  spread(method, value) %>%
  arrange(variable, incgrp) %>%
  mutate_at(vars(-incgrp, -variable, -target), list(pdiff = ~ . / target * 100 - 100)) %>%
  kable(digits=c(rep(0, 7), rep(1, 4)), format.args = list(big.mark=",")) %>%
  kable_styling(full_width = TRUE)


#****************************************************************************************************
#               NOT READY BELOW HERE ####
#****************************************************************************************************

#****************************************************************************************************
#               iterative proportional fitting ####
#****************************************************************************************************
library(mipfp)
??`mipfp-package`

# generation of an intial 2-ways table to be updated
(seed <- array(1, dim=c(2, 2)))

# desired targets (margins)
target.row <- c(87, 13)
target.col <- c(52, 48)

# storing the margins in a list
target.data <- list(target.col, target.row)
target.data

# list of dimensions of each marginal constrain
target.list <- list(1, 2)

# calling the fitting methods
(r.ipfp <- Ipfp(seed, target.list, target.data))
str(r.ipfp)

(r.ml <- ObtainModelEstimates(seed, target.list, target.data, method = "ml"))
str(r.ml)

(r.chi2 <- ObtainModelEstimates(seed, target.list, target.data, method = "chi2"))
str(r.chi2)

(r.lsq <- ObtainModelEstimates(seed, target.list, target.data, method = "lsq"))
str(r.lsq)

# loading the data
data(spnamur, package = "mipfp")
glimpse(spnamur)

# subsetting the data frame, keeping only the first 3 variables
spnamur.sub <- subset(spnamur, select = Household.type:Prof.status)
glimpse(spnamur)
spnamur # true table
count(spnamur, Gender)

(true.table <- table(spnamur.sub))


# extracting the margins
tgt.v1        <- apply(true.table, 1, sum)
tgt.v1.v2     <- apply(true.table, c(1,2), sum)
tgt.v2.v3     <- apply(true.table, c(2,3), sum)
tgt.list.dims <- list(1, c(1,2), c(2,3))
tgt.data      <- list(tgt.v1, tgt.v1.v2, tgt.v2.v3)
sum(tgt.data[[1]])
sum(tgt.data[[2]])
sum(tgt.data[[3]])

# creating the seed, a 10 pct sample of spnamur
seed.df <- spnamur.sub[sample(nrow(spnamur), round(0.10*nrow(spnamur))), ]
seed.table <- table(seed.df)
# applying one fitting method (ipfp)
r.ipfp <- Estimate(seed=seed.table, target.list=tgt.list.dims, 
                   target.data = tgt.data)
print(r.ipfp)
str(r.ipfp)

r.ml <- Estimate(seed=seed.table, target.list=tgt.list.dims, 
                   target.data = tgt.data, method="ml")
str(r.ml)

r.chi2 <- Estimate(seed=seed.table, target.list=tgt.list.dims, 
                 target.data = tgt.data, method="chi2")
str(r.chi2)

r.lsq <- Estimate(seed=seed.table, target.list=tgt.list.dims, 
                   target.data = tgt.data, method="lsq")
str(r.lsq)

#****************************************************************************************************
#               raking and calibration from survey package ####
#****************************************************************************************************
# https://www.r-bloggers.com/survey-raking-an-illustration/
# GOOD: https://cvxr.rbind.io/cvxr_examples/cvxr_survey_calibration/
# Primary Sampling Units (PSU)
# https://stats.stackexchange.com/questions/83089/sample-weighting-with-continuous-variables
# https://faculty.washington.edu/tlumley/survey-jsm-nup.pdf p.129

library("survey")

# here, we penalize deviations from initial weights; the initial weights must be pretty good
calibrate_reweight <- function(weights, data, constraints){
  # svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
  # id = ~0 means no clusters
  sdo <- svydesign(id = ~0, weights = weights, data = data)
  
  pop_totals <- constraints %>% 
    gather(vname, value) %>%
    deframe
  
  pop_totals <- pop_totals[-1]
  
  frm_string <- paste0("~ ", paste(names(pop_totals), collapse=" + "), " - 1")
  frm <- as.formula(frm_string)
  
  calibrated <- calibrate(sdo, formula=frm, population=pop_totals)
  
  return(calibrated)
}

calib_rw <- calibrate_reweight(wts0, sampdata, consdata) # FAST!!
weights <- wts0; data <- sampdata; constraints <- consdata
str(sdo)

sum(wts0)
sum(weights(calib_rw))
tibble(w=weights(calib_rw)) %>%
  ggplot(aes(w)) +
  geom_histogram(fill="blue") +
  geom_vline(xintercept = mean(wts0))

pdiff(wts0, sampdata, consdata) %>% round(2)
pdiff(sa_none$best_weights, sampdata, consdata) %>% round(2)
pdiff(sa_recs$best_weights, sampdata, consdata) %>% round(2)
pdiff(sa_recs_wts$best_weights, sampdata, consdata) %>% round(2)
pdiff(mma_result$par, sampdata, consdata) %>% round(2)
pdiff(weights(calib_rw), sampdata, consdata) %>% round(2)


# working version below
??survey
data(api)
glimpse(apiclus1)
names(apiclus1) %>% sort
sum(apiclus1$pw)

count(apiclus1, fpc) # 183 obs from pop of 757
count(apiclus1, cname)

# dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)
# dclus1 <- svydesign(id = ~0, weights = ~pw, data = apiclus1)
dclus1 <- svydesign(id = ~0, weights = apiclus1$pw, data = apiclus1)
dclus1 <- f(apiclus1, wtvec=apiclus1$pw)

f(weights=apiclus1$pw, data=apiclus1, constraints_calibrate = consdata_cal)

str(dclus1)
summary(dclus1)

frm <- ~api99 + api00 + enroll
cal_names(frm, dclus1) # calibration names
consdata
consdata_cal <- as_tibble(t(pop.totals))
pop.totals <- c(`(Intercept)`=6194, api99=3914069, api00=3989985 * 1.05, enroll=3404940) # named list or vector

frm <- ~api99 + api00 + enroll - 1 # not intercept
cal_names(frm, dclus1) # calibration names
consdata
pop.totals <- c(api99=3914069, api00=3989985 * 1.05, enroll=3404940) # named list or vector
consdata_cal <- as_tibble(t(pop.totals))

(djb <- calibrate(dclus1, formula=frm, population=pop.totals))
djb <- f(weights=apiclus1$pw, data=apiclus1, constraints_calibrate = consdata_cal)

sum(apiclus1$pw)
sum(weights(djb))
tibble(w=weights(djb)) %>%
  ggplot(aes(w)) +
  geom_histogram(fill="blue") +
  geom_vline(xintercept = mean(apiclus1$pw))



svymean(~api00, dclus1)
svyquantile(~api00, dclus1, quantile=c(0.25,0.5,0.75), ci=TRUE)
svytotal(~stype, dclus1)
svytotal(~enroll, dclus1)
apiclus1 %>% summarise(enroll=sum(enroll * pw))

# proportion of HS students who took the test
svyratio(~api.stu, ~enroll, design=subset(dclus1, stype=="H"))

# vars <- names(apiclus1)[c(12:13,16:23,27:37)]
# svymean(make.formula(vars), dclus1, na.rm=TRUE)

#.. djb ----
gclus1 <- calibrate(dclus1, formula=~api99, population=c(6194, 3914069))
names(gclus1)
str(gclus1)
weights(gclus1)
sum(apiclus1$pw)
sum(weights(gclus1))

gclus1$postStrata[[1]]$w
tibble(pw=apiclus1$pw, w=weights(gclus1)) %>%
  ggplot(aes(pw, w)) + geom_point()

tibble(w=weights(gclus1)) %>%
  ggplot(aes(w)) +
  geom_histogram(fill="blue")

pop.totals<-c(`(Intercept)`=6194, stypeH=755, stypeM=1018)

cal_names(~stype, dclus1)

# djb ----
glimpse(apiclus1)
sum(apiclus1$api00 * apiclus1$pw)
frm <- ~api99 + api00 + enroll + stype # use all levels of stype except first (??)
cal_names(frm, dclus1)
pop.totals<-c(`(Intercept)`=6194, api99=3914069, api00=3989985 * 1.05, enroll=3404940, stypeH=755, stypeM=1018) # named list or vector
(dclus1g3 <- calibrate(dclus1, formula=frm, population=pop.totals))
str(dclus1g3)
weights(dclus1g3)

svymean(~api00, dclus1g3)
svytotal(~api00, dclus1g3)
svytotal(~enroll, dclus1g3)
svytotal(~stype, dclus1g3)

tibble(w=weights(dclus1g3)) %>%
  ggplot(aes(w)) +
  geom_histogram(fill="blue") +
  geom_vline(xintercept = 33.8)




svymean(~api00, dclus1)
svymean(~api00, gclus1)

svytotal(~api99, dclus1)
svytotal(~api99, gclus1)

svytotal(~enroll, dclus1)
svytotal(~enroll, gclus1)



#****************************************************************************************************
#               older stuff ####
#****************************************************************************************************




eval_grad_f_wtfs <- function(w, cons, samp){
  # gradient of objective function - a vector length w
  # giving the partial derivatives of obj wrt each w[i]
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs will include:
  #   for each target element in the objective function:
  #     the sum -- s1 for first target element, and so on, and
  #     the multiplier vector (e.g., wages, cap gains, etc.)
  
  # http://www.derivative-calculator.net/
  # https://www.symbolab.com/solver/partial-derivative-calculator
  # Example where we only have one target element in the objective function, and only have
  #   3 records and therefore 3 weights, and need to return a vector of 3 partial derivatives
  # Notation: w1, w2, and w3 are the first 3 weights in the vector of weights
  #           a1, a2, and a3 are the 3 constants they are multiplied by (e.g., wages1, wages2, wages3)
  #           t is the sum or target we are comparing to
  #           p is the priority weight of this element of the objective function
  #           calc is the calculated value of the target with the new weights  
  # The objective function to minimize is p*(w1*a1 + w2*a2 + w3*a3 - s)^2
  # The first partial derivatives of the objective function are:
  #   wrt w1:          2*a1*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a1*p*(calc - t)
  #   wrt w2:          2*a2*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a2*p*(calc - t)
  #   wrt w3:          2*a3*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a3*p*(calc - t)
  # If we have multiple target elements, each vector element will have a more complex sum
  
  # w <- wts0; samp <- sampdata; cons <- consdata
  wdf <- tibble(wt.iter=w, wtnum=1:length(w)) # weights data frame
  
  slong <- samp %>%
    select(names(cons)) %>%
    mutate(wtnum=row_number()) %>%
    gather(obj.element, coeff, -wtnum) %>%
    left_join(cons %>% gather(obj.element, scale) %>% mutate(target=scale), by="obj.element")
  # glimpse(slong)
  
  grad <- slong %>% 
    left_join(wdf, by="wtnum") %>%
    group_by(obj.element) %>%
    mutate(calc=sum(wt.iter*coeff)) %>%
    mutate(grad={2 * coeff * (calc - target)} / {scale^2}) %>%
    group_by(wtnum) %>%
    summarise(grad=sum(grad)) %>% # sum across all of the object elements for this particular weight
    ungroup
  # glimpse(grad)
  
  return(grad$grad)
}

objfn

glimpse(sampdata)
ht(sampdata)
ht(consdata)


pval <- 2
objfn(wts0, consdata, sampdata, p=pval)
objfn(tmp1$best_weights, consdata, sampdata, p=pval)
objfn(tmp2$best_weights, consdata, sampdata, p=pval)
objfn(tmp3$best_weights, consdata, sampdata, p=pval)


inputs <- getinplist(syn, recipe.use)

# bounds on the weights
xlb <- rep(1, nrow(syn))
xub <- rep(1.5*max(puf$wt), nrow(syn)) # FIX THIS djb

# starting point:
x0 <- (xlb + xub) / 2
x0 <- x0 * sum(puf$wt / sum(x0))

opts <- list("algorithm"="NLOPT_LD_MMA",
             "xtol_rel"=1.0e-8,
             "maxeval"=500)
result <- nloptr(x0, 
                 eval_f=eval_f_wtfs,
                 eval_grad_f = eval_grad_f_wtfs,
                 lb = xlb, ub = xub,
                 opts = opts, inputs=inputs)


df <- sampdata %>%
  mutate(wt=tmp2$best_weights)
glimpse(df) 
df %>%
  group_by(stabbr) %>%
  summarise(wt_min=min(wt), wt_mdn=median(wt), wt_max=max(wt), wt_sum=sum(wt))
  

library(nloptr)
a <- proc.time()
t2 <- mma(x0, 
          fn=eval_f_wtfs, 
          gr = eval_grad_f_wtfs,
          lower=xlb, upper=xub,
          nl.info = FALSE, 
          control=list(maxeval=500),
          inputs=inputs)
b <- proc.time()
b - a # 16 mins

# t2$par
t2$value
t2$iter
t2$convergence
t2$message


opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=1000,
             # "derivative_test"="first-order",
             # "derivative_test_print_all"="yes",
             "output_file" = "scratch3.out")

a <- proc.time()
result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_wtfs, 
                 eval_grad_f = eval_grad_f_wtfs,
                 opts = opts,
                 inputs = inputs)
b <- proc.time()
b - a



# nloptr.print.options()
# control = list(xtol_rel = 1e-8)
nloptr.print.options(opts.show = c("algorithm"))

# iter: obj, secs -- with x0=mean
# NLOPT_LD_MMA 20: 4.7, 8 secs; 50: 0.5, 18 secs; 100: 0.07, 36 secs; 200: 0.006, 67 secs; 500: 0.00004, 172 secs
# MMA 50 iter with runif and seed: 1 0.39, 2 0.38
# MMA 100 iter with runif and seed: 1 0.06, 2 0.08, 3 .067
# MMA 200 iter with runif and seed: 1 0.0036, 2 , 3 0.005
# obj 0.0036 looks like it gets apdiff close enough for priority variables

# NLOPT_LD_LBFGS 20: 121, 8; 50: 47, 18 secs; 100, 18.9, 36 secs
# NLOPT_LD_VAR1 20: 136, 8 secs; 100 21, 37 secs
# NLOPT_LD_VAR2 20: 136, 7 secs
# NLOPT_LD_TNEWTON 20: 284, 8 secs
# NLOPT_LD_TNEWTON_PRECOND_RESTART 20: 256, 7; 100: 96, 35
# NLOPT_LD_AUGLAG:
#   NLOPT_LD_MMA 20, 4.7, 8
#   NLOPT_LN_BOBYQA 20, 737, 11 secs
#   NLOPT_LN_COBYLA
#   DO NOT DO THIS NLOPT_LD_SLSQP

# no derivative
# NLOPT_LN_BOBYQA # 20: 737, 11 secs
# NLOPT_LN_COBYLA 20: 737 9 secs

# bad
# DO NOT DO THIS NLOPT_LD_SLSQP # RStudio BLOWS UP
# NLOPT_LD_LBFGS_NOCEDAL DOESN'T WORK

# uncomment this for auglag, but don't think it's needed
# local_opts <- list("algorithm" = "NLOPT_LN_COBYLA", "xtol_rel"  = 1.0e-8) # 
# opts <- list("algorithm" = "NLOPT_LD_AUGLAG",
#              "xtol_rel"  = 1.0e-8,
#              "maxeval"   = 20,
#              "local_opts" = local_opts)




#****************************************************************************************************
#               BAD: optim -- DOES NOT WORK ####
#****************************************************************************************************
# what about optim? No.
# method = c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"),
# bounds can only be used with method L-BFGS-B (or Brent)
# cannot use SANN - does not allow bounds -- 1k iter 14 secs; at least 10% of values are negative!
# L-BFGS-B didn't even complete 1 iteration (without a gradient function)
# Brent only available for 1 dimensional optimization so it does not work
set.seed(seed); system.time(tmp4 <- optim(par=wts0, fn=objfn, gr = NULL, cons=consdata, samp=sampdata,
                                          method = "Brent",
                                          lower = rep(0, length(wts0)), upper = Inf,
                                          control = list(maxit=1), hessian = FALSE))
str(tmp4)
quantile(tmp4$par, probs=c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))
