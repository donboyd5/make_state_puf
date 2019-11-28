
# D:\Dropbox\RPrograms PC\OSPC\make_state_puf\data

# Steps:
# - weight file to come close to 2011 targets
# - use SOI per-return growth rates to set prelim file for 2016
# - reweight prelim 2016 file to come close to 2016 SOI targets
# 

# Issues
# - residents and nonresidents
# - liability from collections vs. reported

# SOI datasets
# https://www.irs.gov/statistics/soi-tax-stats-historic-table-2

# NY datasets
# - https://www.tax.ny.gov/research/stats/statistics/pit-filers-summary-datasets-through-tax-year-2016.htm
# - https://data.ny.gov/Government-Finance/Total-Income-And-Tax-Liability-By-Place-Of-Residen/nacg-rg66
# - https://data.ny.gov/Government-Finance/Income-Tax-Components-by-Size-of-Income-by-Place-o/5bb2-yb85
# - https://data.ny.gov/Government-Finance/Earned-Income-Tax-Credit-EITC-Claims-by-Credit-Typ/6q7b-8vuf
# - https://data.ny.gov/Government-Finance/Total-Income-and-Tax-Liability-of-Part-Year-Reside/gsty-kw6t
# - https://data.ny.gov/Government-Finance/Average-Income-and-Tax-Liability-of-Full-Year-Resi/2w9v-ejxd
# - https://data.ny.gov/Government-Finance/Total-Income-and-Tax-Liability-of-Full-Year-Reside/ren5-txva
# - https://data.ny.gov/Government-Finance/Income-Tax-Components-of-Full-Year-Residents-by-Si/etik-ck6b
# - https://data.ny.gov/Government-Finance/Income-Tax-Components-of-Full-Year-Residents-by-Si/5kgr-h5g5
# - https://data.ny.gov/Government-Finance/Income-Tax-Components-of-Full-Year-Nonresidents-by/4qj3-ti3n
# - https://data.ny.gov/Government-Finance/Income-Tax-Components-of-Full-Year-Nonresidents-by/6ez4-x9ke
# - https://data.ny.gov/Government-Finance/Income-Tax-Components-of-Part-Year-Residents-by-Si/psb3-5dxg
# - 
# - 


#****************************************************************************************************
#                Libraries and globals ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")

source("./r/includes/functions_general.r")
source("./r/includes/functions_target_setup_and_analysis.r")
source("./r/includes/functions_ipopt.r")

source("./r/includes/functions_state_puf.r")


#****************************************************************************************************
#                get puf data ####
#****************************************************************************************************
puf <- readRDS(paste0(globals$tc.dir, "puf_lc.rds")) # note that this has all records, and has wt variable
glimpse(puf)

puf.vnames <- get_puf_vnames()


#****************************************************************************************************
#                Setup: get inclink (for linking to Historical Table 2) and potential targets ####
#****************************************************************************************************

(inclink <- get_income_ranges(2011))

hist2_targets <- get_hist2_targets(2011, "NY")
glimpse(hist2_targets)
hist2_targets  %>% ht


#****************************************************************************************************
#                Prepare initial NY weights for the puf by ratio adjustment ####
#****************************************************************************************************
# pufny <- puf %>%
#   mutate(agi_group=cut(e00100, agibrks, right=FALSE))
# count(pufny, agi_group)

# determine weight factors by income range
# first, get puf weights by income range and marital status
pufwts <- puf %>%
  mutate(agi_group=get_agi_group(e00100, 2011)) %>%
  group_by(agi_group) %>%
  summarise(n=n(), nret=sum(wt), nret_joint=sum(wt * (MARS==2))) %>%
  mutate(nret_other=nret - nret_joint, incgrp=paste0("inc", as.numeric(agi_group)))
pufwts

# next, get same values for the soi data
targwts <- hist2_targets %>%
  filter(lineno %in% 1:2, incgrp!="inc0") %>%
  mutate(vname=ifelse(lineno==1, "nret", "nret_joint")) %>%
  select(incgrp, vname, target) %>%
  spread(vname, target) %>%
  mutate(nret_other=nret - nret_joint)
targwts  

wtfactors <- bind_rows(pufwts %>% select(incgrp, nret_joint, nret_other) %>% mutate(ftype="puf"),
                       targwts %>% select(incgrp, nret_joint, nret_other) %>% mutate(ftype="target")) %>%
  gather(rtype, value, -incgrp, -ftype) %>%
  spread(ftype, value) %>%
  mutate(wtfactor=target / puf)
wtfactors

pufny1 <- puf %>%
  mutate(agi_group=get_agi_group(e00100, 2011),
         incgrp=get_incgrp(agi_group),
         rtype=ifelse(MARS==2, "nret_joint", "nret_other")) %>%
  left_join(wtfactors %>% select(incgrp, rtype, wtfactor),
            by = c("incgrp", "rtype")) %>%
  mutate(wt_puf=wt,
         wtny_ratio=wt_puf * wtfactor,
         wt=wtny_ratio)
# by design, pufny1 has the same weighted number of returns by income group and return type as Historical Table 2


#****************************************************************************************************
#                Create targets ####
#****************************************************************************************************
hist2_targets

# get a list of potential targets to make it easy to drop or what to keep
target_list <- hist2_targets %>%
  filter(incgrp!="inc0", !str_detect(h2vname, "_n")) %>%
  filter(!h2vname %in% c("totinc", "XDEP")) %>%
  group_by(h2vname, table_desc) %>%
  summarise(target=sum(target))
target_list  

# djb ----
# droptargs <- c("totinc", "totinc_n")
# target_list$h2vname[11] "e02300" we are good up to here
# target_list$h2vname[12] e02500 once here we get slacks too small message -- Social Security benefits in AGI
# target_list$h2vname[20] e07150
# target_list$h2vname[21] e07180

keeptargs <- target_list$h2vname[1:30] # 20, iter 134; best 21, iter 134
keeptargs <- c(keeptargs, paste0(keeptargs, "_n")) %>% sort

target.rules <- hist2_targets %>%
  filter(incgrp!="inc0") %>%
  filter(!h2vname %in% c("totinc", "totinc_n", "XDEP")) %>%
  filter(h2vname %in% keeptargs) %>%
  mutate(vname=str_remove(h2vname, "_n")) %>%
  select(vname, h2vname, incgrp, target, select.rule, calc.rule) %>%
  mutate(ftype="target", constraint.num=row_number()) %>%
  left_join(inclink, by = "incgrp") %>%
  mutate(target.num=row_number()) %>%
  left_join(puf.vnames %>% select(vname, vdesc), by = "vname")
# glimpse(target.rules)
# ht(target.rules)
# count(target.rules, vname)
# count(target.rules, h2vname)


#****************************************************************************************************
#                Collapse initial ny puf and compare to targets ####
#****************************************************************************************************
# comp_df <- check_targets(target.rules, pufny1)
# comp_df %>%
#   arrange(desc(abs(pdiff)))

#****************************************************************************************************
#                Reweight initial weighted ny puf and compare to targets ####
#****************************************************************************************************
# e18400 = SALT is hard to hit
system.time(cc.sparse <- get_constraint_coefficients_sparse(pufny1, target.rules))
glimpse(cc.sparse)
names(cc.sparse)

# CRITICAL - DECIDE SCALING HERE !!!!----
# cnum indexes the feasible targets -- target.num is an identifier the actual target number of
scale_factors <- cc.sparse$nzcc %>%
  group_by(i, cnum) %>%
  summarise(nzcc.mdn=median(nzcc)) %>%
  ungroup %>%
  # comment one of these next 2 lines out
  # mutate(scale=50 / nzcc.mdn)
  mutate(scale=1) # if no scaling desired
# END CRITICAL ----

constraint.coefficients.sparse <- cc.sparse$nzcc %>%
  left_join(scale_factors %>% select(cnum, scale), by="cnum") %>% # a data frame
  mutate(nzcc_scaled=nzcc * scale)

# target.rules.enhanced <- cc.sparse$enhanced.targets
feasible.targets <- cc.sparse$feasible.targets %>%
  left_join(scale_factors %>% select(cnum, scale), by="cnum") %>%
  mutate(target_scaled=target * scale,
         rhs_scaled=rhs * scale,
         pdiff=rhs_scaled / target_scaled * 100 - 100,
         apdiff=abs(pdiff))
  

#******************************************************************************************************************
#  Compare weighted values on preliminary pufny file to target values ####
#******************************************************************************************************************



#******************************************************************************************************************
#  Set tolerances ####
#******************************************************************************************************************
#..Automate the setting of tolerances around constraints based upon rules ----
tolerances <- get_tolerances()

targets2 <- feasible.targets %>%
  select(h2vname, agi_group, cnum, target, rhs, target_scaled, rhs_scaled, pdiff, apdiff, vdesc) %>%
  mutate(tol.group=cut(apdiff, tolerances$apdiff.lb, include.lowest = TRUE, right=FALSE),
         tol.group=fct_explicit_na(tol.group, na_level = "(Missing)"),
         tol.default=tolerances$tol.default[as.integer(tol.group)],
         tol=tol.default)

targets2 %>%
  arrange(-tol)

# note comma after all but last row
overrides <- tribble(
  ~h2vname, ~agi_group, ~tol.override,
  "e26270", "[1,2.5e+04)", 100,
  "e01000", "[1,2.5e+04)", 100,
  "e03240", "[1e+05,2e+05)", 100,
  "e03240_n", "[-Inf,1)", 100,
  "e19200", "[1e+06, Inf)", 100,
  "e02500_n", "[1e+06, Inf)", 35,
  "e01700_n",	"[1e+06, Inf)", 10
)
overrides


targets.tol <- targets2 %>%
  left_join(overrides, by = c("h2vname", "agi_group")) %>%
  mutate(tol=ifelse(!is.na(tol.override), tol.override, tol))

count(targets.tol, tol, tol.default, tol.group) %>% mutate(cumn=cumsum(n))

targets.tol %>%
  arrange(desc(tol))

targets.tol %>% filter(tol.default != tol)

#******************************************************************************************************************
#  Prepare inputs for optimization ####
#******************************************************************************************************************
# Define constraint lower bounds (clb) and upper bounds (cub) based on tolerances established earlier
tol <- targets.tol$tol / 100
clb <- targets.tol$target_scaled - abs(targets.tol$target_scaled) * tol
cub <- targets.tol$target_scaled + abs(targets.tol$target_scaled) * tol

# what do the bounds look like vs. targets and data?
targets.tol %>%
  mutate(clb=clb, cub=cub) %>%
  select(h2vname, agi_group, cnum, target_scaled, rhs_scaled, clb, cub, tol, apdiff, vdesc) %>%
  arrange(-apdiff) %>%
  filter(row_number() <= 50)
# %>%   kable(digits=c(0, 0, 1, 1, 1, 1, 3, 3),          format.args=list(big.mark = ',')) %>%    kableExtra::kable_styling()

# make the sparseness structure for the constraint coefficients: 
#   a list of vectors, where each vector contains the INDICES of the non-zero elements of one row
cc.sparse.structure <- make.sparse.structure.from.nzcc(constraint.coefficients.sparse)
length(cc.sparse.structure) - nrow(feasible.targets) # must equal number of constraints

# quantile(constraint.coefficients.sparse$constraint_scale_factor)

# now prepare the inputs for ipopt
inputs <- list()
inputs$p <- 2
inputs$wt <- pufny1$wt
inputs$constraint.coefficients.sparse <- constraint.coefficients.sparse %>% mutate(nzcc=nzcc_scaled)
inputs$eval_jac_g_structure <- cc.sparse.structure
inputs$eval_h_structure <- lapply(1:length(inputs$wt), function(x) x) # diagonal elements of our Hessian

# set starting x values (adjustment factor), plus bounds on the x values
xlb <- rep(0, nrow(pufny1))
xub <- rep(10e3, nrow(pufny1))

x0 <- rep(1, nrow(pufny1))
# set.seed(1234)
# x0 <- runif(nrow(pufny1), min = min(xlb), max = max(xub))



#******************************************************************************************************************
#  Run ipoptr to get optimal x values ####
#******************************************************************************************************************
# se https://www.coin-or.org/Ipopt/documentation/node43.html for scaling
# With ma57, my own scaling, and xm1sq functions, the following work well
#   "obj_scaling_factor" = .001 AND  (1e-3)
#   "nlp_scaling_max_gradient" = 10
#  obj_scaling_factor appears more important
#  nlp_scaling_min_value is not recognized so don't use it

# Best approach seems to be:
#   do NOT use my own scaling
#   vary those 2 scaling factors


opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "obj_scaling_factor" = 1e-3, #  -- default 1, interacts with other scaling
             "nlp_scaling_max_gradient" = 10, # 10 improved things -- default 100
             "max_iter"=500,
             "output_file" = "nytarget_30_scaleopts_ma57.out")

# nytarget_21_scaleopts_ma57.out

# eval_f_xm1sq

# eval_f = eval_f_xtop, 
# eval_grad_f = eval_grad_f_xtop, 
# eval_h = eval_h_xtop, # the hessian is essential for this problem

# eval_f = eval_f_xm1sq,
# eval_grad_f = eval_grad_f_xm1sq,
# eval_h = eval_h_xm1sq, # the hessian is essential for this problem

a <- proc.time()
result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_xm1sq,
                 eval_grad_f = eval_grad_f_xm1sq,
                 eval_h = eval_h_xm1sq, # the hessian is essential for this problem
                 eval_g = eval_g, 
                 eval_jac_g = eval_jac_g,
                 eval_jac_g_structure = inputs$eval_jac_g_structure,
                 eval_h_structure = inputs$eval_h_structure,
                 constraint_lb = clb,
                 constraint_ub = cub,
                 opts = opts,
                 inputs = inputs)
b <- proc.time()
b - a

# If optimal take a quick look at the resulting x values ----
# str(result)
# result$solution
quantile(result$solution, probs=c(0, .01, .05, .10, .25, .5, .75, .9, .95, .99, .995, .999, 1))

tibble(x=result$solution, wt=pufny1$wt, wt2=x * wt) %>%
  filter(wt2 < 1) %>%
  mutate(wt2f=cut(wt2, 10)) %>%
  group_by(wt2f) %>%
  summarise(n=n())

tibble(xopt=result$solution) %>%
  ggplot(aes(xopt)) +
  geom_histogram(binwidth=.001, fill="blue") +
  geom_vline(xintercept = 1) +
  scale_x_continuous(breaks=seq(0, 20, .1), limits=c(0, 10)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of x values (ratio of new weight to old weight)")



#******************************************************************************************************************
#  CHECKING if constraint violations ####
#******************************************************************************************************************
# IF the result is infeasible, compare constraints to bounds
names(result)
names(targets.tol)
glimpse(targets.tol)
check <- tibble(cnum=targets.tol$cnum,
                target=targets.tol$target,
                lb=result$constraint_lower_bounds,
                target.calc=result$constraints,
                ub=result$constraint_upper_bounds) %>%
  mutate(violation=case_when(target.calc < lb ~ target.calc - lb,
                             target.calc > ub ~ ub - target.calc,
                             TRUE ~ 0),
         vpct=violation / target)

violations <-  check %>%
  filter(violation!=0) %>% 
  left_join(targets.tol %>% select(cnum, tol.group, pdiff, tol.default, tol, h2vname, agi_group)) %>%
  arrange(desc(abs(vpct)))




#******************************************************************************************************************
#  Construct a new reweighted file, synfile.rwt, that hits the targets ####
#******************************************************************************************************************
pufny2 <- pufny1 %>%
  mutate(ftype="puf.rwt",
         wt_rwt=wtny_ratio * result$solution,
         wt=wt_rwt)

quantile(pufny2$wt_rwt, probs=c(0, .01, .05, .10, .25, .5, .75, .9, .95, .99, 1))

# do some quick comparisons
stack <- bind_rows(pufny2 %>% mutate(wt=wtny_ratio, ftype="ratio"),
                   pufny2 %>% mutate(wt=wt_rwt, ftype="rwt"))
glimpse(stack)

p <- stack %>%
  ggplot(aes(x=wt, y = ..density..)) +
  geom_histogram(binwidth=25, fill="blue") +
  geom_vline(aes(xintercept = median(wt))) +
  scale_x_continuous(breaks=seq(0, 5000, 250), limits=c(0, 3000)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  facet_wrap(~ftype, nrow=3) +
  ggtitle("Distribution of weights")
p


#******************************************************************************************************************
#  Cursory evaluation of pufny2 ####
#******************************************************************************************************************
vlist <- c("e00100", "e00200")
convars <- unique(constraint.rhs.df$vname)

# prepare all combinations so they will appear in dfsums
base <- expand.grid(vname=unique(c(vlist, constraint.rhs.df$vname)), 
                    incgrp=unique(constraint.rhs.df$incgrp)) %>%
  left_join(inclink) %>%
  mutate(ftype="target")

# put constraint rhs in a form that allows easy comparison ftype incgrp, agi_group, vname wtsum.m valsum.b
rhs <- base %>%
  left_join(constraint.rhs.df) %>%
  mutate(stattype=ifelse(vname=="nret", "wtsum.k", "valsum.b")) %>%
  mutate(vname=ifelse(vname=="nret", "e00100", vname)) %>%
  spread(stattype, target) %>%
  mutate(valsum.b=valsum.b / 1e9,
         wtsum.k=wtsum.k / 1e3)
rhs  

# summaries by income range
dfsums <- stack %>%
  mutate(agi_group=cut(e00100, agibrks, right=FALSE),
         incgrp=paste0("inc", as.numeric(agi_group)),
         wtone=1e9) %>%
  select(ftype, incgrp, agi_group, wt, wtone, vlist) %>%
  gather(vname, value, -ftype, -incgrp, -agi_group, -wt) %>%
  group_by(ftype, incgrp, agi_group, vname) %>%
  summarise(n=n(), wtsum.k=sum(wt) / 1e3, valsum.b=sum(wt * value) / 1e9) %>%
  bind_rows(rhs) %>%
  left_join(puf.vnames %>% select(vname, vdesc))

dfsums

f <- function(vname.in, stat.in){
  dfsums %>%
    filter(vname==vname.in) %>%
    select(ftype, incgrp, agi_group, stat=stat.in, vname, vdesc) %>%
    spread(ftype, stat) %>%
    select(incgrp, agi_group, vname, target, ratio, rwt, vdesc) %>%
    adorn_totals(where="row") %>%
    mutate_at(vars(ratio, rwt), funs(diff=. - target, pdiff=(. - target) / target * 100)) %>%
    select(-c(vdesc), everything(), vdesc) %>%
    kable(digits=c(rep(0, 3), rep(1, 7), 0), format.args=list(big.mark = ','))
}

f("e00100", stat="wtsum.k")
f("e00100", stat="valsum.b")
f("e00200", stat="valsum.b")


dfsums %>% filter(vname=="e00200")




# create vectors for agi ranges, mars.groups, and the cross of the two
# 1 = ‘Under $1’
# 2 = '$1 under $25,000'
# 3 = '$25,000 under $50,000'
# 4 = '$50,000 under $75,000'
# 5 = '$75,000 under $100,000'
# 6 = '$100,000 under $200,000'
# 7 = ‘$200,000 under $500,000’
# 8 = ‘$500,000 under $1,000,000’
# 9 = ‘$1,000,000 or more’
# agi.ranges <- c(
#   "e00100 < 1",
#   "e00100 >= 1 & e00100 <= 25e3",
#   "e00100 >= 25e3 & e00100 <= 50e3",
#   "e00100 >= 50e3 & e00100 <= 75e3",
#   "e00100 >= 75e3 & e00100 <= 100e3",
#   "e00100 >= 100e3 & e00100 <= 200e3",
#   "e00100 >= 200e3 & e00100 <= 500e3",
#   "e00100 >= 500e3 & e00100 <= 1e6",
#   "e00100 >= 1e6")
# agi.ranges <- parens(agi.ranges)
# agi.ranges
# agi.ranges.df <- tibble(group.name=rep("agi.range", length(agi.ranges)),
#                         group.rule=agi.ranges, 
#                         group.sort=1:length(group.name))
# 
# stacked.groups <- agi.ranges.df # we'll use this if we have multiple groups
# 
# recipe.wide <- get_state_weighting_recipe("state_recipe1")
# recipe.wide
# 
# setdiff(recipe.wide$vname, names(pufny1)) # error check
# 
# recipe.long <- get_recipe_long(recipe.wide)
# recipe.long  

# target.rules <- recipe.long %>%
#   filter(vname %in% names(pufny1)) %>% # IMPORTANT -- must do before defining target.num !
#   dplyr::select(-group.description) %>%
#   right_join(stacked.groups)%>%
#   arrange(vname, fn, group.name, group.sort) %>%
#   mutate(calc.rule=paste0(group.rule, " * ", fn.rule),
#          target.num=row_number())
# target.rules
# names(target.rules)
# "vname"      "fn"         "group.name" "fn.rule"    "group.rule" "group.sort" "calc.rule"  "target.num"
# names(targslong)
# "vname"       "incgrp"      "target"      "select.rule" "calc.rule"   "ftype"       "targ_num"    "inum"        "agi_group"   "imin_ge"     "imax_lt" 

# we need: target.num, 
# pass in rule
