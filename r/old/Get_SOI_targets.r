
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
source("./r/includes/globals_system_specific_boyd.r")

source("./r/includes/functions_general.r")
source("./r/includes/functions_target_setup_and_analysis.r")
source("./r/includes/functions_ipopt.r")


#****************************************************************************************************
#                get puf data ####
#****************************************************************************************************
# puf <- get_puf.base() # this drops the aggregate records
puf<- readRDS(paste0(globals$tc.dir, "puf_lc.rds")) # note that this has all records, and has wt variable
glimpse(puf)

puf.vnames <- get_puf_vnames()


#****************************************************************************************************
#                Create inclink - data frame to link Historical Table 2 agi groups to puf cuts ####
#****************************************************************************************************
# define logical expressions for income groups
# 2011
# 0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $25,000'
# 3 = '$25,000 under $50,000'
# 4 = '$50,000 under $75,000'
# 5 = '$75,000 under $100,000'
# 6 = '$100,000 under $200,000'
# 7 = ‘$200,000 under $500,000’
# 8 = ‘$500,000 under $1,000,000’
# 9 = ‘$1,000,000 or more’
agibrks_2011 <- c(-Inf, 1, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf)

# 2016
# 0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $10,000'
# 3 = '$10,000 under $25,000'
# 4 = '$25,000 under $50,000'
# 5 = '$50,000 under $75,000'
# 6 = '$75,000 under $100,000'
# 7 = '$100,000 under $200,000'
# 8 = ‘$200,000 under $500,000’
# 9 = ‘$500,000 under $1,000,000’
# 10 = ‘$1,000,000 or more’
agibrks_2016 <- c(-Inf, 1, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf)

agibrks <- agibrks_2011
ifactor <- cut(agibrks, agibrks, right=FALSE)
ifactor
levels(ifactor)
inclink <- tibble(inum=1:(length(agibrks) - 1), 
                  incgrp=paste0("inc", inum), 
                  agi_group=levels(ifactor)[-length(ifactor)],
                  imin_ge=agibrks[inum],
                  imax_lt=agibrks[inum + 1])
inclink


#****************************************************************************************************
#                ONETIME - create targets in standard format ####
#****************************************************************************************************
#.. SOI Historical Table 2 values for NY for 2011 ----
vnames <- c("item", paste0("inc", 0:9))
df <- read_excel(paste0(globals$hist2, "11in33ny.xls"), col_names = vnames)
glimpse(df)

firstrow <- which(df$item=="NEW YORK") + 1
lastrow <- which(str_detect(df$item, coll("** - Not shown to avoid disclosure"))) - 1
firstrow; lastrow
soiny <- df %>%
  filter(row_number() >= firstrow, row_number() <= lastrow) %>%
  mutate(lineno=row_number()) %>%
  gather(incgrp, target, -lineno, -item) %>%
  mutate(target=as.numeric(target))
ht(soiny)

#.. crosswalk between SOI names and puf names ----
# I created variable names in this Excel file that can be used to link to Historical Table 2
xlfile <- "Boyd_State_PUF_info(2).xlsx"
xw <- read_excel(paste0("./data/", xlfile), 
                 sheet = "2011_hist2_puf",
                 range="A3:D106")
xw

xw %>%
  filter(!is.na(h2vname))

#.. create potential targets and corresponding rules
# mutate(other.rule=ifelse(h2vname=="nret_joint", "(MARS==2)", ""),
#        h2vname=ifelse(h2vname=="nret_joint", "e00100_n", h2vname)) %>%
vdrop <- c("nret_prep", "xdep", "totinc")
soi_rules <- xw %>%
  filter(!h2vname %in% vdrop, !is.na(h2vname)) %>%
  select(lineno, h2vname, table_desc) %>%
  mutate(vname=ifelse(h2vname=="nret_joint", "e00100", str_remove(h2vname, "_n"))) %>%
  left_join(soiny %>% select(-item), by="lineno") %>%
  left_join(inclink %>% select(incgrp, imin_ge, imax_lt), by="incgrp") %>%
  mutate(target=ifelse(str_detect(h2vname, "_n") |
                         str_detect(h2vname, "nret") |
                         str_detect(h2vname, "XTOT"),
                       target, target * 1000)) %>%
  mutate(imin_ge=ifelse(is.na(imin_ge), -Inf, imin_ge),
         imax_lt=ifelse(is.na(imax_lt), Inf, imax_lt),
         puf_multiplier=ifelse(str_detect(h2vname, "_n") | str_detect(h2vname, "nret"), 1, vname)) %>%
  mutate(inc.rule=paste0("e00100 >= ", imin_ge, " & ", "e00100 < ", imax_lt) %>% parens,
         other.rule=case_when(h2vname=="nret_joint" ~ "(MARS==2)",
                              str_detect(h2vname, "_n") & vname!="e00100" ~ paste0(vname, " > 0") %>% parens,
                              TRUE ~ ""),
         select.rule=paste0(inc.rule,
                            ifelse(other.rule!="", 
                                   paste0(" & ", other.rule),
                                   "")) %>% parens) %>%
  mutate(calc.rule=paste0("wt * ", puf_multiplier, " * ", select.rule) %>% parens) %>%
  select(-contains("rule"), everything(), contains("rule"))
soi_rules  %>% ht

count(soi_rules, calc.rule) %>% filter(n>1) # should be empty


#****************************************************************************************************
#                Prepare initial NY weights for the puf by ratio adjustment ####
#****************************************************************************************************
# with(synfile, eval(rules[target.num]))
# incrules <- read_csv("incgrp, rule
# inc0, e00100 < Inf
# inc1, (-Inf < e00100) & (e00100 < 1)
#                      ")

# pufny <- puf %>%
#   mutate(agi_group=cut(e00100, agibrks, right=FALSE))
# count(pufny, agi_group)

# determine weight factors by income range
# first, get puf weights by income range and marital status
pufwts <- puf %>%
  mutate(agi_group=cut(e00100, agibrks, right=FALSE)) %>%
  group_by(agi_group) %>%
  summarise(n=n(), nret=sum(wt), nret_joint=sum(wt * (MARS==2))) %>%
  mutate(nret_other=nret - nret_joint, incgrp=paste0("inc", as.numeric(agi_group)))
pufwts

# next, get same values for the soi data
targwts <- soi_rules %>%
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
  mutate(agi_group=cut(e00100, agibrks, right=FALSE),
         incgrp=paste0("inc", as.numeric(agi_group)),
         rtype=ifelse(MARS==2, "nret_joint", "nret_other")) %>%
  left_join(wtfactors %>% select(incgrp, rtype, wtfactor)) %>%
  mutate(wt_puf=wt,
         wtny_ratio=wt_puf * wtfactor,
         wt=wtny_ratio)


#****************************************************************************************************
#                Collapse initial ny puf and compare to targets ####
#****************************************************************************************************
soi_rules
count(soi_rules, h2vname)

targslong <- soi_rules %>%
  filter(incgrp!="inc0", !str_detect(h2vname, "totinc")) %>%
  mutate(vname=str_remove(h2vname, "_n")) %>%
  select(vname, incgrp, target, select.rule, calc.rule) %>%
  mutate(ftype="target", constraint.num=row_number()) %>%
  left_join(inclink)
ht(targslong)

names(pufny1) %>% sort

f2 <- function(targrule){
  rule <- parse(text=targrule) 
  df <- pufny1 %>%
    mutate(coeff=eval(rule)) %>%
    summarise(pufval=sum(coeff)) %>%
    mutate(calc.rule=targrule)
  return(df)
}

f2(targslong$calc.rule[1])
ldply(targslong$calc.rule[1], f2)

pufvals <- ldply(targslong$calc.rule, f2, .progress = "text")
pufvals

count(targslong, calc.rule) %>% filter(n>1)
count(pufvals, calc.rule) %>% filter(n>1)

comp <- targslong %>%
  select(constraint.num, vname, agi_group, target, calc.rule) %>%
  left_join(pufvals) %>%
  mutate(diff=pufval - target, pdiff=diff / target * 100) %>%
  select(-calc.rule, everything(), calc.rule)

comp %>%
  arrange(desc(abs(diff)))
comp %>%
  arrange(desc(abs(pdiff)))

# pufny1 %>%
#   filter(e00100 < 25e3, e00100 >=1, e01000!=0) %>%
#   select(RECID, e00100, e01000, wt)
# 
# 
# dtot <- function(df){
#   dsums <- df %>% 
#     summarise(target=sum(target)) %>%
#     mutate(ftype=df$ftype[1], vname=df$vname[1], incgrp="inc99", agi_group="Total")
#   dfout <- bind_rows(df, dsums)
#   return(dfout)
# }


# f("nret")
# f("e00100")
# f("e00200")


#****************************************************************************************************
#                Reweight initial weighted ny puf and compare to targets ####
#****************************************************************************************************
library("ipoptr")

# e18400 = SALT is hard to hit
count(targslong, vname) %>% left_join(puf.vnames)
vars <- c("e00100", "e00200", "e18400", "e18500", "e00300", "e00600", "e00900", "e01000", 
          "e01400", "e01700", "e01400", "e02500", "XTOT")

target.rules <- targslong %>%
  filter(vname %in% vars) %>%
  select(vname, select.rule, calc.rule, constraint.num, target) %>% # %>% filter(target.num <= 200)
  mutate(target.num=row_number())

system.time(cc.sparse <- get_constraint_coefficients_sparse(pufny1, target.rules))
glimpse(cc.sparse)
names(cc.sparse)
constraint.coefficients.sparse <- cc.sparse$nzcc
target.rules.enhanced <- cc.sparse$enhanced.targets

# retrieve vector with the constraint values for the synfile, and compute the vector for the puf
statepuf.rhs <- cc.sparse$enhanced.targets$df.rhs

# important to link the sort orders
# constraint.rhs.df <- target.rules %>%
#   filter(vname %in% c("nret", "e00100", "e00200")) %>%
#   mutate(target=ifelse(str_detect(vname, "nret"), target, target * 1000))


constraint.rhs <- target.rules$target
constraint.rhs

cbind(constraint.rhs, statepuf.rhs)

#******************************************************************************************************************
#  Compare weighted values on preliminary pufny file to target values ####
#******************************************************************************************************************
statepuf.vs.targets <- target.rules.enhanced %>%
  select(vname, calc.rule, constraint.num, target.num, feasible) %>%
  mutate(target.value=constraint.rhs,
         statepuf.value=statepuf.rhs,
         diff=statepuf.value - target.value,
         pdiff=statepuf.value / target.value - 1,
         apdiff=abs(pdiff))

targ.comp <- statepuf.vs.targets %>%
  mutate(target.value=target.value / 1e6,
         statepuf.value=statepuf.value / 1e6,
         diff=diff / 1e6)

targ.comp %>%
  kable(caption="Target and state puf weighted sums in $ millions, plus puf % diff from target",
        digits=c(rep(0, 5), 1, 1, 1, 3, 3), 
        format.args=list(big.mark = ','), format="html") %>%
  kableExtra::kable_styling()

targ.comp  %>%
  arrange(-abs(pdiff)) %>%
  filter(row_number() <= 25) %>%
  kable(caption="Target and state puf weighted sums in $ millions, plus state puf % diff from target,
        top up-to-25 worst differences",
        digits=c(rep(0, 5), 1, 1, 1, 3, 3), 
        format.args=list(big.mark = ',')) %>%
  kableExtra::kable_styling()


#******************************************************************************************************************
#  Set tolerances ####
#******************************************************************************************************************
#..Automate the setting of tolerances around constraints based upon rules ----
# They may need some tinkering with
# These can be overriden if desired
tolerances <- read_csv(
"apdiff.lb, tol.default
-Inf, 0
0, .001
.05, .01
.10, .03
.20, .05
.50, .075
.75, .1
1.00, .25
2.00, Inf")
tolerances <- tolerances %>% 
  mutate(apdiff.ub=lead(apdiff.lb)) %>% 
  select(starts_with("ap"), tol.default)
tolerances

statepuf.vs.targets <- statepuf.vs.targets %>%
  mutate(apdiff=abs(pdiff),
         tol.group=cut(apdiff, tolerances$apdiff.lb, include.lowest = TRUE, right=FALSE),
         # tol.group=addNA(tol.group),
         tol.group=fct_explicit_na(tol.group, na_level = "(Missing)"),
         tol.default=tolerances$tol.default[as.integer(tol.group)],
         tol=tol.default)
levels(statepuf.vs.targets$tol.group) # note that apdiff NaN yields missing values
count(statepuf.vs.targets, tol.group, tol.default)

# override the tolerance defaults based upon possibly subequent analysis of violations
targets.tol <- statepuf.vs.targets # %>%
  # mutate(tol=ifelse(constraint.num %in% c(245, 64), .05, tol))

count(targets.tol, tol, tol.default, tol.group)

targets.tol %>%
  arrange(desc(tol))

#******************************************************************************************************************
#  Prepare inputs for optimization ####
#******************************************************************************************************************
feasible.targets <- targets.tol %>%
  filter(feasible)

# Define constraint lower bounds (clb) and upper bounds (cub) based on tolerances established earlier
tol <- feasible.targets$tol
clb <- feasible.targets$target.value - abs(feasible.targets$target.value) * tol
cub <- feasible.targets$target.value + abs(feasible.targets$target.value) * tol

# what do the bounds look like vs. targets and data?
feasible.targets %>%
  mutate(clb=clb, cub=cub) %>%
  mutate_at(vars(clb, target.value, cub, statepuf.value), funs(. / 1e6)) %>%
  select(calc.rule, target.num, target.value, statepuf.value, clb, cub, tol, apdiff) %>%
  arrange(-apdiff) %>%
  filter(!is.infinite(tol)) %>%
  top_n(50) %>%
  kable(digits=c(0, 0, 1, 1, 1, 1, 3, 3), 
        format.args=list(big.mark = ',')) %>% 
  kableExtra::kable_styling()

# make the sparseness structure for the constraint coefficients: 
#   a list of vectors, where each vector contains the INDICES of the non-zero elements of one row
cc.sparse.structure <- make.sparse.structure.from.nzcc(constraint.coefficients.sparse)
length(cc.sparse.structure) - nrow(feasible.targets) # must equal number of constraints

# now prepare the inputs for ipopt
inputs <- list()
inputs$p <- 2
inputs$wt <- pufny1$wt
inputs$constraint.coefficients.sparse <- constraint.coefficients.sparse
inputs$eval_jac_g_structure <- cc.sparse.structure
inputs$eval_h_structure <- lapply(1:length(inputs$wt), function(x) x) # diagonal elements of our Hessian

# set starting x values (adjustment factor), plus bounds on the x values
x0 <- rep(1, nrow(pufny1))
xlb <- rep(0.05, nrow(pufny1))
xub <- rep(100, nrow(pufny1))


#******************************************************************************************************************
#  Run ipoptr to get optimal x values ####
#******************************************************************************************************************
opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=500,
             "output_file" = "nytarget.out")

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

tibble(xopt=result$solution) %>%
  ggplot(aes(xopt)) +
  geom_histogram(binwidth=.001, fill="blue") +
  geom_vline(xintercept = 1) +
  scale_x_continuous(breaks=seq(0, 20, .1), limits=c(0, 10)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of x values (ratio of new weight to old weight)")


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

#******************************************************************************************************************
#  CHECKING if constraint violations ####
#******************************************************************************************************************
# IF the result is infeasible, compare constraints to bounds
names(result)
names(feasible.targets)
glimpse(feasible.targets)
check <- tibble(target.num=feasible.targets$target.num,
                target=feasible.targets$target.value,
                lb=result$constraint_lower_bounds,
                target.calc=result$constraints,
                ub=result$constraint_upper_bounds) %>%
  mutate(violation=case_when(target.calc < lb ~ lb - target.calc,
                             target.calc > ub ~ ub - target.calc,
                             TRUE ~ 0),
         vpct=violation / target * 100)

violations <-  check %>%
  filter(violation!=0)
violations %>% 
  left_join(feasible.targets %>% select(target.num, pdiff, tol.group, tol.default, tol, vname)) %>%
  arrange(desc(abs(violation)))





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
