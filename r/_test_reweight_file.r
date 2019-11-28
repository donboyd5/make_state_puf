# 2/6/2019

# This program reweights a synthetic file so that it will hit or come near to targets in a base file, typically the PUF.

# CAUTION: Currently this is only set to work on the full PUF and a synfile of the full PUF

# Steps in this program ----
# The main things it does are:
# 1. ONETIME PER SYNFILE - Optionally prepare a file and its puf counterpart, if not already prepared.
#    - Get a synfile, stack it with PUF using only the variables that are in both files
#    - Run it through Tax-Calculator so that we can get desired calculated variables
#    - Save the prepared file and Tax-Calculator output as a list so that step 1 need not be done in the future on this synfile
# 2. Get previously-prepared synfile-PUF and tax output, merge, and separate PUF and synfile
# 3. Define and construct a set of targets (weighted values) from the base file
# 4. Compare weighted values on synthetic file to target values
# 5. Prepare inputs for optimization
# 6. Run ipoptr to get optimal x values
# 7. Construct a new reweighted file, synfile.rwt, that hits the targets
#    - Obtain adjustment factors for synfile weights that minimize a distortion function based on size of the adjustment,
#      while satisfying constraints that ensure that the targets are hit (or that results are within defined tolerances)
#    - Construct synfile.rwt by adjusting the wt variable (and save the raw synthesized weight as wt.rawsyn)
# 8. Save all 3 files as a list, and also as csv to synpuf


#****************************************************************************************************
#                TODO ####
#****************************************************************************************************
# - get counts


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("scales")
library("hms") # hms, for times.
library("lubridate") # lubridate, for date/times.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
library("vctrs")
library("precis")

library("tibbletime") # https://business-science.github.io/tibbletime/

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)

library("ipoptr")


# library("synthpop") # note: masks select in dplyr


#****************************************************************************************************
#                Globals ####
#****************************************************************************************************


#****************************************************************************************************
#                Includes ####
#****************************************************************************************************
source("./r/includes/globals_system_specific_boyd.r") # use a different version of this file if changing systems
source("./r/includes/globals_other.r")

source("./r/includes/functions_general.r")
source("./r/includes/functions_target_setup_and_analysis.r")
source("./r/includes/functions_ipopt.r")


#****************************************************************************************************
#                Additional functions ####
#****************************************************************************************************


#****************************************************************************************************
#                Initialization ####
#****************************************************************************************************
puf.vnames <- get_puf_vnames()


#******************************************************************************************************************
#  2. Get previously-prepared synfile-PUF and tax output, merge, and separate PUF and synfile ####
#******************************************************************************************************************
# sfname <- "synthpop4_all_rwprep.rds"
sfname <- "synthpop5_all_rwprep.rds"
# sfname <- "synpuf8.rds_rwprep.rds"

synprep <- readRDS(paste0(globals$tc.dir, sfname))

# merge and then split
# Tax-Calculator variables desired
tcvars <- c("c00100", "standard", "c17000", "c18300", "c21060", "c04800", "taxbc", "c09600", "c05800")
# c00100 AGI
# c17000 Sch A: Medical expenses deducted
# c18300 Sch A: State and local taxes deducted
# c21060 Itemized deductions before phase-out (zero for non-itemizers)
# standard Standard deduction (zero for itemizers)
# c04800 Regular taxable income
# taxbc regular tax before credits
# c09600 Alternative Minimum Tax (AMT) liability
# c05800 taxbc plus AMT liability

mrgdf <- left_join(synprep$tc.base, synprep$tc.output %>% dplyr::select(RECID, tcvars))
glimpse(mrgdf)
count(mrgdf, ftype)
count(mrgdf, m)

puffile <- mrgdf %>% filter(ftype=="puf")
# synfile <- mrgdf %>% filter(ftype=="syn") %>% filter(m==1)
synfile <- mrgdf %>% filter(ftype=="syn") %>% mutate(wt=wt / 3)

names(puffile) %>% sort
names(synfile) %>% sort


#******************************************************************************************************************
#  3a. Define agi and marital status groups, and their cross ####
#******************************************************************************************************************

# create vectors for agi ranges, mars.groups, and the cross of the two
agi.ranges <- c(
  "c00100 < 0",
  "c00100 > 0 & c00100 <= 25e3",
  "c00100 > 25e3 & c00100 <= 50e3",
  "c00100 > 50e3 & c00100 <= 75e3",
  "c00100 > 75e3 & c00100 <= 100e3",
  "c00100 > 100e3 & c00100 <= 200e3",
  "c00100 > 200e3 & c00100 <= 500e3",
  "c00100 > 500e3 & c00100 <= 1e6",
  "c00100 > 1e6 & c00100 <= 10e6",
  "c00100 > 10e6")
agi.ranges <- parens(agi.ranges)
agi.ranges
agi.ranges.df <- tibble(group.name=rep("agi.range", length(agi.ranges)),
                        group.rule=agi.ranges, 
                        group.sort=1:length(group.name))

mars.groups <- c("MARS==1", "MARS==2", "MARS %in% c(3, 4)")
mars.groups <- parens(mars.groups)
mars.groups
mars.groups.df <- tibble(group.name=rep("mars.group", length(mars.groups)),
                         group.rule=mars.groups, 
                         group.sort=1:length(group.name))

# cross these two groups
agi.mars.cross <- expand.grid(a=agi.ranges, b=mars.groups, stringsAsFactors = FALSE) %>%
  mutate(amcross=paste0(a, " & ", b),
         amcross=parens(amcross)) %>%
  .[["amcross"]]
agi.mars.cross
agi.mars.cross.df <- tibble(group.name=rep("agi.range_by_mars.group", length(agi.mars.cross)),
                            group.rule=agi.mars.cross, 
                            group.sort=1:length(group.name))

stacked.groups <- bind_rows(agi.ranges.df, mars.groups.df, agi.mars.cross.df)
stacked.groups


#******************************************************************************************************************
#  3b. Define target rules ####
#******************************************************************************************************************
names(synfile) %>% sort

recipe.wide <- get_weighting_recipe("recipe3")

setdiff(recipe.wide$vname, names(synfile)) # error check

recipe.long <- get_recipe_long(recipe.wide)
recipe.long  

target.rules <- recipe.long %>%
  filter(vname %in% names(synfile)) %>% # IMPORTANT -- must do before defining target.num !
  dplyr::select(-group.description) %>%
  right_join(stacked.groups)%>%
  arrange(vname, fn, group.name, group.sort) %>%
  mutate(calc.rule=paste0(group.rule, " * ", fn.rule),
         target.num=row_number())
target.rules


#******************************************************************************************************************
#  3c. Calculate targets (weighted values) from the base file and counterparts on the synfile ####
#******************************************************************************************************************

# create a set of logical rules that will define subsets of the data for which we will define targets
# and variables that we will target in those subsets, and whether we are targeting
# their weighted sums or their weighted number of records (can do both)

system.time(cc.sparse <- get_constraint_coefficients_sparse(synfile, target.rules))
names(cc.sparse)
constraint.coefficients.sparse <- cc.sparse$nzcc
target.rules.enhanced <- cc.sparse$enhanced.targets

# retrieve vector with the constraint values for the synfile, and compute the vector for the puf
synfile.rhs <- cc.sparse$enhanced.targets$synfile.rhs
system.time(constraint.rhs <- get_constraint_sums(puffile, target.rules))


#******************************************************************************************************************
#  4. Compare weighted values on synthetic file to target values ####
#******************************************************************************************************************
synfile.vs.targets <- target.rules.enhanced %>%
  dplyr::select(vname, fn, calc.rule, target.num, feasible) %>%
  mutate(target.value=constraint.rhs,
         syn.value=synfile.rhs,
         pdiff=syn.value / target.value * 100 - 100,
         apdiff=abs(pdiff))

targ.comp <- synfile.vs.targets %>%
  mutate(target.value=target.value / 1e6,
         syn.value=syn.value / 1e6)

targ.comp %>%
  kable(caption="Target and synthetic weighted sums in $ millions, plus syn % diff from target",
        digits=c(rep(0, 5), 1, 1, 1, 1), 
        format.args=list(big.mark = ','))

targ.comp  %>%
  arrange(-apdiff) %>%
  filter(row_number() <= 25) %>%
  kable(caption="Target and synthetic weighted sums in $ millions, plus syn % diff from target,
        top up-to-25 worst differences",
        digits=c(rep(0, 5), 1, 1, 1, 1), 
        format.args=list(big.mark = ','))


#******************************************************************************************************************
#  4a. Set tolerances ####
#******************************************************************************************************************
#..Automate the setting of tolerances around constraints based upon rules ----
# They may need some tinkering with
# These can be overriden if desired
tolerances <- read_csv(
"apdiff.lb, tol.default
0, .001
5, .01
10, .03
20, .05
50, .3
75, .6
100, .85
200, Inf
Inf, Inf")
tolerances <- tolerances %>% mutate(apdiff.ub=lead(apdiff.lb)) %>% select(starts_with("ap"), tol.default)
tolerances

synfile.vs.targets <- synfile.vs.targets %>%
  mutate(tol.group=cut(apdiff, tolerances$apdiff.lb, include.lowest = TRUE),
         tol.group=addNA(tol.group),
         tol.default=tolerances$tol.default[as.integer(tol.group)],
         tol=tol.default)
summary(synfile.vs.targets)
count(synfile.vs.targets, tol.group, tol.default)


# override the tolerance defaults based upon possibly subequent analysis of violations
# synfile.vs.targets$tol[c(110, 378, 392, 406)] <- .75
# synfile.vs.targets <- synfile.vs.targets %>%
#   mutate(tol=case_when(calc.rule=="(c00100 < 0) * (wt)" ~ .2,
#                        TRUE ~ tol))

targets.tol <- synfile.vs.targets %>%
  mutate(tol=ifelse(vname=="c00100" & (fn %in% c("n.sum", "val.sum")), .005, tol))

count(targets.tol, tol, tol.default, tol.group)


#******************************************************************************************************************
#  5. Prepare inputs for optimization ####
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
  mutate_at(vars(clb, target.value, cub, syn.value), funs(. / 1e6)) %>%
  select(calc.rule, target.num, target.value, syn.value, clb, cub, tol, apdiff) %>%
  arrange(-apdiff) %>%
  filter(!is.infinite(tol)) %>%
  top_n(50) %>%
  kable(digits=c(0, 0, 1, 1, 1, 1, 3), 
        format.args=list(big.mark = ','))

# make the sparseness structure for the constraint coefficients: 
#   a list of vectors, where each vector contains the INDICES of the non-zero elements of one row
cc.sparse.structure <- make.sparse.structure.from.nzcc(constraint.coefficients.sparse)
length(cc.sparse.structure) - nrow(feasible.targets) # must equal number of constraints

# now prepare the inputs for ipopt
inputs <- list()
inputs$p <- 2
inputs$wt <- synfile$wt
inputs$constraint.coefficients.sparse <- constraint.coefficients.sparse
inputs$eval_jac_g_structure <- cc.sparse.structure
inputs$eval_h_structure <- lapply(1:length(inputs$wt), function(x) x) # diagonal elements of our Hessian

# set starting x values (adjustment factor), plus bounds on the x values
x0 <- rep(1, nrow(synfile))
xlb <- rep(0.1, nrow(synfile))
xub <- rep(5, nrow(synfile))


#******************************************************************************************************************
#  6. Run ipoptr to get optimal x values ####
#******************************************************************************************************************
opts <- list("print_level" = 5,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma57", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=500,
             "output_file" = "syntarget.out")

a <- proc.time()
result <- ipoptr(x0 = x0,
              lb = xlb,
              ub = xub,
              eval_f = eval_f_xtop, 
              eval_grad_f = eval_grad_f_xtop, 
              eval_g = eval_g, 
              eval_jac_g = eval_jac_g,
              eval_jac_g_structure = inputs$eval_jac_g_structure,
              eval_h = eval_h_xtop, # the hessian is essential for this problem
              eval_h_structure = inputs$eval_h_structure,
              constraint_lb = clb,
              constraint_ub = cub,
              opts = opts,
              inputs = inputs)
b <- proc.time()
b - a


# IF the result is infeasible, compare constraints to bounds ----
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
violations %>% left_join(feasible.targets %>% select(target.num, pdiff, tol.group, tol.default, tol, vname, fn))


# If optimal take a quick look at the resulting x values ----
# str(result)
# result$solution
quantile(result$solution, probs=c(0, .01, .05, .10, .25, .5, .75, .9, .95, .99, 1))

tibble(xopt=result$solution) %>%
  ggplot(aes(xopt)) +
  geom_histogram(binwidth=.001, fill="blue") +
  geom_vline(xintercept = 1) +
  scale_x_continuous(breaks=seq(0, 20, .05)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  ggtitle("Distribution of x values (ratio of new weight to old weight)")


#******************************************************************************************************************
#  7. Construct a new reweighted file, synfile.rwt, that hits the targets ####
#******************************************************************************************************************
synfile.rwt <- synfile %>%
  mutate(ftype="syn.rwt",
         wt.syn=wt,
         wt.rwt=wt.syn * result$solution,
         wt=wt.rwt)

quantile(synfile.rwt$wt.rwt, probs=c(0, .01, .05, .10, .25, .5, .75, .9, .95, .99, 1))

# do some quick comparisons
stack <- bind_rows(puffile, 
                   synfile.rwt %>% mutate(wt=wt.syn, ftype="syn"),
                   synfile.rwt %>% mutate(wt=wt.rwt, ftype="rwt"))
glimpse(stack)

p <- stack %>%
  mutate(wt=ifelse(ftype %in% c("syn", "rwt"), wt * 3, wt)) %>% # adjust for different number of records
  ggplot(aes(x=wt, y = ..density..)) +
  geom_histogram(binwidth=25, fill="blue") +
  geom_vline(aes(xintercept = median(wt))) +
  scale_x_continuous(breaks=seq(0, 5000, 250), limits=c(0, 3000)) +
  theme(axis.text.x=element_text(size=8, angle=30)) +
  facet_wrap(~ftype, nrow=3) +
  ggtitle("Distribution of weights")
p


#******************************************************************************************************************
#  7a. Cursory evaluation of synfile.rwt ####
#******************************************************************************************************************
# summaries by income range
agiranges <- c(-Inf, 0, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, 10e6, Inf)
vlist <- c("c00100", "e00200", "e00300", "e00600", "e01700", "p23250", "taxbc")
dfsums <- stack %>%
  mutate(agirange=cut(c00100, agiranges, right=FALSE),
         wtone=1e9) %>%
  select(ftype, agirange, wt, wtone, vlist) %>%
  gather(vname, value, -ftype, -agirange, -wt) %>%
  group_by(ftype, agirange, vname) %>%
  summarise(n=n(), wtsum.m=sum(wt) / 1e6, valsum.b=sum(wt * value) / 1e9) %>%
  left_join(puf.vnames %>% select(vname, vdesc))
dfsums

f <- function(vname.in, stat.in){
  dfsums %>%
    filter(vname==vname.in) %>%
    dplyr::select(ftype, agirange, stat=stat.in, vname, vdesc) %>%
    spread(ftype, stat) %>%
    janitor::adorn_totals(where="row") %>%
    mutate_at(vars(syn, rwt), funs(diff=. - puf, pdiff=(. - puf) / puf * 100)) %>%
    select(-c(vname, vdesc), everything(), vname, vdesc) %>%
    kable(digits=c(0, rep(1, 5), rep(1, 2)), format.args=list(big.mark = ','))
}

f("c00100", stat="valsum.b")
f("e00200", stat="valsum.b")
f("taxbc", stat="valsum.b")
f("e01700", stat="valsum.b")


agiranges <- c(-Inf, 0, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, 10e6, Inf)
comps <- stack %>%
  mutate(agirange=cut(c00100, agiranges, right=FALSE), wtvar=1e9) %>%
  group_by(ftype, agirange) %>%
  summarise_at(vars(wtvar, c00100, taxbc, e00200, e01700), funs(sum(. * wt) / 1e9))
comps

f.agi <- function(var) {
  comps %>%
    dplyr::select(ftype, agirange, value=var) %>%
    spread(ftype, value) %>%
    janitor:: adorn_totals(where="row") %>%
    mutate(syn.diff=.[[3]] - puf,
           rwt.diff=.[[4]] - puf,
           syn.pdiff=syn.diff / puf * 100,
           rwt.pdiff=rwt.diff / puf * 100,
           var=var) %>%
    kable(caption="Comparison of puf, synfile, and reweighted synfile",
          digits=c(rep(1, 8), 0), 
          format.args=list(big.mark = ','))
}
f.agi("wtvar")
f.agi("c00100")
f.agi("e00200")
f.agi("taxbc")


# comps.m <- stack %>%
#   mutate(mstat=factor(MARS, levels=c(1:4), labels=c("single", "married", "other", "other")),
#          wtvar=1e9) %>%
#   group_by(ftype, mstat) %>%
#   summarise_at(vars(wtvar, c00100, taxbc, e00200, e01700), funs(sum(. * wt) / 1e9))
# comps.m
# 
# f.mstat <- function(var) {
#   comps.m %>%
#     dplyr::select(ftype, mstat, value=var) %>%
#     spread(ftype, value) %>%
#     janitor:: adorn_totals(where="row") %>%
#     mutate(syn.diff=.[[3]] - puf,
#            rwt.diff=.[[4]] - puf,
#            syn.pdiff=syn.diff / puf * 100,
#            rwt.pdiff=rwt.diff / puf * 100,
#            var=var) %>%
#     kable(caption="Comparison of puf, synfile, and reweighted synfile",
#           digits=c(rep(1, 8), 0), 
#           format.args=list(big.mark = ','))
# }
# f.mstat("wtvar")
# f.mstat("c00100")
# f.mstat("e00200")
# f.mstat("taxbc")
# 
# comps.agim <- stack %>%
#   mutate(agirange=cut(c00100, agiranges, right=FALSE),
#          mstat=factor(MARS, levels=c(1:4), labels=c("single", "married", "other", "other")),
#          wtvar=1e9) %>%
#   group_by(ftype, agirange, mstat) %>%
#   summarise_at(vars(wtvar, c00100, taxbc, e00200, e01700), funs(sum(. * wt) / 1e9)) %>%
#   ungroup
# comps.agim
# 
# f.agim <- function(var, mstat.use) {
#   comps.agim %>%
#     dplyr::select(ftype, agirange, mstat, value=var) %>%
#     filter(mstat==mstat.use) %>%
#     spread(ftype, value) %>%
#     janitor:: adorn_totals(where="row") %>%
#     mutate(syn.diff=.[[4]] - puf,
#            rwt.diff=.[[5]] - puf,
#            syn.pdiff=syn.diff / puf * 100,
#            rwt.pdiff=rwt.diff / puf * 100,
#            var=var) %>%
#     kable(caption="Comparison of puf, synfile, and reweighted synfile",
#           digits=c(0, 0, rep(1, 7), 0), 
#           format.args=list(big.mark = ','))
# }
# f.agim("wtvar", "single")
# f.agim("wtvar", "married")
# f.agim("wtvar", "other")
# 
# f.agim("c00100", "single")
# f.agim("c00100", "married")
# f.agim("c00100", "other")
# 
# f.agim("e00200", "single")
# f.agim("e00200", "married")
# f.agim("e00200", "other")
# 
# f.agim("taxbc", "single")
# f.agim("taxbc", "married")
# f.agim("taxbc", "other")


#******************************************************************************************************************
#  8. Save the reweighted file ####
#******************************************************************************************************************
glimpse(stack)
count(stack, ftype)
sfname <- "synthpop4"
sfname <- "synthpop5"
sfname <- "synpuf8"

saveRDS(synfile.rwt, paste0(globals$tc.dir, "synthpop5_all_rwt.rds"))

# fnout <- function(fileid) paste0(globals$synd, sfname, "_", fileid, ".csv")
# write_csv(stack %>% filter(ftype=="puf"), fnout("puf"))
# write_csv(stack %>% filter(ftype=="syn"), fnout("syn"))
# write_csv(stack %>% filter(ftype=="syn.rwt"), fnout("rwt"))



