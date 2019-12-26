
target_state <- "NY"

# get national data, and all targets -----
# initial weights, every state, every record
ratio_weights_all <- readRDS(paste0(globals$statedir, "state_ratio_adjusted_weights_2017.rds"))

# national puf using my 2017 grow factors, with selected 2017 Tax-Calculator output variables, and wtus_2017 weight
pufbase_all <- readRDS(paste0(globals$statedir, "puf2017_weighted.rds"))

# 2017 targets all states, variables, AGI_STUBs
targets1 <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds")))

# fixes to create targets we want that are not in the targets file ----
# create MARS3
targets_MARS3 <- targets1 %>%
  filter(h2vname %in% c("N1", "MARS1", "MARS2", "MARS4")) %>%
  select(year, stabbr, h2vname, AGI_STUB, target) %>%
  pivot_wider(names_from = h2vname, values_from = target) %>%
  mutate(target = N1 - MARS1 - MARS2 - MARS4,
         # CAUTION: N1 total returns can be inconsistent with MARS1, MARS2, and MARS4, requiring negative MARS3
         # don't allow this
         # and make sure, in targeting that we target each return type rather than N1!! (or else we need to recalc N1)
         target = ifelse(target < 0, 0, target),
         h2vname = "MARS3",
         table_desc = "Number of married-separate returns") %>%
  select(-c(N1, MARS1, MARS2, MARS4))
summary(targets_MARS3)
targets_MARS3 %>% filter(target==0) # two small states in two agi ranges with few people -- ok

# create a number of returns target for AGI using number of returns for total income N02650
# Historical Table 2 data files do not have # of returns with nonzero AGI, but they do have:
#  N1 number of returns and
#  N02650 number of returns with total income
# in 2017 we have:
# N1    152,455,900 from csv
# N02650   151,883,220 from csv
# Number Total income 152,455,900 from xlsx
# note that total income A02650 is 1.4% higher than total AGI A00100 so this may overstate things slightly
targets_N00100 <- targets1 %>%
  filter(h2vname=="N02650") %>%
  mutate(h2vname="N00100",
         table_desc="Number of returns with adjusted gross income (AGI)")

targets_all <- bind_rows(targets1, targets_MARS3, targets_N00100)
ht(targets_all)
# END temporary fix ----

saveRDS(targets_all, here::here("data", "targets_all.rds"))
targets_all <- readRDS(here::here("data", "targets_all.rds"))

# create a crosswalk between PUF variable names and h2vnames ----
# for now we will work with dense matrices - if it is a problem, we'll change to sparse
(constraint_names <- unique(targets_all$h2vname) %>% sort)
(puf_names <- names(pufbase_all) %>% sort)
(puf_link_names <- c(puf_names, paste0("MARS", 1:4))) # we will add MARS variables to the state puf LATER

setdiff_all(constraint_names, puf_names)

# we need a "base name" for each constraint_name if it will match against a comparable puf basename
# for example we want to match constraint A00200 wages amount to puf E00200
mapping <- tibble(constraint_name=unique(targets_all$h2vname) %>% sort) %>%
  mutate(constraint_type =
           case_when(str_sub(constraint_name, 1, 1) == "A" ~ "amount", # all "A" constraints are amounts
                     (str_sub(constraint_name, 1, 1) == "N") &
                       (!constraint_name %in% c("N1", "N2", "NUMDEP")) ~ "n_nonzero",
                     constraint_name %in% c("N1", paste0("MARS", 1:4)) ~ "n_returns",
                     constraint_name == "N2" ~ "n_exempt",
                     constraint_name == "NUMDEP" ~ "n_depend",
                     constraint_name %in% c("ELDERLY") ~ "n_other", # maybe we'll target these some day
                     TRUE ~ "ignore"),
         constraint_base_name = ifelse(constraint_type %in% c("amount", "n_nonzero"),
                                       str_sub(constraint_name, 2, -1),
                                       NA_character_),
         # define linkage to puf. Note that the constraints do NOT have a variable that corresponds to AGI # returns
         puf_link =
           # deal with special cases first, then general cases
           case_when(constraint_base_name == "00100" ~ "c00100",
                     constraint_base_name == "05800" ~ "c05800",
                     constraint_base_name == "09600" ~ "c09600",
                     constraint_name == "N2" ~ "XTOT",
                     constraint_name %in% paste0("MARS", 1:4) ~ constraint_name,
                     constraint_type %in% c("amount", "n_nonzero") ~ paste0("E", constraint_base_name),
                     TRUE ~ NA_character_))
setdiff_all(unique(mapping$puf_link), puf_link_names)
length(intersect(unique(mapping$puf_link), puf_link_names))

mapping
count(mapping, constraint_type)
# mapping %>% filter(is.na(constraint_type))
mapping %>% filter(constraint_type == "ignore")

mapping %>%
  filter(!is.na(constraint_base_name)) %>%
  select(constraint_type, constraint_base_name, constraint_name) %>%
  pivot_wider(names_from = constraint_type, values_from = constraint_name) %>%
  filter(is.na(amount) | is.na(n_nonzero)) # should be no records - we now have a target for n_nonzero for 00100 agi

# this is a pretty good start for targets
targets_trimmed <- mapping %>%
  filter(puf_link %in% puf_link_names) %>%
  left_join(targets_all %>% rename(constraint_name=h2vname)) %>%
  arrange(year, stabbr, AGI_STUB, constraint_name) %>%
  mutate(target_num=row_number())
ht(targets_trimmed)
count(targets_trimmed %>% filter(AGI_STUB != 0), stabbr) # 510 targets per state, 51 for each income range
unique(targets_trimmed$puf_link)

targets_state <- targets_trimmed %>%
  filter(stabbr==target_state, AGI_STUB != 0)
unique(targets_state$puf_link)

# now we need to create a compatible PUf that has only the variables we want and that has the right initial weight
pufbase_state <- 
  pufbase_all %>%
  select(RECID, MARS, one_of(unique(targets_state$puf_link))) %>%
  mutate(stabbr=target_state) %>%
  left_join(ratio_weights_all %>% 
              filter(stabbr==target_state) %>%
              select(RECID, AGI_STUB, weight_state),
            by="RECID") %>%
  mutate(weight2=weight_state) %>% # we need to keep a copy of the state weight
  pivot_wider(names_from = MARS, names_prefix = "MARS", values_from = weight2, values_fill = list(weight2=0)) %>%
  arrange(RECID) %>%
  mutate(i=row_number()) # useful to have for the constraint coefficients data frame
saveRDS(pufbase_state, paste0(globals$statedir, "pufbase_state.rds"))
glimpse(pufbase_state)
sum(pufbase_state$weight_state)


# create constraint coefficients
# to be on the safe side, make a long sparse file and assign a ccnum to each constraints
long <- pufbase_state %>%
  pivot_longer(cols=-c(i, RECID, stabbr, AGI_STUB, weight_state), names_to="puf_link", values_to = "value") %>%
  filter(value!=0)
ht(long)
count(long, AGI_STUB)
count(long, puf_link)

# link it to constraints as we will need 2 constraints for most variables -- # of nonzeros, and amount
long2 <- long %>% 
  full_join(targets_state, by=c("stabbr", "AGI_STUB", "puf_link"))
ht(long2)

tmp <- long2 %>%
  filter(is.na(RECID) | is.na(value))
tmp
# we won't have any returns in the puf where taxbc is nonzero and the AGI_STUB==1 (AGI < $1)
# even though the historical table 2 data have about 910 such returns, with total taxbc of ~$31m
# that target is nonfeasible and so we will make note of it and drop it

targets_state_nonfeas <- long2 %>% filter(is.na(RECID)) %>% select(-RECID, -value)

targets_state_feas <- targets_state %>%
  filter(!target_num %in% targets_state_nonfeas$target_num) %>%
  arrange(AGI_STUB, target_num) %>%
  # create a sequence within AGI_STUB
  arrange(AGI_STUB, constraint_name) %>%
  group_by(AGI_STUB) %>%
  # j is the constraint number WITHIN THIS AGI_STUB - it may not be the same constraint name from one stub to another
  mutate(j=row_number()) %>%
  ungroup
targets_state_feas %>% group_by(AGI_STUB) %>% summarise(ngroups=n(), topgroup=max(j)) # should be the same
# this is what we will use for targeting and for constraint coefficients


# now we can create a data frame of nonzero constraint coefficients
count(long2, constraint_type)
nzcc <- targets_state_feas %>%
  left_join(long2 %>% select(-c(puf_link, year, stabbr, AGI_STUB, table_desc, target, 
                                constraint_name, constraint_type, constraint_base_name)), by="target_num") %>%
  mutate(nzcc=case_when(constraint_type=="amount" ~ value * weight_state,
                        constraint_type=="n_nonzero" ~ weight_state,
                        constraint_type=="n_exempt" ~ value * weight_state,
                        constraint_type=="n_returns" ~ weight_state,
                        TRUE ~ NA_real_))
glimpse(nzcc)
ht(nzcc)

# we can compare the starting point to the targets from this and use it to set constraint bounds
starting_point <- nzcc %>%
  group_by(AGI_STUB, j, target_num, constraint_name, constraint_type, table_desc) %>%
  summarise(target=mean(target),
            file=sum(nzcc),
            diff=file - target,
            pdiff=diff / target * 100)

starting_point %>%
  filter(AGI_STUB==8) %>%
  mutate(table_desc=str_remove(table_desc, "Number of") %>% str_sub(., 1, 35)) %>%
  # note that we do not have taxbc 5800 as a target in AGI_STUB 1 but we do in other stubs
  kable(digits=c(rep(0, 9), 1), format="rst", format.args = list(big.mark=","))

# now we are ready to set up the problem for ipopt

# we need a function to calculate constraints

# create a data frame with tolerances
# show a reminder of the constraints
starting_point %>%
  group_by(constraint_name, constraint_type, table_desc) %>%
  summarise(target=sum(target), pdiff=median(pdiff)) %>%
  arrange(-target)

# create a few priority levels
p1 <- c("A00100", "A00200", "A05800", "A09600", "A18500", "N2", "MARS1", "MARS2", "MARS4")
p2 <- c("N00100", "N00200", "N05800", "N09600", "N18500")
tol_df <- starting_point %>%
  mutate(tol_default=case_when(constraint_name %in% c(p1, p2) ~ .005,
                               TRUE ~ abs(pdiff/100) * .10))

tol_df %>%
  filter(AGI_STUB==2) %>%
  select(-c(target_num, file)) %>%
  mutate(tol_default=tol_default * 100) %>%
  mutate(table_desc=str_remove(table_desc, "Number of") %>% str_sub(., 1, 35)) %>%
  kable(digits=c(rep(0, 8), 1, 1), format="rst")


# run the problem ----
# first scale 
# try to make the "typical" values of the non-zero first partial derivatives of the objective and constraint functions
# to be on the order of, say, 0.01 to 100. For example, if you multiply a problem function by a number K, then the first partial derivatives for this function are also multiplied by K.

tol_df %>% filter(AGI_STUB==stub) 

stub <- 1
cnames <- tol_df %>% filter(AGI_STUB==stub) %>% .$table_desc %>% str_remove(., "Number of") %>% str_sub(., 1, 35)

constraints_unscaled <- tol_df %>% filter(AGI_STUB==stub) %>% .$target
constraint_scales <- ifelse(constraints_unscaled==0, 1, abs(constraints_unscaled) / 1000) # each constraint will be this number when scaled
(constraints <- constraints_unscaled / constraint_scales)
# constraints <- constraints_unscaled

# create nzcc for the stub, and its own i and j for each record
nzcc_stub <- nzcc %>%
  ungroup %>% # just to be sure
  filter(AGI_STUB==stub) %>%
  arrange(constraint_name, RECID) %>%
  # NOTE!!: i gives the index for constraints, j gives it for the RECID (for the variables)
  mutate(i=group_indices(., constraint_name),
         j=group_indices(., RECID)) %>% # file-wide indexes based on sort of RECID and constraint_name
  mutate(nzcc_unscaled=nzcc,
         nzcc=nzcc_unscaled / constraint_scales[i])

nzcc_stub %>% select(constraint_name, RECID, i, j, nzcc, nzcc_unscaled) %>% arrange(RECID, constraint_name)

inputs <- list()
inputs$p <- 2
inputs$wt <- pufbase_state %>% filter(AGI_STUB==stub) %>% arrange(RECID) %>% .$weight_state
inputs$RECID <- pufbase_state %>% filter(AGI_STUB==stub) %>% arrange(RECID) %>% .$RECID
inputs$constraint_coefficients_sparse <- nzcc_stub
inputs$n_variables <- length(inputs$wt)
inputs$n_constraints <- length(constraints)
inputs$objscale <- 1e6
# inputs$objscale <- 1
inputs$constraint_scales <- constraint_scales

xlb <- rep(0, inputs$n_variables)
xub <- rep(100, inputs$n_variables)
x0 <- rep(1, inputs$n_variables)


tol <- tol_df %>% filter(AGI_STUB==stub) %>% .$tol_default

clb <- constraints - abs(constraints) * tol
cub <- constraints + abs(constraints) * tol
cbind(clb, constraints, cub) %>% round(0)

eval_jac_g_structure <- define_jac_g_structure_sparse2(inputs$constraint_coefficients_sparse, ivar="i", jvar="j")
eval_jac_g_structure[1]
length(eval_jac_g_structure)

eval_h_structure <- lapply(1:inputs$n_variables, function(x) x) # diagonal elements of our Hessian

eval_f_xtop(x0, inputs)# ; eval_f_absapprox(xlb, inputs); eval_f_absapprox(xub, inputs)
eval_grad_f_xtop(x0, inputs)
eval_g(x0, inputs)
eval_jac_g(x0, inputs)
eval_h_xtop(x0, obj_factor=1, hessian_lambda=rep(1, inputs$n_constraints), inputs) # length is n_variables
# length(unlist(eval_h_structure)) # length should be n_variables


# ma86 was faster in one test I did
opts <- list("print_level" = 0,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma86", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"= 100,
             #"obj_scaling_factor" = 10, # 1e7, # default 1
             #"nlp_scaling_max_gradient" = 1e-3, # 1e-3, # default 100
             #"derivative_test" = "first-order",
             #"derivative_test_print_all" = "yes",
             "output_file" = "temp1.out")

# osf, maxg, obj, conviol
# 1: 1 100: 2.15e7, 3.6e8
# 2: 100 100: 7.1e6, 3.9e8
# 3: 100 1000: 7.6e6 2.7e8
# 4: 100 10: 1e7 2.6e8
# 5: 1e5 10: 8.6e6 3.6e8
# 6: 1e-2 100: 2e8 7e9
# 7: 1e-3 1e3: 2e8 3e8
# 8: 1e7 1e-3: 1e7 2e8

result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_xtop, # arguments: x, inputs
                 eval_grad_f = eval_grad_f_xtop,
                 eval_g = eval_g, # constraints LHS - a vector of values
                 eval_jac_g = eval_jac_g,
                 eval_jac_g_structure = eval_jac_g_structure,
                 eval_h = eval_h_xtop, # the hessian is essential for this problem
                 eval_h_structure = eval_h_structure,
                 constraint_lb = clb,
                 constraint_ub = cub,
                 opts = opts,
                 inputs = inputs)

str(result)

tmp <- tibble(clb=clb * constraint_scales, 
              targ=constraints * constraint_scales,
              cub=cub * constraint_scales,
              conval=result$constraints * constraint_scales,
              start=eval_g(x0, inputs) * constraint_scales,
              pdiff1=start / targ * 100 - 100,
              pdiff2=conval / targ * 100 - 100,
              startv=case_when(start < clb ~ "LOW",
                               start > cub ~ "HIGH",
                               TRUE ~ ""),
              viol=case_when(conval < clb ~ "LOW",
                             conval > cub ~ "HIGH",
                             TRUE ~ ""),
              cname=cnames)
tmp %>%
  kable(format="rst", digits=c(rep(0, 5), 1, 1), format.args=list(big.mark=","))



# now cycle through the AGI_STUBs to get new weights for all records ----
runstub <- function(AGI_STUB) {
  stub <- AGI_STUB
  cnames <- tol_df %>% filter(AGI_STUB==stub) %>% .$table_desc %>% str_remove(., "Number of") %>% str_sub(., 1, 35)
  
  constraints_unscaled <- tol_df %>% filter(AGI_STUB==stub) %>% .$target
  constraint_scales <- ifelse(constraints_unscaled==0, 1, abs(constraints_unscaled) / 1000) # each constraint will be this number when scaled
  constraints <- constraints_unscaled / constraint_scales
  
  # create nzcc for the stub, and its own i and j for each record
  nzcc_stub <- nzcc %>%
    ungroup %>% # just to be sure
    filter(AGI_STUB==stub) %>%
    arrange(constraint_name, RECID) %>%
    # NOTE!!: i gives the index for constraints, j gives it for the RECID (for the variables)
    mutate(i=group_indices(., constraint_name),
           j=group_indices(., RECID)) %>% # file-wide indexes based on sort of RECID and constraint_name
    mutate(nzcc_unscaled=nzcc,
           nzcc=nzcc_unscaled / constraint_scales[i])
  
  inputs <- list()
  inputs$p <- 2
  inputs$wt <- pufbase_state %>% filter(AGI_STUB==stub) %>% arrange(RECID) %>% .$weight_state
  inputs$RECID <- pufbase_state %>% filter(AGI_STUB==stub) %>% arrange(RECID) %>% .$RECID
  inputs$constraint_coefficients_sparse <- nzcc_stub
  inputs$n_variables <- length(inputs$wt)
  inputs$n_constraints <- length(constraints)
  inputs$objscale <- 1e6
  inputs$constraint_scales <- constraint_scales
  
  xlb <- rep(0, inputs$n_variables)
  xub <- rep(100, inputs$n_variables)
  x0 <- rep(1, inputs$n_variables)
  
  tol <- tol_df %>% filter(AGI_STUB==stub) %>% .$tol_default
  
  clb <- constraints - abs(constraints) * tol
  cub <- constraints + abs(constraints) * tol
  
  eval_jac_g_structure <- define_jac_g_structure_sparse2(inputs$constraint_coefficients_sparse, ivar="i", jvar="j")
  eval_h_structure <- lapply(1:inputs$n_variables, function(x) x) # diagonal elements of our Hessian
  
  # ma86 was faster in one test I did
  opts <- list("print_level" = 0,
               "file_print_level" = 5, # integer
               "linear_solver" = "ma86", # mumps pardiso ma27 ma57 ma77 ma86 ma97
               "max_iter"= 100,
               "output_file" = paste0("results/logs/stub_", str_pad(stub, width=2, side="left", pad="0"), ".out"))

  result <- ipoptr(x0 = x0,
                   lb = xlb,
                   ub = xub,
                   eval_f = eval_f_xtop, # arguments: x, inputs
                   eval_grad_f = eval_grad_f_xtop,
                   eval_g = eval_g, # constraints LHS - a vector of values
                   eval_jac_g = eval_jac_g,
                   eval_jac_g_structure = eval_jac_g_structure,
                   eval_h = eval_h_xtop, # the hessian is essential for this problem
                   eval_h_structure = eval_h_structure,
                   constraint_lb = clb,
                   constraint_ub = cub,
                   opts = opts,
                   inputs = inputs)
  saveRDS(result, paste0("results/ipopt_output/ipopt_", str_pad(stub, width=2, side="left", pad="0"), ".rds"))
  print(result$message)
  
  df <- tibble(AGI_STUB=stub, RECID=inputs$RECID, wt_init=inputs$wt, x=result$solution)
  return(df)
}


tmp <- ldply(1:10, runstub, .progress="text")
glimpse(tmp)
count(tmp, AGI_STUB)
ht(tmp)
quantile(tmp$x, probs=c(0, .01, .05, .1, .25, .5, .75, .9, .95, .99, 1))
saveRDS(tmp, here::here("results", "NY_x.rds"))


# compare selected results to targets ----
targets_state
pufbase_state

xvalues <- readRDS(here::here("results", "NY_x.rds"))

comp <- pufbase_state %>%
  left_join(xvalues %>% select(-AGI_STUB), by="RECID") %>%
  mutate(weight_state2=wt_init * x)

checksums <- comp %>%
  group_by(AGI_STUB) %>%
  # amounts in $b
  summarise(nret=sum(weight_state2), agi=sum(c00100 * weight_state2) / 1e9) %>%
  mutate(type="calc")

checktargs <- targets_state %>%
  filter((constraint_name=="A00100" & constraint_type=="amount") |
           str_sub(constraint_name, 1, 4)=="MARS") %>%
  select(AGI_STUB, constraint_name, constraint_type, target) %>%
  mutate(target=ifelse(constraint_type=="amount", target / 1e9, target)) %>%
  group_by(AGI_STUB, constraint_type) %>%
  summarise(target=sum(target)) %>%
  mutate(constraint_type=ifelse(constraint_type=="amount", "agi", "nret")) %>%
  mutate(type="target")

comp2 <- checksums %>%
  pivot_longer(c(agi, nret)) %>%
  bind_rows(checktargs %>% rename(name=constraint_type, value=target)) %>%
  pivot_wider(names_from=type) %>%
  select(name, AGI_STUB, target, calc) %>%
  mutate(diff=calc - target,
         pdiff=diff / target * 100) %>%
  arrange(name, AGI_STUB)

comp2 %>%
  kable(digits=c(0, 0, 1, 1, 1, 1), format.args=list(big.mark=","), format="rst")




# old ----




# constraint coefficients
cc <- puf2017_stateweights1 %>%
  filter(AGI_STUB==stub) %>%
  select(AGI_STUB, stabbr, wtst_2017, c00100, E00200) %>%
  mutate(N1=wtst_2017, A00100=c00100 * wtst_2017, A00200=E00200 * wtst_2017)

# AGI_STUB 1 AGI has -$23.9b target, -$4.6b value on the file!!
# AGI_STUB 2 AGI has $6.7b target, $7.0b value on the file
# AGI_STUB 3 AGI has $34.3b target, $34.2b value on the file
targets
sum(cc$wtst_2017 * cc$c00100)


# reweighting strategy
# get all targets for a range
# see where we are far off
# reset







# we need a way to automatically and selectively tighten or loosen individual targets
xnames <- c("year", "stabbr", "AGI_STUB")
(unames <- setdiff(names(targets), xnames))

colSums(select(cc, unames)) / select(targets, unames)


x <- rep(1, nrow(cc))
x <- rnorm(nrow(cc), 1, .02)
colSums(x * select(cc, unames))

colSums(select(cc, unames))


inputs <- list()
inputs$s <- .1
inputs$concoef <- cc
inputs$constraint_vars <- unames # names of the constraints, in desired order
inputs$n_variables <- nrow(inputs$concoef)
inputs$n_constraints <- length(inputs$constraint_vars)
inputs$cc_dense <- cc_flatten(inputs$concoef, inputs$constraint_vars)

inputs$p <- 2
inputs$wt <- cc$wtst_2017

# 1917015 1574111 ... then
# 2457083 1574110...
# 118 83.5 0.6888... then
# start <- inputs$n_variables*2 + 1
# inputs$cc_dense[start:(start + 5)]

xlb <- rep(0, inputs$n_variables)
xub <- rep(1000, inputs$n_variables)
x0 <- rep(1, inputs$n_variables)

constraints <- targets[, inputs$constraint_vars] %>% as.numeric

clb <- constraints
cub <- constraints

eval_jac_g_structure_dense <- 
  define_jac_g_structure_dense(n_constraints=inputs$n_constraints, 
                               n_variables=inputs$n_variables)
length(eval_jac_g_structure_dense)


eval_h_structure <- lapply(1:inputs$n_variables, function(x) x) # diagonal elements of our Hessian

eval_f_absapprox(x0, inputs)# ; eval_f_absapprox(xlb, inputs); eval_f_absapprox(xub, inputs)
eval_grad_f_absapprox(x0, inputs)
eval_g_dense(x0, inputs)
eval_jac_g_dense(x0, inputs)
eval_h_absapprox(x0, obj_factor=1, hessian_lambda=rep(1, inputs$n_constraints), inputs) # length is n_variables
# length(unlist(eval_h_structure)) # length should be n_variables


# ma86 was faster in one test I did
opts <- list("print_level" = 0,
             "file_print_level" = 5, # integer
             "linear_solver" = "ma86", # mumps pardiso ma27 ma57 ma77 ma86 ma97
             "max_iter"=100,
             # "nlp_scaling_max_gradient" = 100, # 1e-3, # default 100
             # "obj_scaling_factor" = 1e7, # 1e7, # default 1
             # "derivative_test"="first-order",
             # "derivative_test_print_all"="yes",
             "output_file" = "temp5.out")

result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_absapprox, # arguments: x, inputs
                 eval_grad_f = eval_grad_f_absapprox,
                 eval_g = eval_g_dense, # constraints LHS - a vector of values
                 eval_jac_g = eval_jac_g_sparse,
                 eval_jac_g_structure = define_jac_g_structure_sparse(inputs),
                 eval_h = eval_h_absapprox, # the hessian is essential for this problem
                 eval_h_structure = eval_h_structure,
                 constraint_lb = clb,
                 constraint_ub = cub,
                 opts = opts,
                 inputs = inputs)


result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_xtop, # arguments: x, inputs
                 eval_grad_f = eval_grad_f_xtop,
                 eval_g = eval_g_dense, # constraints LHS - a vector of values
                 eval_jac_g = eval_jac_g_dense,
                 eval_jac_g_structure = define_jac_g_structure_dense(inputs$n_constraints, inputs$n_variables),
                 eval_h = eval_h_xtop, # the hessian is essential for this problem
                 eval_h_structure = eval_h_structure,
                 constraint_lb = clb,
                 constraint_ub = cub,
                 opts = opts,
                 inputs = inputs)

result <- ipoptr(x0 = x0,
                 lb = xlb,
                 ub = xub,
                 eval_f = eval_f_xm1sq, # arguments: x, inputs
                 eval_grad_f = eval_grad_f_xm1sq,
                 eval_g = eval_g_dense, # constraints LHS - a vector of values
                 eval_jac_g = eval_jac_g_dense,
                 eval_jac_g_structure = define_jac_g_structure_dense(inputs$n_constraints, inputs$n_variables),
                 eval_h = eval_h_xm1sq, # the hessian is essential for this problem
                 eval_h_structure = eval_h_structure,
                 constraint_lb = clb,
                 constraint_ub = cub,
                 opts = opts,
                 inputs = inputs)


# result <- ipoptr(x0 = x0,
#                  lb = xlb,
#                  ub = xub,
#                  eval_f = eval_f_xtop, # arguments: x, inputs
#                  eval_grad_f = eval_grad_f_xtop,
#                  eval_g = eval_g_dense, # constraints LHS - a vector of values
#                  eval_jac_g = eval_jac_g_sparse,
#                  eval_jac_g_structure = define_jac_g_structure_sparse(inputs),
#                  eval_h = eval_h_xtop, # the hessian is essential for this problem
#                  eval_h_structure = eval_h_structure,
#                  constraint_lb = clb,
#                  constraint_ub = cub,
#                  opts = opts,
#                  inputs = inputs)


str(result)

eval_f_absapprox(x0, inputs)# ; eval_f_absapprox(xlb, inputs); eval_f_absapprox(xub, inputs)
eval_f_absapprox(result$solution, inputs)# ; eval_f_absapprox(xlb, inputs); eval_f_absapprox(xub, inputs)

eval_g_dense(x0, inputs)
eval_g_dense(result$solution, inputs)
constraints

# > eval_f_absapprox(x0, inputs)# ; eval_f_absapprox(xlb, inputs); eval_f_absapprox(xub, inputs)
# [1] 219.42
# > eval_f_absapprox(result$solution, inputs)# ; eval_f_absapprox(xlb, inputs); eval_f_absapprox(xub, inputs)
# [1] 375.3975
# 530.7 xm1sq
# 532 xtop

# 2194.2 --> 2242.26 w approx, s=0.1
#            2274 xtop
#            2266 xm1sq
