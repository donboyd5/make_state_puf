
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
                     constraint_base_name == "05800" ~ "taxbc",
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
glimpse(pufbase_state)


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
  filter(AGI_STUB==2) %>%
  mutate(table_desc=str_remove(table_desc, "Number of") %>% str_sub(., 1, 35)) %>%
  # note that we do not have taxbc 5800 as a target in AGI_STUB 1 but we do in other stubs
  kable(digits=c(rep(0, 9), 1), format="rst", format.args = list(big.mark=","))

# now we are ready to set up the problem for ipopt

# we need a function to calculate constraints







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
