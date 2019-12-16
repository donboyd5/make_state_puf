
target_state <- "NY"

readRDS(here::here("data", "growthratios_2011to2017.rds"))

puf2017_stateweights1 <- readRDS(paste0(globals$statedir, "puf2017_stateweights1.rds"))

targets_full <- readRDS(here::here("data", paste0("hist2_targets", 2017, ".rds"))) %>%
  filter(stabbr==target_state) %>%
  mutate(target=ifelse(str_sub(h2vname, 1, 1)=="A", target * 1000, target))

# create a crosswalk between PUF variable names and h2vnames


# choose a subset of these as targets

# for now let's just look at a subset of the file and a subset of targets and reweight
glimpse(targets_full)

stub <- 3
targets <- targets_full %>%
  filter(AGI_STUB==stub, h2vname %in% c("N1", "A00100", "A00200")) %>%
  select(year, stabbr, AGI_STUB, h2vname, target) %>%
  pivot_wider(names_from=h2vname, values_from = target)

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
