
#****************************************************************************************************
#                general utility functions ####
#****************************************************************************************************
ht <- function(df, nrecs=6){
  print(utils::head(df, nrecs))
  print(utils::tail(df, nrecs))
}

naz <- function(vec) {return(ifelse(is.na(vec), 0, vec))} # NAs to zero

ns <- function(df) {names(df) %>% sort}

pdiff <- function(weights, data, constraints) {
  # percent difference between calculated constraints and targets
  calculated_constraints <- calc_constraints(weights, data, names(constraints))
  (calculated_constraints - constraints) / constraints * 100
}


#****************************************************************************************************
#                helper functions for propblem setup ####
#****************************************************************************************************
calc_constraints <- function(weights, data, constraint_vars){
  # weights: numeric vector
  # data: df with 1 column per constraint variable (plus other info); 1 row per person
  # constraint_vars: character vector with names of constraint variables
  colSums(weights * select(data, constraint_vars))
}


objfn <- function(weights, data, constraints, p=2){
  # this objfn scales the data and constraint values used in the obj calculation
  
  calculated_constraints <- calc_constraints(weights, data, names(constraints))
  
  # scale the constraints and the calculations by the constraint values
  diff_to_p <- (calculated_constraints / constraints - constraints / constraints)^p
  
  obj <- sum(diff_to_p)
  return(obj)
}


#****************************************************************************************************
#               simulated annealing ####
#****************************************************************************************************
exp_decay <- function(phase_iter, min_value, max_value, niter){
  # return an integer vector of length niter (number of iterations)
  # declining exponentially as iterations increase
  # nphase is the phase-in period, by which we want values to equal 1
  # can be used for number of records to replace in each iteration
  # or for maximum weights to be added in each iteration
  # a * b^n where
  #   a is max_replace
  #   b is in (0, 1)
  phase_iter <- pmin(round(niter / 2), phase_iter)
  decay <- exp(-log(max_value) / phase_iter) # rate that will hit 1 (before rounding at nphase iterations)
  values <- max_value * decay ^ (1:niter)
  values <- round(pmax(values, min_value))
  return(values)
}


sim_ann <- function(objfn, wts0, data, constraints, p=2, niter = 5, step = 0.1, phase_iter=500, max_recshare=.3, max_wtshare=1) {
  # I wrote this after looking at a few simulated annealing functions on the web. Unfortunately, I do not know
  # right now which one I modeled this after. I'll update this documentation once I find it.
  # However, it is not very fast. If we find that simulated annealing is important to use, we'll either want to find an
  # already-fast function in an R package (I did not find one, and this is faster than what I found), or write a better
  # version.
  
  # simulated annealing with two sets of heuristics:
  # 1) allow more than one record to be replaced at a time, decaying as iterations increase: vector n_reduce
  # 2) allow the weight reduction to decay as iterations increase: vector max_change
  # phase_iter is the number of iterations over which to phase these items down
  
  # max_recshare is the initial share of records that may be replaced - if <=0, all n_reduce values are 1
  # max_wtshare is the initial share of a weight that may be reduced - if <=0, all max_change values are 1
  
  # Initialize
  nrecords <- nrow(data)
  wts_best <- wts_current <- wts_new <- wts0
  obj_best <- obj_current <- obj_new <- objfn(wts0, data, constraints, p)
  
  n_reduce <- rep(1, niter) # number of records for which weight can be reduced
  max_change <- rep(1, niter) # maximum reduction per weight
  
  # compute the exponential phase-down vectors of number of records to reduce, and weight amounts to reduce
  # one element per iteration
  if(max_recshare > 0 & phase_iter > 0) {
    n_reduce <- exp_decay(phase_iter, min_value=1, max_value=max_recshare * nrecords, niter)
  }
  
  if(max_wtshare > 0 & phase_iter > 0) {
    max_change <- exp_decay(phase_iter, min_value=1, max_value=max(wts0), niter)
  }
  
  # fill temperature vector before the loop
  temperature <- pmax((1 - step)^(1:niter), 1e-6) # use pmax so temperature does not hit zero -- causes error when we divide by temperature below
  
  for (iter in 1:niter) {
    if(iter %% 100 == 0) print(paste0("iter #: ", iter, " of ", niter))
    
    # consider random alternative persons
    nz_indexes <- which(wts_current > 0) # define the universe of people for whom we can reduce weights
    n_to_sample <- min(length(nz_indexes), n_reduce[iter]) # djb addition
    i_reduce <- sample(nz_indexes, n_to_sample, replace=FALSE)
    # get indexes of persons for whom we will increase weights
    i_increase <- sample(setdiff(1:length(wts0), i_reduce), n_reduce[iter], replace=FALSE) # setdiff because they must be different people
    # i_increase <- sample(1:length(wts0), n_reduce[iter], replace=TRUE)
    
    # determine weight reduction for each record to be reduced - no more than 1 or the amount of the weight
    wt_reduction <- pmin(wts_current[i_reduce], max_change[iter])
    
    # now we are ready to reduce weights of reduction records, then add weights to the increase records
    wts_new <- wts_current # copy the current weights before replacing
    wts_new[i_reduce] <- wts_new[i_reduce] - wt_reduction
    avg_increase <- sum(wt_reduction) / length(i_increase)
    wts_new[i_increase] <- wts_new[i_increase] +  rep(avg_increase, length(i_increase))
    
    obj_new <- objfn(wts_new, data, constraints, p)
    
    # update current weights
    if ((obj_new < obj_current) ||
        # caution - this next statement will result in error if temperature is allowed to hit zero
        (runif(1, 0, 1) < exp(-(obj_new - obj_current) / temperature[iter]))) { # we have a new better result so keep it
      # if (obj_new < obj_current) { # we have a new better result so keep it
      wts_current <- wts_new
      obj_current <- obj_new
    }
    # update best state
    if (obj_new < obj_best) {
      wts_best <- wts_new
      obj_best <- obj_new
    }
    
    if(obj_best < 1e-8) break
  }
  return(list(iterations = iter, best_obj = obj_best, best_weights = wts_best))
}






