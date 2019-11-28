


ns <- function(df) {names(df) %>% sort}

calc_constraints <- function(weights, data, constraint_vars){
  # weights: numeric vector
  # data: df with 1 column per constraint variable (plus other info); 1 row per person
  colSums(weights * select(data, constraint_vars))
}

pdiff <- function(weights, data, constraints) {
  # percent difference between calculated constraints and targets
  calculated_constraints <- calc_constraints(weights, data, names(constraints))
  (calculated_constraints - constraints) / constraints * 100
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
  # return an integer vector values of length niter (number of iterations)
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

# data <- sampdata
# constraints <- consdata
# niter <- 1000
# iter <- 1
sim_ann <- function(objfn, wts0, data, constraints, p=2, niter = 5, step = 0.1, phase_iter=500, max_recshare=.3, max_wtshare=1) {
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


#****************************************************************************************************
#               mma -- method of moving asymptotes ####
#****************************************************************************************************
mma_nloptr <- function(objfn, wts0, sampdata, consdata, niter=1000){
  inputs <- list()
  inputs$consdata <- consdata
  inputs$sampdata <- sampdata
  inputs$coeffs <- inputs$sampdata %>%
    select(names(inputs$consdata)) %>%
    mutate(wtnum=row_number()) %>%
    gather(obj.element, coeff, -wtnum) %>%
    left_join(inputs$consdata %>% 
                gather(obj.element, scale) %>% 
                mutate(target=scale), 
              by="obj.element")
  
  objfn2 <- function(weights, inputs){
    result <- objfn(weights, inputs$sampdata, inputs$consdata)
    return(result)
  }
  
  grad2 <-  function(w, inputs){
    # gradient of objective function - a vector length w
    # giving the partial derivatives of obj wrt each w[i]
    wdf <- tibble(wt.iter=w, wtnum=1:length(w)) # weights data frame
    
    grad <- inputs$coeffs %>% 
      left_join(wdf, by="wtnum") %>%
      group_by(obj.element) %>%
      mutate(calc=sum(wt.iter*coeff)) %>%
      mutate(grad={2 * coeff * (calc - target)} / {scale^2}) %>%
      group_by(wtnum) %>%
      summarise(grad=sum(grad)) %>% # sum across all of the object elements for this particular weight
      ungroup
    
    return(grad$grad)
  }
  
  mma_result <- mma(wts0, 
                    fn=objfn2, 
                    gr=grad2,
                    lower=rep(0, length(wts0)), upper=NULL,
                    nl.info = FALSE, 
                    control=list(maxeval=niter),
                    inputs=inputs)
  return(mma_result)
}




#****************************************************************************************************
#               calibrate -- from the survey package ####
#****************************************************************************************************

calibrate_reweight <- function(weights, data, constraints){
  # id = ~0 means no clusters
  sdo <- svydesign(id = ~0, weights = weights, data = data)
  
  pop_totals <- constraints %>% 
    gather(vname, value) %>%
    deframe
  
  pop_totals <- pop_totals[-1]
  
  frm_string <- paste0("~ ", paste(names(pop_totals), collapse=" + "), " - 1")
  frm <- as.formula(frm_string)
  
  # note that logit calibration requires finite bounds
  fn <- c("linear","raking","logit")
  calibrated <- calibrate(sdo, formula=frm, population=pop_totals, calfun=fn[1], eta=weights, sparse=TRUE, force=TRUE)
  
  return(calibrated)
}


#****************************************************************************************************
#               OLD items ####
#****************************************************************************************************

sa <- function(objfn, wts0, data, constraints, niter = 5, step = 0.1, p=2) {
  # define simulated annealing
  
  # Initialize
  wts_best <- wts_current <- wts_new <- wts0
  obj_best <- obj_current <- obj_new <- objfn(wts0, data, constraints, p)
  
  for (iter in 1:niter) {     
    temperature <- max((1 - step)^iter, 1e-6) # do not let temperature hit zero or we will have error when we divide by temperature below
    # consider a random alternative person(s)
    nz_indexes <- which(wts_current > 0) # define the universe of people for whom we can reduce weights
    i_reduce <- sample(nz_indexes, 1, replace=TRUE)
    i_increase <- sample(setdiff(1:length(wts0), i_reduce), 1, replace=TRUE) # get index of persons for whom we will increase weights (later maybe get multiples)
    
    
    wt_change <- pmin(wts_current[i_reduce], 1) # don't let the weight reduction exceed the weight to be reduced
    wts_new <- wts_current # copy the current weights before replacing
    wts_new[i_reduce] <- wts_new[i_reduce] - wt_change
    wts_new[i_increase] <- wts_new[i_increase] + wt_change # for now, add to the weight of this person
    obj_new <- objfn(wts_new, data, constraints, p)
    
    # update current weights
    if ((obj_new < obj_current) ||
        # caution - this next statement will result in error if tempreature is allowed to hit zero
        (runif(1, 0, 1) < exp(-(obj_new - obj_current) / temperature))) { # we have a new better result so keep it
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


sa2 <- function(objfn, wts0, data, constraints, niter = 5, step = 0.1, max_fraction=.1, nphase=500, p=2) {
  # define simulated annealing - allowing more than one record to be replaced at a time
  
  # Initialize
  nsamp <- length(wts0)
  
  wts_best <- wts_current <- wts_new <- wts0
  obj_best <- obj_current <- obj_new <- objfn(wts0, data, constraints, p)
  
  # compute the phase-down of number of records to replace - vectorized, computed at start - exponential decay
  f_reduce <- function(nphase, max_fraction, nsamp, niter){
    # return an integer vector n_replace
    # with number of records to replace in each iteration
    # declining exponentially as iterations increase
    # nphase is the phase-in period, by which we want n_replace to equal 1
    # a * b^n where
    #   a is max_replace
    #   b is in (0, 1)
    nphase <- pmin(round(niter / 2), nphase)
    max_replace <- nsamp * max_fraction
    decay <- exp(-log(max_replace) / nphase) # rate that will hit 1 (before rounding at nphase iterations)
    n_reduce <- max_replace * decay ^ (1:niter)
    n_reduce <- round(pmax(n_reduce, 1))
    return(n_reduce)
  }
  n_reduce <- f_reduce(nphase, max_fraction, nsamp, niter)
  
  # fill temperature vector before the loop
  temperature <- pmax((1 - step)^(1:niter), 1e-6) # use max so temperature does not hit zero -- causes error when we divide by temperature below
  #iter <- 1
  for (iter in 1:niter) {
    # consider random alternative persons
    nz_indexes <- which(wts_current > 0) # define the universe of people for whom we can reduce weights
    i_reduce <- sample(nz_indexes, n_reduce[iter], replace=FALSE)
    # get indexes of persons for whom we will increase weights
    i_increase <- sample(setdiff(1:length(wts0), i_reduce), n_reduce[iter], replace=FALSE) # setdiff because they must be different people
    # i_increase <- sample(1:length(wts0), n_reduce[iter], replace=TRUE)
    
    # determine weight reduction for each record to be reduced - no more than 1 or the amount of the weight
    wt_reduction <- pmin(wts_current[i_reduce], 1)
    
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


sa3 <- function(objfn, wts0, data, constraints, niter = 5, step = 0.1, max_fraction=.1, nphase=500, p=2) {
  # define simulated annealing - allowing more than one record to be replaced at a time
  
  # Initialize
  nsamp <- length(wts0)
  
  wts_best <- wts_current <- wts_new <- wts0
  obj_best <- obj_current <- obj_new <- objfn(wts0, data, constraints, p)
  
  # compute the phase-down of number of records to replace - vectorized, computed at start - exponential decay
  f_decay <- function(phase_iter, min_value, max_value, niter){
    # return an integer vector values of length niter (number of iterations)
    # declining exponentially as iterations increase
    # nphase is the phase-in period, by which we want values to equal 1
    # can be used for number of records to replace in each iteration
    # or for maximum weights to be added in each iteration
    # a * b^n where
    #   a is max_replace
    #   b is in (0, 1)
    phase_iter <- pmin(round(niter / 2), phase_iter)
    decay <- exp(-log(max_value) / nphase) # rate that will hit 1 (before rounding at nphase iterations)
    values <- max_value * decay ^ (1:niter)
    values <- round(pmax(values, min_value))
    return(values)
  }
  # nphase <- 200; nsamp <- 1000; max_fraction=.1; niter=3000
  n_reduce <- f_decay(nphase, min_value=1, max_value=max_fraction * nsamp, niter)
  max_change <- f_decay(nphase, min_value=1, max_value=max(wts0), niter)
  
  # fill temperature vector before the loop
  temperature <- pmax((1 - step)^(1:niter), 1e-6) # use max so temperature does not hit zero -- causes error when we divide by temperature below
  #iter <- 1
  for (iter in 1:niter) {
    # consider random alternative persons
    nz_indexes <- which(wts_current > 0) # define the universe of people for whom we can reduce weights
    i_reduce <- sample(nz_indexes, n_reduce[iter], replace=FALSE)
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
