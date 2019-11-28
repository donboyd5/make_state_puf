
#****************************************************************************************************
#  Wrapper function to call optimizers with uniform interface and return value ####
#****************************************************************************************************

opt_wrapper <- function(method, wts0, data, constraints, objfn=NULL, ...){
  #.. opt_wrapper - wrapper for alternative optimization methods ----
  
  # call any of the methods with a single uniform interface and get a uniform return value (prior name uni_opt)
  # Arguments:
  #   method: character, any of c("simann", "mma", "calibrate")
  #   wts0:   numeric vector of initial weights to be used in the optimization
  #   data:   data frame
  #   constraints: 
  
  # Return:
  # A list with one or more of the following elements
  #   method
  #   obj      objective function value<- objfn(weights(calib_result), data, constraints)
  #   weights  weights computed under the new method
  #   iter     number of interations, if relevant
  #   elapsed_seconds elapsed time
  #   opt_object 
  #   notes 
  
  methods <- c("calibrate", "mma", "simann", "mnl")
  if(!method %in% methods){
    err_msg1 <- paste0("STOP: Method ", method, " not supported. Method must be one of:")
    err_msg2 <- paste(methods, collapse=", ")
    print(err_msg1)
    print(err_msg2)
    return(NULL)
  }
  
  
  args <- list(...) # only gets the dots values; match.call() gets all names, but not the values
  if(hasArg(maxiter)) maxiter <- args$maxiter else maxiter <- 1000 # always define even if not needed
  
  if(method=="calibrate"){
    a <- proc.time()
    calib_result <- calibrate_nloptr(wts0, data, constraints)
    b <- proc.time()
    
    result <- list()
    result$method <- method
    result$obj <- objfn(weights(calib_result), data, constraints)
    result$weights <- unname(weights(calib_result))
    result$iter <- 0
    result$elapsed_seconds <- unname((b - a)[3])
    result$opt_object <- NULL
    result$notes <- "(1) obj is NOT from the calibrate function, it is the general objfn; (2) result object not returned - too big"
    
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
    
  } else if(method=="simann"){
    set.seed(1234)
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

    
  } else if(method=="mnl"){
    # CAUTION: This is a quick and dirty effort -- mnldata must exist!, frm is fixed
    a <- proc.time()
    frm <- stabbr ~ pincp + wagp + intp + retp + ssip + ssp +
      sex + agep + mar + mar*agep + sex*agep
    
    mnl_result <- multinom(frm,
                           data=mnldata,
                           model=FALSE, # return the model that is estimated
                           MaxNWts=3000, # to accommodate larger models
                           maxit=1000)
    b <- proc.time()
    
    # get weights, etc.
    wtsdf_mnl <- mnldata %>%
      mutate(prob_ny=mnl_result$fitted.values[, "NY"],
             wts_mnl=pwgtp * prob_ny) %>%
      filter(incgroup==ygrp) %>%
      select(serialno, sporder, stabbr,pwgtp, prob_ny, wts_mnl)
    
    wts_mnl <- wtsdf_mnl$wts_mnl
    
    result <- list()
    result$method <- method
    result$obj <- objfn(wts_mnl, data, constraints)
    result$weights <- wts_mnl
    result$iter <- 0
    result$elapsed_seconds <- unname((b - a)[3])
    result$opt_object <- NULL
    result$notes <- "obj is NOT minimized in this approach"
  }
  return(result)
}



#****************************************************************************************************
#  Functions to call an individual optimizer ####
#****************************************************************************************************
# data <- sampdata; constraints <- consdata; weights <- wts0
calibrate_nloptr <- function(weights, data, constraints){
  #.. calibrate -- from the survey package ----
  require("survey")
  # id = ~0 means no clusters
  sdo <- svydesign(id = ~0, weights = weights, data = data) # create a survey design object
  
  # create a named vector of constraints from the contstraints data frame
  # pop_totals <- constraints %>% 
  #   gather(vname, value) %>%
  #   deframe 
  pop_totals <- constraints %>% 
    pivot_longer(cols=names(.), names_to="vname", values_to="value") %>%
    deframe
  
  # pop_totals <- pop_totals[-1] # djb
  
  # create a formula to be used in calibrate
  frm_string <- paste0("~ ", paste(names(pop_totals), collapse=" + "), " - 1") # -1 means no intercept
  frm <- as.formula(frm_string)
  
  # note that logit calibration requires finite bounds
  fn <- c("linear", "raking", "logit")
  calibrated <- calibrate(sdo, formula=frm, population=pop_totals, calfun=fn[1], eta=weights, sparse=TRUE, force=TRUE)
  
  return(calibrated)
}


mma_nloptr <- function(objfn, wts0, sampdata, consdata, niter=1000){
  #.. mma -- from the nloptr package ----
  require("nloptr")
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



multinom_nnet <- function(objfn, wts0, sampdata, consdata, niter=1000){
  # we don't need most of the input arguments I include them for a quick and dirty function
  
  mnldata <- samp_clean %>% 
    mutate(stabbr=as.character(stabbr),  # character to remove levels of states not in sample
           sex=as.factor(sex), agep=as.factor(agep), mar=as.factor(mar)) %>% # just to be sure these are treated as factors
    mutate_at(vars(pincp, wagp, intp, retp, ssip, ssp), list(~ (. - min(.)) / (max(.) - min(.)))) # scale continuous vars to [0, 1]
  
  # the better the model, using all data we have available in our state-labeled data that are also in our
  # sample data, the better the predictions will be
  
  frm <- stabbr ~ pincp + wagp + intp + retp + ssip + ssp +
    sex + agep + mar + mar*agep + sex*agep
  
  mnl <- multinom(frm,
                  data=mnldata,
                  model=FALSE, # return the model that is estimated?
                  MaxNWts=3000,
                  maxit=niter)
  return(mnl)
}





