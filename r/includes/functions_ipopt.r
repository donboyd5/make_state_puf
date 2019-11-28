

#****************************************************************************************************
#                Fast function to get the sparsity structure of a matrix ####
#****************************************************************************************************

make.sparse.structure <- function(A) {
  # return a list with indexes for nonzero constraint coefficients
  # this is much faster than the make.sparse function in ipoptr
  f <- function(x) which(x!=0, arr.ind=TRUE)
  rownames(A) <- NULL # just to be safe
  S <- apply(A, 1, f)
  S <- as.list(S)
  return(S)
}


#****************************************************************************************************
#                constraint evaluation and coefficient functions for ipoptr -- same for all ####
#****************************************************************************************************
eval_g <- function(x, inputs) {
  # constraints that must hold in the solution - just give the LHS of the expression
  # return a vector where each element evaluates a constraint (i.e., sum of (x * a ccmat column), for each column)
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs$constraint.coefficients.sparse has the fixed constraint coefficients in sparse form in a dataframe that has:
  #   i -- the FEASIBLE constraint number
  #   target.num -- constraint number (among all constraints -- simply an identifier)
  #   j -- index into x (i.e., the variable number)
  #   nzcc
  
  constraints <- inputs$constraint.coefficients.sparse %>%
    group_by(i, target.num) %>%
    summarise(constraint.value=sum(nzcc * x[j]))
  
  return(constraints$constraint.value)
}


eval_jac_g <- function(x, inputs){
  # the Jacobian is the matrix of first partial derivatives of constraints (these derivatives may be constants)
  # this function evaluates the Jacobian at point x
  
  # return: a vector where each element gives a NONZERO partial derivative of constraints wrt change in x
  # so that the first m items are the derivs with respect to each element of first column of ccmat
  # and next m items are derivs with respect to 2nd column of ccmat, and so on
  # so that it returns a vector with length=nrows x ncolumns in ccmat
  
  # because constraints in this problem are linear, the derivatives are all constants
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  return(inputs$constraint.coefficients.sparse$nzcc)
}


#****************************************************************************************************
#                x^p + x^-p {xtop} -- functions for ipoptr ####
#****************************************************************************************************
eval_f_xtop <- function(x, inputs) {
  # objective function - evaluates to a single number
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  # here are the objective function, the 1st deriv, and the 2nd deriv
  # http://www.derivative-calculator.net/
  # w{x^p + x^(-p) - 2}                                 objective function
  # w{px^(p-1) - px^(-p-1)}                             first deriv
  # p*w*x^(-p-2)*((p-1)*x^(2*p)+p+1)                    second deriv
  
  # make it easier to read:
  p <- inputs$p
  w <- inputs$wt
  
  obj <- sum(w * {x^p + x^(-p) -2})
  
  return(obj)
}


eval_grad_f_xtop <- function(x, inputs){
  # gradient of objective function - a vector length x 
  # giving the partial derivatives of obj wrt each x[i]
  
  # ipoptr requires that ALL functions receive the same arguments, so I pass the inputs list to ALL functions
  
  # http://www.derivative-calculator.net/
  # w{x^p + x^(-p) - 2}                                 objective function
  # w{px^(p-1) - px^(-p-1)}                             first deriv
  # p*w*x^(-p-2)*((p-1)*x^(2*p)+p+1)                    second deriv
  
  # make it easier to read:
  p <- inputs$p
  w <- inputs$wt
  
  gradf <- w * (p * x^(p-1) - p * x^(-p-1))
  
  return(gradf)
}


eval_h_xtop <- function(x, obj_factor, hessian_lambda, inputs){
  # The Hessian matrix has many zero elements and so we set it up as a sparse matrix
  # We only keep the (potentially) non-zero values that run along the diagonal.
  
  # http://www.derivative-calculator.net/
  # w{x^p + x^(-p) - 2}                                 objective function
  # w{px^(p-1) - px^(-p-1)}                             first deriv
  # p*w*x^(-p-2)*((p-1)*x^(2*p)+p+1)                    second deriv
  
  # make it easier to read:
  p <- inputs$p
  w <- inputs$wt
  
  hess <- obj_factor * 
    { p*w*x^(-p-2) * ((p-1)*x^(2*p)+p+1) }
  
  return(hess)
}



#****************************************************************************************************
#                (x - 1)^2 {xm1sq} -- functions for ipoptr ####
#****************************************************************************************************
eval_f_xm1sq <- function(x, inputs) {
  # objective function - evaluates to a single number
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  # here are the objective function, the 1st deriv, and the 2nd deriv
  # http://www.derivative-calculator.net/
  # w*(x-1)^2                  objective function
  # 2*w*(x-1)                  first deriv
  # 2*w                        second deriv
  
  # make it easier to read:
  w <- inputs$wt
  
  obj <- sum(w * (x-1)^2)
  
  return(obj)
}


eval_grad_f_xm1sq <- function(x, inputs){
  # gradient of objective function - a vector length x 
  # giving the partial derivatives of obj wrt each x[i]
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # http://www.derivative-calculator.net/
  # w*(x-1)^2                  objective function
  # 2*w*(x-1)                  first deriv
  # 2*w                        second deriv
  
  # make it easier to read:
  w <- inputs$wt
  
  gradf <- 2 * w * (x-1)
  
  return(gradf)
}


eval_h_xm1sq <- function(x, obj_factor, hessian_lambda, inputs){
  # The Hessian matrix has many zero elements and so we set it up as a sparse matrix
  # We only keep the (potentially) non-zero values that run along the diagonal.
  
  # http://www.derivative-calculator.net/
  # w*(x-1)^2                  objective function
  # 2*w*(x-1)                  first deriv
  # 2*w                        second deriv
  
  # make it easier to read:
  w <- inputs$wt
  
  hess <- obj_factor * 2 * w
  
  return(hess)
}




