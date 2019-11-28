

#******************************************************************************************************************
#  helper functions ####
#******************************************************************************************************************

n.sum <- function(df, var, weight, condition=TRUE){
  # get the weighted number of records for which a logical condition related to the variable is met
  # var is a numeric column in df
  # weight is a vector of weights (could be a column in df, or could be external)
  # condition: boolean expression as text
  # returns a scalar which is the weighted number of records
  condition <- parse(text=condition)
  df %>%
    dplyr::select(variable=!!var) %>%
    summarise(value=sum(weight * eval(condition))) %>%
    .[[1]]
}


val.sum <- function(df, var, weight, condition=TRUE){
  # get the weighted value of a variable for which a logical condition related to the variable is met
  # var is a numeric column in df
  # weight is a vector of weights (could be a column in df, or could be external)
  # condition: boolean expression as text
  # returns a scalar which is the weighted value of the variable
  condition <- parse(text=condition)
  df %>% 
    dplyr::select(variable=!!var) %>%
    summarise(value=sum(variable * weight * eval(condition))) %>%
    .[[1]]
}


n.pos <- function(df, var, weight){
  n.sum(df, var, weight, condition="variable>0")
}

n.neg <- function(df, var, weight){
  n.sum(df, var, weight, condition="variable<0")
}


val.pos <- function(df, var, weight){
  val.sum(df, var, weight, condition="variable>0")
}

val.neg <- function(df, var, weight){
  val.sum(df, var, weight, condition="variable<0")
}

# val.pos(puf.full, vlist[6], puf.full$wt)
# val.neg(puf.full, vlist[6], puf.full$wt)



#******************************************************************************************************************
#  objective and gradient functions for the weight from-scratch (unconstrained) approach -- shorthand is wtfs ####
#******************************************************************************************************************
eval_f_wtfs <- function(w, inputs) {
  # objective function - evaluates to a single number
  
  # w is the vector of weights computed by ipopt in this iteration
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs$recipe is a data frame where each row defines an element of the objective function, with columns:
  #   obj.element -- a short name for the element, giving the variable involved and the function to be applied to it
  #   var -- the variable involved -- e.g., c00100 or e00200
  #   fn -- the function to be applied -- one of n.sum, val.sum, n.pos, val.pos, n.neg, val.neg
  #   multiplier -- a weighting factor that can make this element more important than other elements
  #     default is 1; larger numbers make an element more important
  #   target -- the value for this calculation as applied to the target puf file
  
  # inputs$synsub is a dataframe that is a subset of the synfile that only includes the columns needed to
  #   calculate the objective function. 
  #     The wt column is special in that its value is set to 1, to make
  #     calculations involving a weight variable easy. (Multiplying this iteration's weight vector, w, by
  #     the synsub$wt weight variable (i.e., 1), gives the tentative weight for the file. The sum of this is
  #     of course the sum of weights. THis allows us to treat the weight like it is any other variable.)
  
  # obj -- calculated below is the objective function value; it is the sum across all elements of the:
  #   priority-weighted 
  #     squared differences of the 
  #       sum of values calculated for an element using the weight vector from this iteration, minus the
  #         corresponding target value in the puf calculated  using its weights
  # ratio <- function(target.calc, target) ifelse(target==0, 1, target.calc / target)
  
  # for serial
  # objdf <- inputs$recipe %>%
  #   rowwise() %>%
  #   # the mutate step applies this element's function to this element's variable, using this iteration's weights:
  #   mutate(target.calc=do.call(fn, list(inputs$synsub, vname, w))) %>% 
  #   ungroup %>%
  #   mutate(obj=priority.weight * {(target.calc / scale - target / scale)^2})
  
  # for parallel
  # apply this element's function to this element's variable, using this iteration's weights:
  objdf <- inputs$recipe
  objdf$target.calc <- NA_real_
  for(i in 1:nrow(objdf)){
    objdf$target.calc[i] <- get(objdf$fn[i])(df=inputs$synsub, var=objdf$vname[i], w)
  }
  
  objdf <- objdf %>%
    mutate(obj=priority.weight * {(target.calc / scale - target / scale)^2})
  
  obj <- sum(objdf$obj)
  
  return(obj)
}


eval_grad_f_wtfs <- function(w, inputs){
  # gradient of objective function - a vector length w
  # giving the partial derivatives of obj wrt each w[i]
  
  # ipoptr requires that ALL functions receive the same arguments, so the inputs list is passed to ALL functions
  
  # inputs will include:
  #   for each target element in the objective function:
  #     the sum -- s1 for first target element, and so on, and
  #     the multiplier vector (e.g., wages, cap gains, etc.)
  
  # http://www.derivative-calculator.net/
  # https://www.symbolab.com/solver/partial-derivative-calculator
  # Example where we only have one target element in the objective function, and only have
  #   3 records and therefore 3 weights, and need to return a vector of 3 partial derivatives
  # Notation: w1, w2, and w3 are the first 3 weights in the vector of weights
  #           a1, a2, and a3 are the 3 constants they are multiplied by (e.g., wages1, wages2, wages3)
  #           t is the sum or target we are comparing to
  #           p is the priority weight of this element of the objective function
  #           calc is the calculated value of the target with the new weights  
  # The objective function to minimize is p*(w1*a1 + w2*a2 + w3*a3 - s)^2
  # The first partial derivatives of the objective function are:
  #   wrt w1:          2*a1*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a1*p*(calc - t)
  #   wrt w2:          2*a2*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a2*p*(calc - t)
  #   wrt w3:          2*a3*p*(w1*a1 + w2*a2 + w3*a3 - t) = 2*a3*p*(calc - t)
  # If we have multiple target elements, each vector element will have a more complex sum
  
  wdf <- dplyr::tibble(wt.iter=w, wtnum=1:length(w))
  
  grad <- inputs$coeffs %>% 
    dplyr::left_join(wdf, by="wtnum") %>%
    dplyr::group_by(obj.element) %>%
    dplyr::mutate(calc=sum(wt.iter*coeff)) %>%
    dplyr::mutate(grad={2 * coeff * priority.weight * (calc - target)} / {scale^2}) %>%
    dplyr::group_by(wtnum) %>%
    dplyr::summarise(grad=sum(grad)) %>% # sum across all of the object elements for this particular weight
    ungroup
  
  return(grad$grad)
}


