
library(sms)
census
survey


schaffer <- function(xx) {
  x1 <- xx[1]
  x2 <- xx[2]
  fact1 <- (sin(x1^2-x2^2))^2 - 0.5
  fact2 <- (1 + 0.001*(x1^2+x2^2))^2
  y <- 0.5 + fact1/fact2
  return(y)
}

(s <- rnorm(2, c(0, 2), 1))
schaffer(s)


f <- function(recs){
  recs[1]^2 + recs[2]
}


sa <- function(objfn, s0, niter = 10, step = 0.1) {
  
  # Initialize
  ## s stands for state
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  s_b <- s_c <- s_n <- s0
  f_b <- f_c <- f_n <- objfn(s_n)
  message("It\tBest\tCurrent\tNeigh\tTemp")
  message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", 0L, f_b, f_c, f_n, 1))
  
  for (k in 1:niter) {     
    Temp <- (1 - step)^k # .9, .81, .729, .656, .590,... see (1-.1)^(1:10)
    # consider a random neighbor
    s_n <- rnorm(2, s_c, 1) # get 2 random normal values from distribution -- 1 with mean 2, 1 with mean 2, both sd1, for c(0, 2)
    f_n <- objfn(s_n)
    # update current state
    if (f_n < f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) { # we have a new better result so keep it
      s_c <- s_n
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      s_b <- s_n
      f_b <- f_n         
    }
    message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
  }
  return(list(iterations = niter, best_value = f_b, best_state = s_b))
}

(1-.1)^(1:10)

set.seed(1234)
rnorm(n=2, mean=c(0, 2), sd=c(0, 10))

sa(schaffer, s0 = c(0, 2))
sa(f, s0 = c(0, 2), niter=1000)


sol <- simulated_annealing(schaffer, s0 = c(0, 2))
#  It   Best    Current Neigh   Temp
#  0    0.5722  0.5722  0.5722  1.0000
#  1    0.1054  0.1054  0.1054  0.9000
#  2    0.1054  0.1922  0.1922  0.8100
#  3    0.1054  0.1922  0.3579  0.7290
#  4    0.1054  0.1922  0.9059  0.6561
#  5    0.1054  0.6428  0.6428  0.5905
#  6    0.1054  0.8893  0.8893  0.5314
#  7    0.1054  0.1597  0.1597  0.4783
#  8    0.1054  0.6489  0.6489  0.4305
#  9    0.1054  0.7535  0.7535  0.3874
#  10   0.1054  0.2236  0.2236  0.3487

sol

# knapsack problem ----
wmax <- 20
w <- c(5, 4, 6, 3, 2, 6, 7) # weights of objects
v <- c(50, 40, 30, 50, 30, 24, 36) # values of objects
# max value subject to weights not > wmax

fv <- function(is){
  v <- c(50, 40, 30, 50, 30, 24, 36)
  sum(v[is])
}
fv(1:5)
s0 <- 2:6
objfn <- fv
niter <- 10
step <- .01

sa <- function(objfn, s0, niter = 10, step = 0.1) {
  # Initialize
  ## s stands for state
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  s_b <- s_c <- s_n <- s0
  f_b <- f_c <- f_n <- objfn(s_n)
  message("It\tBest\tCurrent\tNeigh\tTemp")
  message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", 0L, f_b, f_c, f_n, 1))
  
  for (k in 1:niter) {     
    Temp <- (1 - step)^k # .9, .81, .729, .656, .590,... see (1-.1)^(1:10)
    # consider a random neighbor
    s_n <- sample(1:length(s0), 1) # get 1 random value
    f_n <- objfn(s_n)
    # update current state
    if (f_n < f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) { # we have a new better result so keep it
      s_c <- s_n
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      s_b <- s_n
      f_b <- f_n         
    }
    message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
  }
  return(list(iterations = niter, best_value = f_b, best_state = s_b))
}


# djb ----
# simple world
con_age <- read_csv(file.path(PROJHOME, "play", "age.csv"))
con_sex <- read_csv(file.path(PROJHOME, "play", "sex.csv"))
(cons2 <- cbind(con_age, con_sex))

ind <- read_csv(file.path(PROJHOME, "play", "ind-full.csv"))
ind
(ind$age_cat <- cut(ind$age, breaks = c(0, 50, 100), labels=c("age_0_50", "age_50_100")))
ind
recs <- ind %>%
  mutate(v=1) %>%
  spread(age_cat, v, fill=0)
recs


f <- function(weights, cons=100, recs.in=recs){
  df <- recs.in %>%
    summarise_at(vars(starts_with("age_")), ~sum(weights * .))
  error=(df$age_0_50 - 30)^2 + (df$age_50_100 - 70)^2
  error
}

f(weights=c(20, 30, 40, 0, 0))
f(weights=c(20, 30, 20, 20, 0))
f(weights=rep(20, 5))
f(weights=rep(100, 5))

sa <- function(objfn, x0, niter = 10, step = 0.1) {
  # Initialize
  ## x stands for x -- the set of weights to assign to the records in the survey
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  x_b <- x_c <- x_n <- x0
  f_b <- f_c <- f_n <- objfn(x_n)
  print(f_n)
  for (k in 1:niter) {
    # consider a random neighbor
    irec <- sample(1:length(x0), 1) # get 1 random index into the survey
    wt <- runif(1, 0, 100)
    x_n[irec] <- wt
    f_n <- objfn(x_n)
    # update current state
    # if (f_n < f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) { # we have a new better result so keep it
    if (f_n < f_c) { # we have a new better result so keep it
      x_c <- x_n
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      x_b <- x_n
      f_b <- f_n         
    }
    # message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
    if(k %% 2000 ==0 || k==niter) print(c(f_b, x_b))
  }
  return(list(f_b=f_b, x_b=x_b))
}

sa2 <- function(objfn, x0, niter = 10, step = 0.1) {
  # Initialize
  ## x stands for x -- the set of weights to assign to the records in the survey
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  x_b <- x_c <- x_n <- x0
  f_b <- f_c <- f_n <- objfn(x_n)
  print(f_n)
  for (k in 1:niter) {
    # consider a random neighbor
    irec <- sample(1:length(x0), 1) # get 1 random index into the survey
    wt <- runif(1, 0, 100)
    x_n[irec] <- wt
    f_n <- objfn(x_n)
    # update current state
    # if (f_n < f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) { # we have a new better result so keep it
    if (f_n < f_c) { # we have a new better result so keep it
      x_c <- x_n
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      x_b <- x_n
      f_b <- f_n         
    }
    # message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
    if(k %% 200 ==0 || k==niter) print(c(f_b, sum(x_b), x_b))
  }
  return(list(f_b=f_b, x_b=x_b))
}


tmp <- sa2(f, rep(100, 5), niter=3000)
tmp <- sa2(f, rep(1, 5), niter=3000)
tmp
f(tmp$x_b)
sum(tmp$x_b)

recs %>%
  summarise_at(vars(starts_with("age_")), ~sum(tmp$x_b * .))



sa <- function(objfn, s0, niter = 10, step = 0.1) {
  
  # Initialize
  ## s stands for state
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  s_b <- s_c <- s_n <- s0
  f_b <- f_c <- f_n <- objfn(s_n)
  message("It\tBest\tCurrent\tNeigh\tTemp")
  message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", 0L, f_b, f_c, f_n, 1))
  
  for (k in 1:niter) {     
    Temp <- (1 - step)^k # .9, .81, .729, .656, .590,... see (1-.1)^(1:10)
    # consider a random neighbor
    s_n <- rnorm(2, s_c, 1) # get 2 random normal values from distribution -- 1 with mean 2, 1 with mean 2, both sd1, for c(0, 2)
    f_n <- objfn(s_n)
    # update current state
    if (f_n < f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) { # we have a new better result so keep it
      s_c <- s_n
      f_c <- f_n
    }
    # update best state
    if (f_n < f_b) {
      s_b <- s_n
      f_b <- f_n         
    }
    message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
  }
  return(list(iterations = niter, best_value = f_b, best_state = s_b))
}




# djb ----
# http://eprints.ncrm.ac.uk/2236/1/1.html
# djb This is really useful ----

# Problem definition (applies to all methods)
# i indexes zones, N(Z) total zones
# j indexes members of sample, N(S) total members
# k indexes attributes, N(a) total attributes
# cjk is the characteristic of attribute k in the jth member of the population sample
# wij is weight of person j in zone i
# Tik-m is a constraint table (targets) where m is the cell -- the # of people with characteristic k in zone i
#   it has observed values (from the census) and calculated values from the model
# Tik-m(modeled)=sumj[wij * δjk-m] where δjk-m is 1 if cjk = m, and 0 otherwise. 

# In order to compare the modelled and observed distributions, a variety of goodness of fit measures could be employed,
# but typically the most useful will be the total absolute error statistic:
#  TAE = sumk[abs(Tik-m(modeled) - Tik-m(observed))]


# initialize
# assign weight of 1 to every person, for every zone - all wij=1



# n is the new category value.
# o is the old category value.
# To is the total population observed for this geographical zone.
# Tc is the total population calculated from the Census extract.


#****************************************************************************************************
#                best I think ####
#****************************************************************************************************

# Tanton, Robert, Paul Williamson, and Ann Harding. 
# “Comparing Two Methods of Reweighting a Survey File to Small Area Data.” 
# International Journal of Microsimulation 7, no. 1 (2014). https://www.microsimulation.org/IJM/V7_1/4_IJM_7_1_Tanton_Williamson_Harding.pdf.

# The Combinatorial Optimisation algorithm, as currently implemented, may be viewed as an
# integer reweighting algorithm. For each household in an SLA (as recorded in the Census
# benchmark counts), CO randomly selects one household (with replacement) from the survey
# dataset. This is equivalent to setting all survey household weights to 0, then incrementing
# household weights at random by a count of 1 until the sum of weighted households matches the
# equivalent benchmark count. In each subsequent iteration the weight of one survey household is
# randomly increased by one, whilst the weight of another (non-zero weighted) household is
# randomly decreased by one. This is equivalent to randomly swapping households in and out of
# the set (combination) of households currently selected to represent the SLA. If the change in
# weights leads to improved fit, the change is retained; a few adverse changes in weights are also
# accepted, with a probability that diminishes in proportion to (i) size of the adverse impact and (ii)
# number of iterations, in order to avoid getting stuck in a local sub-optima; otherwise the change
# is rejected and the weights are reverted to their previous values. CO will continue to iterate until
# either a minimum fit threshold is achieved, or until a maximum number of user-specified
# iterations has been exceeded (5 million for the results reported in this paper). Full details of the
# algorithm and its links to simulated annealing are published in an article by Williamson et al.
# (Williamson, et al., 1998). The user manual for the publicly available fortran-based version of the
# CO program has been published by Paul Williamson (Williamson, 2007), whilst more recently a
# version of CO has been made available via the R package ‘sms’ (Kavroudakis, 2013).

# define some data
# simple world
# constraints from census
con_age <- read_csv(file.path(PROJHOME, "play", "age.csv"))
con_sex <- read_csv(file.path(PROJHOME, "play", "sex.csv"))
(cons <- cbind(con_age, con_sex))
# put the data in form I will use
cons2 <- cons %>%
  mutate(area=row_number()) %>%
  rename(age0to49_n=a0.49,
         age50plus_n=`a.50+`,
         male_n=m,
         female_n=f) %>%
  select(area, everything())
cons2

ind <- read_csv(file.path(PROJHOME, "play", "ind-full.csv"))
glimpse(ind)
ind
ind2 <- ind %>%
  mutate(age_cat=cut(ind$age, breaks = c(0, 50, 100), labels=c("age0to49_n", "age50plus_n")),
         sex_cat=factor(sex, levels=c("m", "f"), labels=c("male_n", "female_n")),
         count=1) %>%
  gather(variable, constraint_name, sex_cat, age_cat) %>%
  select(-variable) %>%
  spread(constraint_name, count, fill=0)
ind2
  

objfn <- function(weights, constraints, recs){
  # df <- recs.in %>%
  #   summarise_at(vars(starts_with("age_")), ~sum(weights * .))
  # error=(df$age_0_50 - 30)^2 + (df$age_50_100 - 70)^2
  # error
  calculated_constraints <- colSums(weights * recs)
  sqdiff <- (calculated_constraints - constraints)^2
  return(sum(sqdiff))
}

# weights <- c(20, 30, 40, 0, 0)
# recs <- ind2[, -c(1:4)]
# truec <- cons2[1, -1]
# cc <- colSums(weights * recs)
# (cc - truec)^2
# sum((cc - truec)^2)

wts <- c(20, 30, 40, 0, 0)
wts <- c(20, 30, 20, 20, 0)
wts <- rep(20, 5)
wts <- rep(100, 5)
wts <- rep(0, 5) 

objfn(weights=wts, constraints=cons2[1, -1], recs=ind2[, -c(1:4)])

# create initial weights
need <- 12 # this is the grand population total in the area of interest
i <- sample(x=1:nrow(ind2), size=need, replace=TRUE) # these are the indexes that will be filled
wts0 <- as.vector(table(i))
wts0


icheck <- c(1, 0, 3, 4, 0, 7)
nzi # get vector of non-zero indexes
which(icheck > 0)


objfn(weights=wts0, constraints=cons2[1, -1], recs=ind2[, -c(1:4)])


sa <- function(objfn, wts0, niter = 5, step = 0.1, constraints=cons2[1, -1], recs=ind2[, -c(1:4)]) {
  # define simulated annealing
  
  # Initialize
  wts_best <- wts_current <- wts_new <- wts0
  obj_best <- obj_current <- obj_new <- objfn(wts0, constraints, recs)
  
  for (iter in 1:niter) {     
    temperature <- max((1 - step)^iter, 1e-6) # do not let temperature hit zero or we will have error when we divide by temperature below
    # consider a random alternative person(s)
    nz_indexes <- which(wts_current > 0) # define the universe of people for whom we can reduce weights
    i_reduce <- sample(nz_indexes, 1, replace=TRUE)
    i_increase <- sample(setdiff(1:length(wts0), i_reduce), 1, replace=TRUE) # get index of persons for whom we will increase weights (later maybe get multiples)
    
    wts_new <- wts_current
    wts_new[i_reduce] <- wts_new[i_reduce] - 1
    wts_new[i_increase] <- wts_new[i_increase] + 1 # for now, add 1 to the weight of this person
    obj_new <- objfn(wts_new, constraints, recs)
    
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
  #   message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", k, f_b, f_c, f_n, Temp))
    if(obj_best < 1e-3) break
  }
  return(list(iterations = iter, best_obj = obj_best, best_weights = wts_best))
}

# system.time(result <- sa(objfn, wts0, 10e3)) # wow, 10e3 iterations take 22 seconds -- maybe try Julia -- or rcpp?
# depends on how much longer it takes with real data and real constraints
result <- sa(objfn, wts0, 10e3)
result

sa2 <- function(num){
  result <- sa(objfn, wts0, 10e3)
  result$iterations
}

nits <- sapply(1:1000, sa2) # run the function 1000 times
quantile(nits, probs=c(0, .1, .25, .5, .75, .9, .95, .99,  1)) # worst case was 112 iters, 99% was 49 iters

objfn(weights=wts0, constraints=cons2[1, -1], recs=ind2[, -c(1:4)])
objfn(weights=result$best_weights, constraints=cons2[1, -1], recs=ind2[, -c(1:4)])


#****************************************************************************************************
#                apply best to the sms data, which is bigger than simple world ####
#****************************************************************************************************
