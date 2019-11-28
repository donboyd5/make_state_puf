


#****************************************************************************************************
#                Libraries and globals ####
#****************************************************************************************************

# source("./r/includes/libraries.r")
source(file.path(PROJHOME, "r/includes", "libraries.r"))



#****************************************************************************************************
#                simple world ####
#****************************************************************************************************

con_age <- read_csv(file.path(PROJHOME, "play", "age.csv"))
con_sex <- read_csv(file.path(PROJHOME, "play", "sex.csv"))
(cons <- cbind(con_age, con_sex))

ind <- read_csv(file.path(PROJHOME, "play", "ind-full.csv"))
ind
(ind$age_cat <- cut(ind$age, breaks = c(0, 50, 100)))
ind

target <- list(as.matrix(con_age[1,]), as.matrix(con_sex[1,]))
str(target) # two matrices, each with column names and no row names

descript <- list(1, 2)
descript

(seed <- table(ind[c("age_cat", "sex")]))
str(seed)

# mipfp
library(mipfp)
res <- Ipfp(seed, descript, target)
res
str(res)
res$x.hat # the result for the first zone
12 * res$p.hat

ObtainModelEstimates(seed, descript, target, method = "ml")
ObtainModelEstimates(seed, descript, target, method = "chi2")
ObtainModelEstimates(seed, descript, target, method = "lsq")


# now ipfp
library(ipfp)
A <- t(cbind(model.matrix(~ ind$age_cat - 1),
             model.matrix(~ ind$sex - 1)[, c(2, 1)]))
A

cons2 <- apply(cons, 2, as.numeric) # convert to numeric data

(weights <- apply(cons2, 1, function(x) ipfp(x, A, x0 = rep(1, 5))))
weights
weights[, 1] # result for the first zone
colSums(weights)


#****************************************************************************************************
#                GREGWT ####
#****************************************************************************************************
devtools::install_github("emunozh/GREGWT")
library(GREGWT)
age <- read_csv(file.path(PROJHOME, "play", "age.csv"))
sex <- read_csv(file.path(PROJHOME, "play", "sex.csv"))
ind <- read_csv(file.path(PROJHOME, "play", "ind-full.csv"))
ind
(ind$age_cat <- cut(ind$age, breaks = c(0, 50, 100)))
(ind$w <- vector(mode = "numeric", length=dim(ind)[1]) + 1)
ind

data_in <- prepareData(census=cbind(age, sex),
                       survey=ind, 
                       census_area_id = FALSE, # area code should be generated
                       breaks = c(2))
data_in

# prepare a data.frame to store the result
fweights <- NULL
Result <- as.data.frame(matrix(NA, ncol=3, nrow=dim(age)[1]))
names(Result) <- c("area", "income", "cap.income")

for(area in seq(dim(age)[1])){
  gregwt = GREGWT(data_in = data_in, area_code = area)
  fw <- gregwt$final_weights
  fweights <- c(fweights, fw)
  ## Estimate income
  sum.income <- sum(fw * ind$income)
  cap.income <- sum(fw * ind$income / sum(fw))
  Result[area,] <- c(area, sum.income, cap.income)
}

data("GREGWT.census")
data("GREGWT.survey")
glimpse(GREGWT.census)
length(unique(GREGWT.census$Area.Code)) # 11,287 areas
glimpse(GREGWT.survey)

simulation_data <- prepareData(GREGWT.census, GREGWT.survey,
                               census_categories=seq(2,24),
                               survey_categories=seq(1,3))

simulation_data1 <- prepareData(GREGWT.census, GREGWT.survey,
                                census_categories=seq(2,24),
                                survey_categories=seq(1,3),
                                pop_benchmark=c(2,12),
                                verbose=TRUE)

# compute the total population as the mean of all benchmarks. Breaks parameters
# needs to be defined. In this case the breaks are displaced by one because the
# area code is on the first column.
simulation_data2 <- prepareData(GREGWT.census, GREGWT.survey,
                                census_categories=seq(2,24),
                                survey_categories=seq(1,3),
                                breaks=c(11, 17),
                                verbose=TRUE)

total_pop1 <- simulation_data1$total_pop
glimpse(simulation_data1)
plot(total_pop1$pop)

total_pop2 <- simulation_data2$total_pop
points(total_pop2$pop, col="red", pch="+")

# reweight and plot the results for a single area
acode = "02"
weights_GREGWT_02 <- GREGWT(data_in= simulation_data, area_code=acode)
str(weights_GREGWT_02)



#****************************************************************************************************
#                rgenoud ####
#****************************************************************************************************
# https://github.com/Robinlovelace/spatial-microsim-book/blob/master/code/optim-tests-SimpleWorld.R
library(rgenoud)

fun <- function(par, ind_n.Freq, con){
  sim <- colSums(par * ind_n.Freq)
  ae <- abs(sim - con) # Absolute error per category
  sum(ae) # the Total Absolute Error (TAE)
}

(ind <- read_csv(file.path(PROJHOME, "play", "ind.csv")))
# Convert age into a categorical variable with user-chosen labels
(ind$age <- cut(ind$age, breaks = c(0, 49, 120), labels = c("a0_49", "a50+")))
names(con_age) <- levels(ind$age) # rename aggregate variables
cons <- cbind(con_age, con_sex)

cat_age <- model.matrix(~ ind$age - 1)
cat_sex <- model.matrix(~ ind$sex - 1)[, c(2, 1)]
(ind_cat <- cbind(cat_age, cat_sex)) # combine flat representations of the data

colSums(ind_cat) # view the aggregated version of ind
ind_agg <- colSums(ind_cat) # save the result

rbind(cons[1,], ind_agg) # test compatibility between ind_agg and cons objects

weights <- matrix(data = NA, nrow = nrow(ind), ncol = nrow(cons))
dim(weights) # the dimension of the weight matrix: 5 rows by 3 columns


# Set min and maximum values of constraints with 'Domains'
(m <- matrix(c(0, 100), ncol = 2)[rep(1, nrow(ind)),])
set.seed(2014)

out <- genoud(nvars = nrow(indu), fn = fun, ind_n.Freq = indu, con = cons[1,], control = list(maxit = 1000), data.type.int = TRUE, Domains = matrix(c(rep(0, nrow(indu)),rep(100000, nrow(indu))), ncol = 2))


genoud(nrow(ind), fn = fun, ind_num = ind, con = cons[1,],
       control = list(maxit = 1000), data.type.int = TRUE, D = m)


#****************************************************************************************************
#                sms ####
#****************************************************************************************************

library("sms")

#load the data
data(census)
data(survey) 

census
survey

in.lexicon <- createLexicon() # Create a data lexicon for holding the associated column names.
str(in.lexicon)

in.lexicon <- addDataAssociation(in.lexicon, c("he","he"))
in.lexicon <- addDataAssociation(in.lexicon, c("females","female"))

in.lexicon

this_area <- as.data.frame(census[1,]) #Select the first area from the census table

insms <- new("microsimulation", census=census, panel=survey, lexicon=in.lexicon, iterations=10)
getInfo(insms)
getTAEs(insms)

best <- find_best_selection(this_area, insms)
best

best$selection # 56 obs

myselection <- find_best_selection_SA(this_area, insms, inseed=1900)
myselection # 56 obs


hc <- run_parallel_HC(insms, inseed = -1)
sa <- run_parallel_SA(insms, inseed = -1)

sa #results
sa@results

# getElement(sa, @census)
sa@census
getInfo(sa)
getTAEs(sa)

slot(sa, census)
slotNames(sa)
getSlots(sa)
sa@census
res <- sa@results
str(res)
res[[1]]$selection
count(res[[1]]$selection, pid) %>% arrange(desc(n))
sort(res[[1]]$selection$pid)



#****************************************************************************************************
#                simulated annealing from scratch ####
#****************************************************************************************************
# https://codereview.stackexchange.com/questions/84688/simulated-annealing-in-r

schaffer <- function(xx)
{x1 <- xx[1]
x2 <- xx[2]
fact1 <- (sin(x1^2-x2^2))^2 - 0.5
fact2 <- (1 + 0.001*(x1^2+x2^2))^2
y <- 0.5 + fact1/fact2
return(y)
}


simulated_annealing <- function(func, s0, niter = 10, step = 0.1) {
  
  # Initialize
  ## s stands for state
  ## f stands for function value
  ## b stands for best
  ## c stands for current
  ## n stands for neighbor
  s_b <- s_c <- s_n <- s0
  f_b <- f_c <- f_n <- func(s_n)
  message("It\tBest\tCurrent\tNeigh\tTemp")
  message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f", 0L, f_b, f_c, f_n, 1))
  
  for (k in 1:niter) {     
    Temp <- (1 - step)^k
    # consider a random neighbor
    s_n <- rnorm(2, s_c, 1)
    f_n <- func(s_n)
    # update current state
    if (f_n < f_c || runif(1, 0, 1) < exp(-(f_n - f_c) / Temp)) {
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
# $iterations
# [1] 10
#
# $best_value
# [1] 0.1053682
# 
# $best_state
# [1] -0.3203791  1.7080835

