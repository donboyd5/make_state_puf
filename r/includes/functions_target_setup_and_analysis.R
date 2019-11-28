

#****************************************************************************************************
#                target preparation functions ####
#****************************************************************************************************

get_state_weighting_recipe <- function(sheet){
  
  warn <- options()$warn
  options(warn=-1)
  df <- read_excel(paste0("./data/", "state_weighting_recipes.xlsx"), sheet=sheet) %>%
    dplyr::select(-comments) %>%
    mutate(puf.value=as.numeric(puf.value)) %>%
    filter(!(is.na(n.sum) & is.na(val.sum) & is.na(n.pos) & is.na(n.neg) & is.na(val.pos) & is.na(val.neg)))
  options(warn=warn)
  
  return(df)
}


get_recipe_long <- function(recipe.wide){
  recipe.long <- recipe.wide %>%
    dplyr::select(-puf.value, -vdesc) %>%
    gather(fn, group.description, -vname) %>%
    filter(!is.na(group.description)) %>%
    mutate(group1=group.description,
           group1=ifelse(group.description=="each_agi.range_mars.group", "agi.range", group1),
           group2=ifelse(group.description=="each_agi.range_mars.group", "mars.group", NA_character_)) %>%
    gather(subgroup, group.name, group1, group2) %>%
    filter(!is.na(group.name)) %>%
    dplyr::select(-subgroup) %>%
    arrange(vname, fn, group.name) %>%
    mutate(fn.rule=case_when(fn=="n.sum"  ~    "wt",
                             fn=="val.sum" ~   paste0("wt * ", vname),
                             fn=="n.pos"  ~    paste0("wt * ", "(", vname, "  > 0)"),
                             fn=="n.neg"  ~    paste0("wt * ", "(", vname, "  < 0)"),
                             fn=="val.pos"  ~  paste0("wt * ", vname, " * ", "(", vname, "  > 0)"),
                             fn=="val.neg"  ~  paste0("wt * ", vname, " * ", "(", vname, "  < 0)"),
                             TRUE ~ NA_character_) %>% parens(.))
  return(recipe.long)
}


add_rulenames <- function(target.rules){
  target.rules <- target.rules %>%
    mutate(constraint.name=paste0(wtvar, "_", str_remove_all(subgroup, " ")),
           constraint.shortname=paste0("con_", row_number()),
           target.num=row_number())
  return(target.rules)
}


#****************************************************************************************************
#                constraint coefficient functions ####
#****************************************************************************************************
get_constraint_coefficients_sparse <- function(df, target.rules){
  # get the nonzero constraint coefficients for all feasible targets, in sparse form, as a data frame
  # also create an enhanced targets data frame that includes a feasibility true-false indicator, a counter
  # for the feasible constraint number, and sum of the RHS for the constraint
  
  # target.num is the target number - there may be fewer feasible constraints than desired targets
  # i is the constraint number (there may be fewer feasible constraints than targets)
  # j is the variable number
  
  get_nzcc <- function(target.num, df, target.rules, rules, enhanced.targets, wtvar="wt"){
    # get constraint coefficients for a single target (a row in target.rules), as a data frame
    
    # CAUTION ----
    #  target.num is the target's number among all of the targets in target.rules
    #  cnum is the constraint number among all feasible constraints
    #  some targets may not result in feasible constraints - if there are no non-zero constraint coefficients
    #    (i.e., if the target value will not change no matter what - as would happen if all
    #     observations that enter into the calculation are zero)
    #  thus cnum (the number for a feasible constraint) can be less than target.num (its location in the list of targets)

    # get all constraint coefficients for this target
    cc <- with(df, eval(rules[target.num])) 
    
    inzcc <- which(cc!=0)  # get indexes of the nonzero constraint coefficients
    
    nzcc.row <- NULL
    if(length(inzcc) > 0){
      # only increase i, the counter of feasible constraints, if this constraint has at least one nonzero constraint coefficient
      i <<- i + 1 # MUST use the << syntax to change the value of the global variable i
      nzcc.row <- tibble(i=i, j=inzcc, nzcc=cc[inzcc], target.num=target.rules$target.num[target.num])
      enhanced.targets$cnum[target.num] <<- i # the constraint number associated with this target.num -- cnum can be < target.num
      enhanced.targets$rhs[target.num] <<- sum(nzcc.row$nzcc)
    } else enhanced.targets$feasible[target.num] <<- FALSE  # note the << syntax to change global variable
    
    return(nzcc.row)
  }
  
  # Prepare to run get_nzcc on all targets
  i <- 0 # counter for the number of feasible targets
  enhanced.targets <- target.rules
  enhanced.targets$feasible <- TRUE # we will change this to false for any non-feasible targets
  enhanced.targets$cnum <- NA
  enhanced.targets$rhs <- 0
  rules <- parse(text=target.rules$calc.rule) 
  nzcc <- ldply(1:nrow(target.rules), get_nzcc, df, target.rules, rules, enhanced.targets)
  feasible.targets <- enhanced.targets %>% filter(feasible)
  # return enhanced targets and feasible targets in case we want to examine the enhanced to see what we are dropping
  
  return(list(nzcc=nzcc, enhanced.targets=enhanced.targets, feasible.targets=feasible.targets))
}


get_constraint_sums <- function(df, target.rules){
  get_consum <- function(target.num, df, target.rules, rules){
    # get constraint coefficients for a single target (a row in target.rules), as a data frame
    
    # determine whether the coefficient will be:
    #   the weight for each record, if we are targeting number of returns, or
    #   the weighted multiplied by a variable's value, for each record, if we are targeting weighted value of a variable
    # the vector multiplier has the weight, or the weighted value of the variable
    
    # get all constraint coefficients for this target
    cc <- with(df, eval(rules[target.num])) 
    return(sum(cc))
  }
  
  rules <- parse(text=target.rules$calc.rule) # R logical expression for rules used in eval below
  vsums <- laply(1:nrow(target.rules), get_consum, df, target.rules, rules)
  return(vsums)
}


# get_constraint_sums <- function(df, target.rules){
#   get_consum <- function(target.num, df, target.rules, rules, wtvar="wt"){
#     # get constraint coefficients for a single target (a row in target.rules), as a data frame
#     
#     # determine whether the coefficient will be:
#     #   the weight for each record, if we are targeting number of returns, or
#     #   the weighted multiplied by a variable's value, for each record, if we are targeting weighted value of a variable
#     # the vector multiplier has the weight, or the weighted value of the variable
#     if(target.rules$wtvar[target.num]==wtvar) multiplier <- rep(1, nrow(df)) else # weighted number of records 
#       multiplier <- df[[target.rules$wtvar[target.num]]] # weighted value
#     
#     # get all constraint coefficients for this target
#     cc <- with(df, eval(rules[target.num]) * wt) * multiplier 
#     return(sum(cc))
#   }
#   
#   rules <- parse(text=target.rules$subgroup) # R logical expression for rules used in eval below
#   vsums <- laply(1:nrow(target.rules), get_consum, df, target.rules, rules)
#   return(vsums)
# }


make.sparse.structure.from.nzcc <- function(constraint.coefficients.sparse){
  dlply(constraint.coefficients.sparse, .(i), function(x) x$j)
}


get_constraint_coefficients_dense <- function(synfile, target.rules, wtvar="wt") {
  # get constraint coefficients -- how the value of a target changes when the weight changes
  
  rules <- parse(text=target.rules$subgroup) # R logical expression for rules used in eval below
  
  cc.base <- synfile
  for(i in 1:nrow(target.rules)){
    vname <- target.rules$constraint.shortname[i]
    if(target.rules$wtvar[i]==wtvar) mult <- rep(1, nrow(cc.base)) else
      mult <- cc.base[[target.rules$wtvar[i]]]
    cc.base[[vname]] <- with(cc.base, eval(rules[i]) * wt) * mult
  }
  cc.dense <- cc.base %>% dplyr::select(one_of(target.rules$constraint.shortname))
  return(cc.dense)
}


find_non_feasible_constraints <- function(synfile, target.rules){
  # find out if any of the constraints are not in the sparse matrix, meaning that the synthetic
  # file has one or more records that meet the logical criteria for that constraint (e.g., c00100==0)
  # but all values for the variable to be weighted are zero and thus it is impossible to reweight
  # and get a different value
  
  # There's got to be a better way to find these!
  
  constraint.coefficients.dense <- get_constraint_coefficients_dense(synfile, target.rules)
  constraint.shortname.vec <- names(constraint.coefficients.dense)
  
  # put the constraint coefficients into a sparse format that only 
  # includes non-zero coefficients, in a dataframe that has:
  #   i -- the constraint number
  #   constraint.shortname
  #   j -- the variable number (an index into the vector x)
  #   value -- the constraint coefficient
  constraint.coefficients.sparse.df <- constraint.coefficients.dense %>% 
    mutate(j=row_number()) %>%
    gather(constraint.shortname, value, -j) %>%
    mutate(constraint.shortname=
             factor(constraint.shortname, levels=constraint.shortname.vec), # factor so we can sort in order of appearance in cc.full
           i=match(constraint.shortname, constraint.shortname.vec)) %>%
    filter(value!=0) %>%
    dplyr::select(i, constraint.shortname, j, value) %>%
    arrange(i, j) # SORT ORDER IS CRITICAL
  
  # return a list of 3 items
  feasibility.list <- list()
  feasibility.list$nonfeasible.indexes <- which(!target.rules$constraint.shortname %in% 
                                                  unique(constraint.coefficients.sparse.df$constraint.shortname))
  feasibility.list$feasible.indexes <- setdiff(1:nrow(target.rules), feasibility.list$nonfeasible.indexes)
  
  feasibility.list$target.rules.feasible <- add_rulenames(target.rules[feasibility.list$feasible.indexes, ])
  
  return(feasibility.list)
}


get_constraint_coefficients_sparse_from_dense <- function(constraint.coefficients.dense){
  constraint.shortname.vec <- names(constraint.coefficients.dense)
  constraint.shortname.vec
  
  constraint.coefficients.sparse <- constraint.coefficients.dense %>% 
    mutate(j=row_number()) %>%
    gather(constraint.shortname, value, -j) %>%
    mutate(constraint.shortname=
             factor(constraint.shortname, levels=constraint.shortname.vec), # factor so we can sort in order of appearance in cc.full
           i=match(constraint.shortname, constraint.shortname.vec)) %>%
    filter(value!=0) %>%
    dplyr::select(i, constraint.shortname, j, value) %>%
    arrange(i, j) # SORT ORDER IS CRITICAL
}

