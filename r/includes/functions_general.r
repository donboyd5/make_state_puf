
#****************************************************************************************************
#                general functions ####
#****************************************************************************************************

parens <- function(stringvec){
  # put parentheses around each element of a string vector
  paste0("(", stringvec, ")")
}


move <- function(vec, element, after.element){
  # element is a unique element of vec
  # after.element also is a unique element of vec
  # move element into the position right after after.element
  vec2 <- setdiff(vec, element)
  iafter <- which(vec2==after.element)
  vec3 <- c(vec2[1:iafter], element)
  if(iafter < length(vec2)) vec3 <- append(vec3, vec2[(iafter+1):length(vec2)])
  return(vec3)
}


#****************************************************************************************************
#                file manipulation functions ####
#****************************************************************************************************

get_puf_vnames <- function(){
  # get data frame with PUF variables names and labels (vdesc)
  readRDS("./data/puf.vnames.rds")
}


fix_puf_vnames <- function(){
  # onetime fixes to puf.vnames
  df <- readRDS("./data/puf.vnames.rds")
  df$vdesc[df$vname=="e18400"] <- "State and local income or sales taxes" # mislabeled as State and local taxes
  df$value <- NULL # to get rid of the annoying warning "Unknown or uninitialised column: 'value'."
  saveRDS(df, "./data/puf.vnames.rds")
}


get_puf.base <- function(keepnames=NULL){
  # puf <- readRDS(paste0(globals$tc.dir, "puf.rds"))
  puf <- readRDS(paste0(globals$tc.dir, "puf_lc.rds")) # note that this has all records, and has wt variable
  if(is.null(keepnames)) keepnames <- names(puf)
  # CAUTION: make sure we do not lose the variable I created, pufseqn
  
  puf.base <- puf %>% 
    filter(!RECID %in% 999996:999999) %>%
    dplyr::select(keepnames) %>%
    mutate(ftype="puf")
  return(puf.base)
}


get_pufbase_sums <- function(){
  # get data frame with sums of PUF variables names and labels (vdesc)
  readRDS("./data/puf.base.sums.rds")
}


fget <- function(ftype, fvec){
  # read a synthesized file that is in csv form
  # fvec is a named vector of filenames, 
  # ftype is one of the column names of the vector -- i.e., a
  #   short name of the file we want to get
  read_csv(fvec[ftype], 
           col_types = cols(.default= col_double()), 
           n_max=-1) %>%
    mutate(ftype=ftype)
}

get_synfiles <- function(use.files, synfile.fnames, remove.vars=NULL, bad.MARS.files){
  synfiles <- ldply(use.files, fget, synfile.fnames)
  # glimpse(synfiles)
  # count(synfiles, ftype)
  
  synfiles <- synfiles %>%
    dplyr::select(-remove.vars) %>% # temporary -- remove vars that are not in synpufs
    mutate(MARS=ifelse(ftype %in% bad.MARS.files, round(MARS), MARS)) # early files had non-integer MARS
  return(synfiles)
}



change_case <- function(oldnames, upper_case_vars=c("MARS", "XTOT", "DSI", "EIC", "FDED", "MIDR", "RECID")){
  # convert variable names to lower case, except for those that should
  # remain upper case
  newnames <- oldnames
  lower_case_indexes <- !newnames %in% upper_case_vars
  lower_case_names <- newnames[lower_case_indexes] %>% str_to_lower
  newnames[lower_case_indexes] <-lower_case_names
  return(newnames)
}



#****************************************************************************************************
#                binning functions ####
#****************************************************************************************************

getbins <- function(vname.in, stack, nbins){
  # create puf-based bins (ntiles) for variable vname.in and get the
  # relative frequencies of the puf data for vname.in and the syn data for vname.in
  
  # get the data we'll use
  df <- stack %>%
    dplyr::select(ftype, value=vname.in)
  
  # construct puf-based bins
  bins <- df %>%
    filter(ftype=="puf") %>%
    mutate(ntile=ntile(value, nbins)) %>%
    group_by(ntile) %>%
    summarise(pufn=n(), valmin=min(value), valmax=max(value)) %>%
    group_by(valmin) %>%
    summarise(pufn=sum(pufn)) %>%
    ungroup

  # replace first min with -Inf, and add Inf
  # explicitly add - Inf, 0, small positive, and Inf, get unique values, and sort
  brks <- c(-Inf, 0, 1e-9, Inf, bins$valmin) %>% unique %>% sort
  
  # use these puf-based breaks to bin both the puf and the syn data and then get relative frequencies
  binned <- df %>%
    mutate(bin=cut(value, brks, include.lowest=TRUE, right=FALSE), bin.num=as.numeric(bin)) %>%
    group_by(ftype, bin.num, bin) %>%
    summarise(n=n()) %>%
    spread(ftype, n) %>%
    mutate_at(vars(puf, syn), funs(naz)) %>%
    ungroup %>%
    mutate_at(vars(puf, syn), funs(pct=. / sum(.) * 100)) %>%
    mutate(vname=vname.in) %>%
    dplyr::select(vname, everything()) %>%
    arrange(vname, bin.num)
  
  return(binned)
}


#****************************************************************************************************
#                correlation and statistical functions ####
#****************************************************************************************************
cordf <- function(df){
  # get correlations among vars in a df
  # put in long format with 3 columns:
  #   var1  - text variable name
  #   var2  - text variable name
  #   value - correlation between var1 and var2
  # return as a data frame
  cordf <- cor(df, use="pairwise.complete.obs") %>%
    as_tibble(rownames = "var1") %>%
    gather(var2, value, -var1)
  return(cordf)
}


trimcor <- function(df){
  # trim a long correlations data frame (such as produced by cordf)
  # so that it doesn't include "self-correlations" such as x1 with x1, which are always 1
  # and includes only 1 version of each correlation -- e.g., x1-x2 correlation, but not x2-x1 correlation
  # returns data frame with 3 columns:
  #   ftype - the grouping variable that was used
  #   combo - text variable naming the correlation -- e.g., "x1-x2"
  #   value - the correlation
  df2 <- df %>%
    filter(!var1==var2) %>%
    mutate(combo=ifelse(var1 < var2, paste0(var1, "-", var2), paste0(var2, "-", var1))) %>%
    arrange(combo) %>%
    group_by(combo) %>%
    filter(row_number()==1) %>%
    dplyr::select(one_of(c("ftype", "combo", "value"))) # use one_of so that this works even if ftype is not in file
  return(df2)
}

ks.p <- function(ftype, value) {
  # return the Kolmogorov-Smirnov test p.value
  # null hypothesis that x and y were drawn from the same continuous distribution is performed.
  # reject null when D is large
  # the larger the test statistic D is, the greater the distance between the cdfs of the distributions for
  # x and y (puf and syn)
  # we want D to be small, and p to be high
  # https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Two-sample_Kolmogorov.E2.80.93Smirnov_test
  # http://www.aip.de/groups/soe/local/numres/bookcpdf/c14-3.pdf
  df <- tibble(ftype, value)
  x <- df %>% filter(ftype=="puf") %>% .[["value"]]
  y <- df %>% filter(ftype=="syn") %>% .[["value"]]
  p.value <- ks.test(x, y)$p.value
  return(p.value)
}

ks.D <- function(ftype, value) {
  # return the Kolmogorov-Smirnov test statistic D
  # null hypothesis that x and y were drawn from the same continuous distribution is performed.
  # reject null when D is large
  # the larger the D, the greater the distance between the cdfs of the distributions for
  # x and y (puf and syn)
  # we want D to be small, and p to be high
  # https://en.wikipedia.org/wiki/Kolmogorov%E2%80%93Smirnov_test#Two-sample_Kolmogorov.E2.80.93Smirnov_test
  # http://www.aip.de/groups/soe/local/numres/bookcpdf/c14-3.pdf
  df <- tibble(ftype, value)
  x <- df %>% filter(ftype=="puf") %>% .[["value"]]
  y <- df %>% filter(ftype=="syn") %>% .[["value"]]
  D <- ks.test(x, y)$statistic
  return(D)
}

mse <- function(y, yhat){
  sqerr <- (yhat - y)^2
  return(mean(sqerr))
}

rmse <- function(y, yhat){
  rmse <- sqrt(mse(y, yhat))
  return(rmse)
}


rsq <- function (x, y) {cor(x, y) ^ 2}


sse <- function(var1, var2) {
  error <- var1 - var2
  sqrt(sum(error^2))
}


utility <- function(mod){
  # mod is a glm logistic model
  # utility is a [0, 1] measure of the utility of the data, where 1=optimal
  
  pprobs <- mod$fitted.values # predicted probabilities
  # get mse of predicted probabilities -- 
  #   mse 0 would be optimal -- we can't predict whether record is syn -- they are all 0.5
  #   mse worst is 0.25
  pmse <- mse(rep(.5, length(pprobs)), pprobs)
  utility <- 1 - (pmse / .25)
  return(utility)
}


#******************************************************************************************************************
#  distance functions ####
#******************************************************************************************************************

edist <- function(vec, mat) {
  # euclidean distance between a vector and each row of a matrix
  # returns a vector of distances with same length as vec
  matdiff <- sweep(mat, 2, vec) # subtract vec from each row of mat
  return(sqrt(rowSums(matdiff ^2)))
}


edist.ab <- function(amat, bmat){
  # euclidean distance between each row of an a matrix and each row of a b matrix
  # returns a long vector with all of the distances
  # the first subvector is distances between row 1 of amat and all rows of bmat
  # the next subvector is distances between row 2 of amat and all rows of bmat
  # and so on
  ab <- apply(amat, 1, edist, bmat)
  return(as.vector(ab))
}



#****************************************************************************************************
#                loss measures ####
#****************************************************************************************************
qloss <- function(q, y, fitted){
  # computes qloss for an individual observation, or a
  # vector of qloss for a vector of observations
  # qloss is:
  #  q *e       if e>=0         
  #  (1-q) * e  if e <0
  
  # Examples:  e   q   qloss
  #            3   .1   0.3
  #            3   .9   2.7
  #           -3   .1   2.7
  #           -3   .9   0.3
  e <- y - fitted
  qloss <- pmax(q * e, (q-1) * e)
  return(qloss)
}


#****************************************************************************************************
#                graphing and plotting functions ####
#****************************************************************************************************
# var <- "e00200"
cdfplot.unwtd <- function(var, stackdf=stack, vnames=puf.vnames){
  # print(var)
  vdesc <- vnames$vdesc[match(var, vnames$vname)]
  df <- stackdf %>%
    dplyr::select(ftype, value=var) %>%
    group_by(ftype) %>%
    arrange(value) %>%
    mutate(cum.pct=cumsum(value) / sum(value))
  
  if(nrow(df) < 100){
    p <- paste0(var, " has only ", nrow(df), " rows.")
    return(p)
  }
  
  # find a good minimum x-axis value to start the plot on -- based on a desired cumulative percentage
  cum.pct.threshold <- .01
  iminval <- min(which(df$cum.pct[df$ftype=="puf"] > cum.pct.threshold))
  minval <- df$value[df$ftype=="puf"][iminval]
  # minval
  
  capt <- "- x-axis is log10 scale\n- For display purposes x-axis is truncated at left to start at puf's cum.pct=1%"
  gtitle <- paste0("Cumulative distribution of unweighted ", var, ": ", vdesc)
  gsub <- "Aggregate records excluded"
  ylab <- paste0("Cumulative proportion of the sum of unweighted ", var)
  
  # define x scale break points and associated labels
  sq10 <- c(0, 1e3, 10e3, 25e3, 50e3, 100e3, 250e3, 500e3, 750e3, 1e6,
            1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, 25e6, 50e6, 100e6)
  xlabs <- scales::comma(sq10 / 1e3)
  xscale.l10 <- scale_x_log10(name=paste0(var, " in $ thousands"), breaks=sq10, labels=xlabs)
  
  p <- df %>%
    filter(value > minval) %>%
    ggplot(aes(value, cum.pct, colour=ftype)) + 
    geom_line(size=1.5) +
    theme_bw() +
    ggtitle(gtitle, subtitle=gsub) +  labs(caption=capt) +
    scale_y_continuous(name=ylab, breaks=c(seq(0, .9, .05), seq(.92, 1, .02))) +
    xscale.l10 +
    theme(axis.text.x=element_text(angle=45, size=10, hjust=1, colour="black")) +
    theme(plot.caption = element_text(hjust=0, size=rel(.8)))
  return(p)
}


cdfplot <- function(var){
  # print(var)
  vdesc <- synvars$vdesc[match(var, synvars$vname)]
  df <- stack %>%
    dplyr::select(ftype, wt, value=var) %>%
    group_by(ftype) %>%
    arrange(value) %>%
    mutate(cum.pct=cumsum(wt * value) / sum(wt * value))
  
  if(nrow(df) < 100){
    p <- paste0(var, " has only ", nrow(df), " rows.")
    return(p)
  }
  
  # find a good minimum x-axis value to start the plot on -- based on a desired cumulative percentage
  cum.pct.threshold <- .01
  iminval <- min(which(df$cum.pct[df$ftype=="puf.full"] > cum.pct.threshold))
  minval <- df$value[df$ftype=="puf.full"][iminval]
  # minval
  
  capt <- "- x-axis is log10 scale\n- For display purposes x-axis is truncated at left to start at puf.full's cum.pct=1%"
  gtitle <- paste0("Cumulative distribution of weighted ", var, ": ", vdesc)
  gsub <- "Aggregate records excluded"
  ylab <- paste0("Cumulative proportion of the sum of weighted ", var)
  
  # define x scale break points and associated labels
  sq10 <- c(0, 1e3, 10e3, 25e3, 50e3, 100e3, 250e3, 500e3, 750e3, 1e6,
            1.5e6, 2e6, 3e6, 4e6, 5e6, 10e6, 25e6, 50e6, 100e6)
  xlabs <- scales::comma(sq10 / 1e3)
  xscale.l10 <- scale_x_log10(name=paste0(var, " in $ thousands"), breaks=sq10, labels=xlabs)
  
  p <- df %>%
    filter(value > minval) %>%
    ggplot(aes(value, cum.pct, colour=ftype)) + 
    geom_line(size=1.5) +
    theme_bw() +
    ggtitle(gtitle, subtitle=gsub) +  labs(caption=capt) +
    scale_y_continuous(name=ylab, breaks=c(seq(0, .9, .05), seq(.92, 1, .02))) +
    xscale.l10 +
    theme(axis.text.x=element_text(angle=45, size=10, hjust=1, colour="black")) +
    theme(plot.caption = element_text(hjust=0, size=rel(.8)))
  return(p)
}



#****************************************************************************************************
#                Prep a synthetic file and its puf counterpart for tax analysis and weighting ####
#****************************************************************************************************
# stacked.pufsyn <- stack
# cmd <- "C:/ProgramData/Anaconda3/Scripts/tc \"D:/tcdir/tcbase.csv\" 2013  --dump --outdir \"D:/tcdir/\""
# system(cmd)

get_pufsyn_taxplan_results <- function(stacked.pufsyn, taxplan.fn=NULL, prepare=TRUE, runname="base") {
  # get_pufsyn_taxplan_results requires as inputs:
  #    stacked.pufsyn -- a dataframe with stacked tax syn file and its corresponding puf (same variables) -- variable name case will be adjusted
  #    taxplan.fn -- character variable holding the name (without directory) of the taxplan json file e.g., "rate_cut.json"
  #  CURRENTLY THIS ONLY WORKS FOR 2013 TAX YEAR!!!!
  
  # it returns a list with two data frames:
  #   tc.base -- the input file, as adjusted for Tax-Calculator
  #   tc.output -- the output file with Tax-Calculator results
  
  # prepare a base file for tax calculator
  prepare_tc.base <- function(stacked.pufsyn){
    tc.base <- stacked.pufsyn %>%
      setNames(change_case(names(.))) %>% # Tax-Calculator expects mostly lower-case names
      do(impose_variable_rules(.)) %>% # not needed for synpuf5 and later
      do(prime_spouse_splits(.)) %>%
      arrange(ftype, m) %>% # DJB NEW!
      mutate(RECID=row_number()) # WILL OVERWRITE OLD RECID!!!!
    return(tc.base)
  }
  
  if(prepare==TRUE) tc.base <- prepare_tc.base(stacked.pufsyn) else tc.base <- stacked.pufsyn
  
  # write tcbase to a file, because the Tax-Calculator CLI reads a csv file
  # maybe use temp file?
  # tc.fn <- "tcbase.csv"
  # tc.fn <- paste0(basename(tempfile()), ".csv")
  tc.fn <- paste0(runname, "_", str_remove(basename(taxplan.fn), ".json"), ".csv")
  write_csv(tc.base, paste0(globals$tc.dir, tc.fn))
  # write_csv(tc.base, tc.fn)
  
  # cmd <- tc.wincmd(tc.fn, globals$tc.dir, globals$tc.cli)
  cmd <- tc.wincmd(tc.fn, globals$tc.dir, globals$tc.cli, taxyear=2013, taxplan.fn=taxplan.fn, taxplans.dir=globals$taxplans.dir)
  # cmd    # a good idea to look at the command
  
  a <- proc.time()
  system(cmd) # CAUTION: this will overwrite any existing output file that had same input filename!
  proc.time() - a # it can easily take 5-10 minutes depending on the size of the input file

  # base <- str_remove(basename(tc.fn), ".csv")
  if(is.null(taxplan.fn)) tc.outfn <- paste0(runname, "-", 13, "-#-#-#", ".csv") else
    tc.outfn <- paste0(runname, "-", 13, "-#-", str_remove(basename(taxplan.fn), ".json"), "-#.csv")
  # print(tc.outfn)
  
  tc.output <- read_csv(paste0(globals$tc.dir, tc.outfn),
                        col_types = cols(.default= col_double()),
                        n_max=-1)

  results <- list(tc.base=tc.base, tc.output=tc.output)
  return(results)
}


run_taxplan_getresults <- function(source.fn, taxplan.fn=NULL) {
  # get_pufsyn_taxplan_results requires as inputs:
  #    source.fn -- basename of csv file with stacked tax syn file and its corresponding puf (same variables)
  #      it should be located in globals$tc.dir
  #    taxplan.fn -- character variable holding the basename (without directory) of the taxplan json file e.g., "rate_cut.json"
  #  CURRENTLY THIS ONLY WORKS FOR 2013 TAX YEAR!!!!
  
  # it returns a list with two data frames:
  #   tc.base -- the input file, as adjusted for Tax-Calculator
  #   tc.output -- the output file with Tax-Calculator results

  cmd <- tc.wincmd(source.fn, globals$tc.dir, globals$tc.cli, taxyear=2013, taxplan.fn=taxplan.fn, taxplans.dir=globals$taxplans.dir)
  # cmd    # a good idea to look at the command
  
  a <- proc.time()
  system(cmd) # CAUTION: this will overwrite any existing output file that had same input filename!
  proc.time() - a # it can easily take 5-10 minutes depending on the size of the input file
  
  # base <- str_remove(basename(tc.fn), ".csv")
  if(is.null(taxplan.fn)) tc.outfn <- paste0(str_remove(source.fn, ".csv"), "-", 13, "-#-#-#", ".csv") else
    tc.outfn <- paste0(str_remove(source.fn, ".csv"), "-", 13, "-#-", str_remove(taxplan.fn, ".json"), "-#.csv")
  # print(tc.outfn)
  # 
  tc.output <- read_csv(paste0(globals$tc.dir, tc.outfn),
                        col_types = cols(.default= col_double()),
                        n_max=-1)
  saveRDS(tc.output, paste0(globals$tc.dir, str_remove(source.fn, ".csv"), "_", str_remove(taxplan.fn, ".json"), "_output.rds"))
  
  return(NULL)
}


#****************************************************************************************************
#                functions related to tax analysis ####
#****************************************************************************************************

impose_variable_rules <- function(df){
  # Per https://pslmodels.github.io/Tax-Calculator/, Tax-Calculator requires:
  #    ordinary dividends (e00600) >= qualified dividends (e00650)
  #    total pension and annuity income (e01500) >= taxable pension and annuity income (e01700)
  
  # A few of the early synthesized files did not enforce this so we enforce it here by
  # forcing the e00600 and e01500 to be at least as large as their counterparts
  # This does NO ERROR CHECKING or reporting to let the user know of a potential problem.
  # Once we no longer use those early files we should stop running this function.
  # This should not be needed for synpuf5 and later.
  
  if("e00650" %in% names(df) & "e00600" %in% names(df)){
    df <- df %>%
      mutate(e00600=pmax(e00650, e00600))
  }
  if("e01500" %in% names(df) & "e01700" %in% names(df)){
    df <- df %>%
      mutate(e01500=pmax(e01500, e01700))
  }
  return(df)
}


prime_spouse_splits <- function(df){
  # Per https://pslmodels.github.io/Tax-Calculator/, Tax-Calculator requires the filing-unit total 
  # for each of several earnings-related variables to be split between the taxpayer and the spouse:
  #   e00200 = e00200p + e00200s  # wages
  #   e00900 = e00900p + e00900s  # business income or loss
  #   e02100 = e02100p + e02100s  # Schedule F net profit/loss
  
  # For now, create arbitrary prime-spouse splits of these variables so that we can
  # run Tax-Calculator
  prime.pct <- ifelse(df$MARS==2, .5, 1)
  spouse.pct <- ifelse(df$MARS==2, .5, 0)
  df <- df %>%
    mutate(e00200p=e00200 * prime.pct,
           e00900p=e00900 * prime.pct,
           e02100p=e02100 * prime.pct,
           
           e00200s=e00200 * spouse.pct,
           e00900s=e00900 * spouse.pct,
           e02100s=e02100 * spouse.pct)
  return(df)
}


tc.wincmd <- function(tc.fn, tc.dir, tc.cli, taxyear=2013, taxplan.fn=NULL, taxplans.dir=NULL){
  # Build a Windows system command that will call the Tax-Calculator CLI. See:
  #   https://pslmodels.github.io/Tax-Calculator/
  # CAUTION: must use full dir names, not relative to working directory
  # 2013 is the FIRST possible tax year that Tax-Calculator will do
  
  # To update Tax-Calculator:
  #  open Anaconda prompt box - for example, from Windows button at lower left of screen or on keyboard
  #  execute:
  #    conda update -c PSLmodels taxcalc
  
  tc.infile.fullpath <- shQuote(paste0(paste0(tc.dir, tc.fn)))
  tc.outdir <- shQuote(str_sub(tc.dir, 1, -1)) # must remove trailing "/"
  
  taxplanstring <- NULL
  if(!is.null(taxplan.fn)) taxplanstring <- paste0("--reform", " ", shQuote(paste0(paste0(taxplans.dir, taxplan.fn))))
  
  cmd <- paste0(tc.cli, " ", tc.infile.fullpath, " ", taxyear, " ", taxplanstring, " ", "--dump --outdir ", tc.outdir)
  return(cmd)
}

