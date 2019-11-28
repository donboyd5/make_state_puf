# 5/18/2019

# Parse and save SOI Historical Table 2 data, which we will use as targets for the 2011 data file.

# Save the result in hist2_2011.csv in the data directory.


#****************************************************************************************************
#                Libraries and globals ####
#****************************************************************************************************
source("./r/includes/libraries.r")
source("./r/includes/globals.r")

source("./r/includes/functions_general.r")
source("./r/includes/functions_state_puf.r")


#****************************************************************************************************
#                get and save data ####
#****************************************************************************************************
# read the data, put line numbers on it, reformat as needed
# lineno h2vname table_desc             incgrp  target imin_ge imax_lt vname puf_multiplier inc.rule        other.rule select.rule        calc.rule            
# <dbl> <chr>   <chr>                  <chr>    <dbl>   <dbl>   <dbl> <chr> <chr>          <chr>           <chr>      <chr>              <chr>                
#   1   1024 e18600  State and local other~ inc4    5.93e7   50000   75000 e186~ e18600         (e00100 >= 500~ ""         ((e00100 >= 50000~ (wt * e18600

(inclink <- get_income_ranges(2011))

xwalk <- get_SOI_puf_xwalk(2011)
ht(xwalk)

globals


# create variable names for the hist2 data
x1 <- read_excel(paste0(globals$hist2, "11in54cm.xlsx"), range="A3:TK3", col_names=FALSE)
# put the state names into x2, without "Item" or NAs
x2 <- x1 %>% unlist %>% unname
x2 <- x2[!is.na(x2)]
x2 <- x2[-1]
# replace state names with state abbreviations
imatch <- match(x2, str_to_upper(state.name))
x3 <- x2
x3 <- state.abb[imatch]
x3[1] <- "US"; x3[10] <- "DC"; x3[53] <- "OA"
# check
cbind(x2, x3) # good, x3 is what we want

# now create vnames - each state has incgrp 0-9 where inc0 is all returns
inc <- paste0("inc", 0:9)
# CAUTION - keep x3 in original order
vnamesdf <- expand.grid(stabbr=x3, incgrp=inc, stringsAsFactors = FALSE) %>%
  mutate(stabbr=factor(stabbr, levels=x3), # will keep the desired order
         stinc=paste0(stabbr, "_", incgrp)) %>% 
  arrange(stabbr, incgrp)
vnames <- c("table_desc", vnamesdf$stinc)
# NY should be 332:341 based on looking at workbook
# vnames[332:341]
  
# get the 53-"state" hist2 data (states, DC, US, other areas)
df1 <- read_excel(paste0(globals$hist2, "11in54cm.xlsx"), range="A10:TK112", col_names=vnames) %>%
  mutate(lineno=row_number()) %>%
  select(lineno, everything())
df1[c(1:5, (nrow(df1)-5):nrow(df1)), c(1:5, (ncol(df1) - 5):ncol(df1))]

df2 <- df1 %>%
  gather(vname, value, -lineno, -table_desc) %>%
  separate(vname, c("stabbr", "incgrp"))
ht(df2)

write_csv(df2, "./data/hist2_2011.csv")


# tmpny <- read_csv("./data/hist2_2011.csv") %>% filter(stabbr=="NY")


