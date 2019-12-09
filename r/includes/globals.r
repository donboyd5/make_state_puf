

#****************************************************************************************************
#                SYSTEM-SPECIFIC GLOBALS ####
#****************************************************************************************************
# change these variables when moving to a new system 

if(!exists("globals")) globals <- list()
globals$dbox <- "C:/Users/donbo/Dropbox/"

globals$pufdir <- paste0(globals$dbox, "OSPC - Shared/IRS_pubuse_2011/") # location of puf2011.csv

# private directory for Tax-Calculator record-level output that we don't want moved from this machine
globals$tcdir <- "D:/tcdir/"

globals$statedir <- paste0(globals$tc.dir, "state_puf/")

globals$hist2 <- paste0(globals$dbox, "RPrograms PC/OSPC/make_state_puf/data/SOI_Historical_Table_2/")

globals$tc.cli <- "C:/ProgramData/Anaconda3/Scripts/tc" # location of Tax-Calculator command-line interface

# globals$taxplans.dir <- "D:/Dropbox/RPrograms PC/OSPC/syndata4/tax_plans/"

# filenames ----
globals$xlfile <- "Boyd_State_PUF_info(4).xlsx"


#****************************************************************************************************
#                Non-System-Specific Globals ####
#****************************************************************************************************
globals$agibrks <- list(`2011` = c(-Inf, 1, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf),
                        `2016` = c(-Inf, 1, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf),
                        `2017` = c(-Inf, 1, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf))
# 2011 Historical Table 2 definition
# 0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $25,000'
# 3 = '$25,000 under $50,000'
# 4 = '$50,000 under $75,000'
# 5 = '$75,000 under $100,000'
# 6 = '$100,000 under $200,000'
# 7 = ‘$200,000 under $500,000’
# 8 = ‘$500,000 under $1,000,000’
# 9 = ‘$1,000,000 or more’
# globals$agibrks_hist2_2011 <- c(-Inf, 1, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf)

# 2016 Historical Table 2 definition
# 0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $10,000'
# 3 = '$10,000 under $25,000'
# 4 = '$25,000 under $50,000'
# 5 = '$50,000 under $75,000'
# 6 = '$75,000 under $100,000'
# 7 = '$100,000 under $200,000'
# 8 = ‘$200,000 under $500,000’
# 9 = ‘$500,000 under $1,000,000’
# 10 = ‘$1,000,000 or more’
# globals$agibrks_hist2_2016 <- c(-Inf, 1, 10e3, 25e3, 50e3, 75e3, 100e3, 200e3, 500e3, 1e6, Inf)

# no change to agi breaks in 2017
# 2017 Historical Table 2 definition
# Size of adjusted gross income	0 = No AGI Stub
# 1 = ‘Under $1’
# 2 = '$1 under $10,000'
# 3 = '$10,000 under $25,000'
# 4 = '$25,000 under $50,000'
# 5 = '$50,000 under $75,000'
# 6 = '$75,000 under $100,000'
# 7 = '$100,000 under $200,000'
# 8 = ‘$200,000 under $500,000’
# 9 = ‘$500,000 under $1,000,000’
# 10 = ‘$1,000,000 or more’
# globals$agibrks_hist2_2017 <- globals$agibrks_hist2_2016

# globals$agibrks[[as.character(2011)]]

