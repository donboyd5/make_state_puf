
# get the TaxData-ready file and create a Tax-Calculator-ready version

pufny2017 <- read_csv(paste0(globals$state_puf_shared, "puf_ny_2017.csv"))
ns(pufny2017)

pufny2017tc <- pufny2017 %>%
  setNames(change_case(names(.))) %>% # Tax-Calculator expects mostly lower-case names
  do(impose_variable_rules(.)) %>% # not needed for synpuf5 and later
  do(prime_spouse_splits(.))

write_csv(pufny2017tc, paste0(globals$state_puf_shared, "puf_ny_2017_tcversion.csv"))


# plandir <- "C:/Users/donbo/Dropbox/RPrograms PC/OSPC/syndata_presentation/tax_plans/"
# (data_path <- paste0(taxref, "estack_rotten2013.csv"))
# # reforms <- c( "2017_law.json", "Trump2017.json")
# reforms <- c( "rate_cut_2017.json", "winlose_2017.json")
# 
# run_reforms(data_path, taxyear=2017, taxref, reforms, plandir)




#****************************************************************************************************
#                prepare puf for Tax-Calculator and run the tc CLI ####
#****************************************************************************************************
tc_cmd <- function(input_fn, reform="baseline") {
  #.. define the Windows command to call the tc CLI ####
  # Build a Windows system command that will call the Tax-Calculator CLI. See:
  #   https://pslmodels.github.io/Tax-Calculator/
  #   https://pslmodels.github.io/Tax-Calculator/tc_starting.html
  #   https://pslmodels.github.io/Tax-Calculator/uguide.html
  
  # CAUTION: must use full dir names, not relative to working directory
  # CAUTION: any directory names that have spaces in them must be shQuoted
  # CAUTION: when I updated Anaconda most recently, I had to add 
  #   C:\Users\donbo\Anaconda3\Library\bin to the system path for Tax-Calculator to work with the system(cmd) approach
  # CAUTION: 2013 is the FIRST possible tax year that Tax-Calculator will do
  
  # Here is the tc CLI usage: 
  # tc INPUT TAXYEAR [--help]
  # [--baseline BASELINE] [--reform REFORM] [--assump  ASSUMP]
  # [--exact] [--tables] [--graphs]
  # [--dump] [--dvars DVARS] [--sqldb] [--outdir OUTDIR]
  # [--test] [--version]  
  
  dvars <- c("e00200", "c00100", "c62100", "c09600", "c05800", "taxbc")
  
  plan_dir <- "C:/RPrograms PC/OSPC/make_state_puf/tax_plans/"
  data_dir <- globals$state_puf_shared
  
  data_path <- paste0(data_dir, input_fn)
  
  dvars_path <- paste0(data_dir, "dumpvars.txt")
  cat(dvars, file=dvars_path, sep=" ") # write the dvars file
  
  cmd <- "C:/ProgramData/Anaconda3/Scripts/tc"
  
  # define arguments to the command
  args_base <- c(shQuote(paste0(globals$state_puf_shared, input_fn)), 2017)
  
  # set args_reform to NULL, only change it if a plan is named
  args_reform <- NULL
  if(reform != "baseline") args_reform <- c("--reform", shQuote(paste0(plan_dir, reform)))
  
  args_dump <- "--dump"
  args_dvars <- c("--dvars", shQuote(dvars_path))
  args_output <- c("--outdir", shQuote(globals$state_puf_shared))
  
  args <- c(args_base, args_reform, args_dump, args_dvars, args_output)
  
  return(list(cmd=cmd, args=args))
}


fn <- "puf_ny_2017_tcversion.csv"
calc1 <- tc_cmd(fn, "2017_law.json")
calc2 <- tc_cmd(fn, "Trump2017.json")

calc <- calc2

#.. run the command ----
a <- proc.time()
system2(calc$cmd, calc$args) # CAUTION: this will overwrite any existing output file that was based on the same input filename!
b <- proc.time()
b - a  # it can easily take 5-10 minutes depending on the size of the input file


#****************************************************************************************************
#                compare 2017 law and Trump proposal, US and NY ####
#****************************************************************************************************
base <- read_csv(paste0(globals$state_puf_shared, "puf_ny_2017.csv")) %>%
  select(RECID, E00100, AGI_STUB, E05800, wtus_2017, weight_state)

law2017 <- read_csv(paste0(globals$state_puf_shared, "puf_ny_2017_tcversion-17-#-2017_law-#.csv")) %>%
  select(RECID, tax_base=c05800)

trump2017 <- read_csv(paste0(globals$state_puf_shared, "puf_ny_2017_tcversion-17-#-Trump2017-#.csv")) %>%
  select(RECID, tax_reform=c05800)

comp <- base %>%
  left_join(law2017) %>%
  left_join(trump2017)
glimpse(comp)


complong <- comp %>%
  pivot_longer(c(wtus_2017, weight_state), names_to="geo", values_to = "weight") %>%
  mutate(geo=ifelse(geo=="weight_state", "NY", "US")) %>%
  group_by(AGI_STUB, geo) %>%
  summarise(tax_base=sum(weight * tax_base), tax_reform=sum(weight * tax_reform)) %>%
  mutate(diff=tax_reform - tax_base,
         pdiff=diff / tax_base * 100)
complong
glimpse(complong)

tab <- complong %>%
  select(AGI_STUB, geo, pdiff) %>%
  pivot_wider(names_from = geo, values_from = pdiff) %>%
  left_join(globals$agilabs %>% select(-year))
tab


winlose1 <- comp %>%
  pivot_longer(c(wtus_2017, weight_state), names_to="geo", values_to = "weight") %>%
  mutate(geo=ifelse(geo=="weight_state", "NY", "US"),
         winlose=case_when(tax_reform > tax_base ~ "lose",
                           tax_reform == tax_base ~ "nochange",
                           tax_reform < tax_base ~ "win",
                           TRUE ~ "ERROR")) %>%
  group_by(AGI_STUB, geo, winlose) %>%
  summarise(tax_base=sum(weight * tax_base), 
            tax_reform=sum(weight * tax_reform),
            wtdn=sum(weight)) 
winlose2 <- winlose1 %>%
  group_by(geo, winlose) %>%
  summarise_all(sum) %>%
  mutate(AGI_STUB=11)
winlose <- bind_rows(winlose1, winlose2) %>%
  group_by(AGI_STUB, geo) %>%
  mutate(pctlose=ifelse("lose" %in% winlose, wtdn[winlose=="lose"] / sum(wtdn) * 100, 0))
winlose

tab <- winlose %>%
  ungroup %>%
  filter(AGI_STUB==1 | winlose=="lose") %>%
  select(AGI_STUB, geo, pctlose) %>%
  pivot_wider(names_from = geo, values_from = pctlose) %>%
  left_join(globals$agilabs %>% select(-year)) %>%
  select(AGI_STUB, agi_label, US, NY) %>%
  mutate(agi_label=ifelse(AGI_STUB==11, "Total", agi_label))

ft <- tab %>%
  flextable(col_keys = c("agi_label", 
                         "separator1",
                         "US", "NY")) %>%
  set_header_labels(agi_label="AGI range") %>%
  add_header_row(values=c("Percentage of returns with tax increase"), colwidths=4) %>%
  add_header_row(values="", colwidths=4) %>%
  align(i = 1, j = NULL, align = "center", part = "header") %>%
  colformat_num(j = c("US", "NY"),
                big.mark=",", digits = 1, na_str = "N/A") %>%
  width(j= ~ agi_label, width=3) %>%
  border_remove() %>%
  hline_bottom(j= ~ agi_label + US + NY, border = fp_border(width = 1), part = "header") %>%
  hline(i=10, j= ~ agi_label + US + NY, border = fp_border(width = 1), part = "body") %>%
  fontsize(size = 16, part = "all") %>%
  fontsize(i=1, size = 16, part = "header")
ft
save_as_image(ft, path = here::here("results", "taxincreases.png"), zoom=7)



# A00100 <- make_flex("A00100", tab)
# save_as_image(A00100, path = here::here("results", "A00100.png"), zoom=7)

savetab <- function(vname, tab) {
  ftab <- make_flex(vname, tab)
  save_as_image(ftab, path = here::here("results", paste0(vname, ".png")), zoom=7)
}








