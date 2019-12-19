
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr stringr forcats

library("readxl")

library("scales")

library("btools") # devtools::install_github("donboyd5/btools")

library("janitor") # for adorn_totals

# library("htmltools")
library("knitr")
library("kableExtra") # caution - this prevents knitr from rendering output to console, but it goes to viewer if %>% kable_styling()
# kable(mtcars[1:5, ]) # for testing
# kable(mtcars[1:5], format="html")
# kable(mtcars[1:5], format="rst") # works in console even if in an rmd file
# detach(package:kableExtra)

library("ipoptr")

