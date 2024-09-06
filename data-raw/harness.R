
# Program for playing with the penvir package ----


# useful code -------------------------------------------------------------

file.edit("../pkgtools.R")
file.edit("../_package_creation_example.R")



# load packages -----------------------------------------------------------

source("../pkgtools.R") # load packages


# examine penvir ----------------------------------------------------------

library(penvir)

?penvir

load_all() # shift-ctrl-l

# test out some functions -------------------------------------------------

check_fund_status()
populate("frs")
check_fund_status()
depopulate("frs")
check_fund_status()

frs2 <- get_fund("frs")
check_fund_status()
ns(frs2)
frs2$beneficiaries
frs2$calculate_benefits()
depopulate("frs2")  # generates error as is not an internal fund



# test package ------------------------------------------------------------

devtools::test()





