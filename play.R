
source("data-raw/pkgtools.R")

library(penvir)

load_all()

data(package="penvir")

names(frs)
names(trs)

initialize_frs()
names(frs)
names(trs)

frs$calculate_benefits
frs$beneficiaries
