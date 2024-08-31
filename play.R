
source("data-raw/pkgtools.R")

library(penvir)
usethis::use_roxygen_md()

load_all()
devtools::document()
devtools::build() # not needed, creates a tarball
devtools::install()
devtools::check()


data(package="penvir")

names(frs)
names(trs)

initialize_frs()
names(frs)
names(trs)

frs$calculate_benefits
frs$beneficiaries


# useful usethis functions
use_data() # save data to data/
