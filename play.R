
source("data-raw/pkgtools.R")

library(penvir)
usethis::use_roxygen_md()
use_mit_license(copyright_holder = NULL)
usethis::use_data_raw() # create data-raw/ and Rbuildignore it

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


# put environment data into inst/extdata
usethis::use_directory("inst/extdata") # create the inst/extdata directory

# create path to data if it exists (needs penvir installed)
fpath <- system.file("extdata", "my_data.csv", package = "penvir")

# it's ok to create subfolders
system.file("extdata", "subfolder", "datafile.rds", package = "yourpackage")





# useful usethis functions
use_data() # save data to data/
usethis::use_directory("inst/extdata") # create a special folder for the pension data
