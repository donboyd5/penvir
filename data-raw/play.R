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


data(package = "penvir")

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
# fpath <- system.file("extdata", "my_data.csv", package = "penvir")
fpath <- system.file("extdata", "frs", "beneficiaries.rds", package = "penvir")
readRDS(fpath)

tpath <- system.file("extdata", "trs", "beneficiaries.rds", package = "penvir")
readRDS(tpath)


# it's ok to create subfolders
system.file("extdata", "subfolder", "datafile.rds", package = "yourpackage")

fpath <- system.file("extdata", "frs", "beneficiaries", package = "penvir")
readRDS(fpath)





# useful usethis functions
use_data() # save data to data/
usethis::use_directory("inst/extdata") # create a special folder for the pension data

usethis::use_package("tibble")
usethis::use_import_from("tibble", "tibble")



frs_copy <- frs
names(frs_copy)
frs_copy$beneficiaries

library(penvir)
