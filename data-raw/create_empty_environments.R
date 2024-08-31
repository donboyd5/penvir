# In your R script, create an empty environment with no parent, for each pension fund
frs <- new.env(parent = emptyenv())
trs <- new.env(parent = emptyenv())

# Save these empty environments in the data/ directory
usethis::use_data(frs, trs, overwrite = TRUE)
