# In your R script, create an empty environment for the FRS pension fund
frs <- new.env()
trs <- new.env()

# Save these empty environments in the data/ directory
usethis::use_data(frs, trs, overwrite = TRUE)
