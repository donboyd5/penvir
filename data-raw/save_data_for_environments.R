

# frs data ----------------------------------------------------------------
fdir <- fs::path("inst", "extdata", "frs")
saveRDS(mtcars |>
          rename(benefits=mpg), fs::path(fdir, "beneficiaries.rds"))

# trs data ----------------------------------------------------------------

tdir <- fs::path("inst", "extdata", "trs")
saveRDS(iris |>
          rename(benefits=Sepal.Length), fs::path(tdir, "beneficiaries.rds"))


