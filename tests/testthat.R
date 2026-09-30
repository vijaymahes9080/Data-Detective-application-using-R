library(testthat)

# Source all R engine scripts
r_files <- list.files("../R", pattern = "\\.R$", full.names = TRUE)
for (f in r_files) {
  source(f)
}

test_check("DataDetective")
