#' @title Placebo Test for the STAR Dataset
#' @description This function performs a placebo test on the STAR dataset to check for the presence of a treatment effect.
#' @param data A data frame containing the STAR dataset.
#' @param n_permutations The number of permutations to perform for the placebo test.

# Install Prettier R package
install.packages("styler")

devtools::install_url("https://cran.r-project.org/src/contrib/Archive/gaoptim/gaoptim_1.1.tar.gz")

# Install lintr for linting R code
install.packages("lintr")

install.packages("usethis")


# Install IRkernel for Jupyter support
install.packages("IRkernel")

# Install styler for R code formatting (optional but recommended)
install.packages("styler")

# Make sure the IRkernel is available in Jupyter
IRkernel::installspec(user = FALSE)
