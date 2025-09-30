source("renv/activate.R")

if (requireNamespace("renv", quietly = TRUE)) {
  renv::activate()

  # automatically install missing packages
  pkgs <- renv::dependencies()$Package
  missing <- setdiff(pkgs, rownames(renv::installed_packages()))
  if (length(missing) > 0) {
    renv::install(missing)
    renv::snapshot(prompt = FALSE)
  }
}
