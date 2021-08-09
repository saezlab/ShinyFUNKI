#.libPaths( c( "R/libs", .libPaths()) )

options(renv.consent = TRUE)

# Install renv (if it is needed!)
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

renv::restore(packages = c('BiocManager'), prompt = FALSE)
renv::restore(repos = BiocManager::repositories(), prompt = FALSE)
renv::isolate()
