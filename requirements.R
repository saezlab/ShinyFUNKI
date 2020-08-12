# First, install renv (if it is needed!)
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

renv::init()

# Bioconductor itself must be recorded
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

# This line adding the Bioconductor repositories is needed,
# in order to have the URLs to their repos in the
# reproducible snapshot
options(repos=structure(BiocManager::repositories()))

# At last, the snapshot is generated
renv::snapshot()