#.libPaths( c( "R/libs", .libPaths()) )

# Zero, cleanup of libs
unlink(list.dirs("renv/library", recursive=FALSE),recursive=TRUE)

# Install renv (if it is needed!)
options(renv.consent = TRUE)
if (!requireNamespace("remotes"))
  install.packages("remotes")

remotes::install_github("rstudio/renv")

renv::init(bare=TRUE, restart=TRUE)

packageList <- c("shiny", "shinyWidgets", "DT", "tidyverse", "ggplot2", "ggrepel",
	"cowplot", "pheatmap", "plotly", "bioc::progeny")

install.packages(packageList)

if (!requireNamespace("BiocManager", quietly = TRUE)) {
	install.packages("BiocManager")
}

# This line adding the Bioconductor repositories is needed,
# in order to have the URLs to their repos in the
# reproducible snapshot
options(repos=BiocManager::repositories())
# BiocManager::install(version = "3.11")

renv::snapshot(prompt=FALSE)

