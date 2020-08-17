# First, install renv (if it is needed!)
#.libPaths( c( "R/libs", .libPaths()) )
options(renv.consent = TRUE)
if (!requireNamespace("renv", quietly = TRUE)) {
	install.packages("renv")
}
# La pescadilla que se muerde la cola
renv::restore(packages=c('BiocManager'),prompt=FALSE)
renv::restore(repos=BiocManager::repositories(), prompt=FALSE)
renv::isolate()
